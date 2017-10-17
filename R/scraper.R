#' @importFrom magrittr "%>%"

#' @title Base url for votes
base_votes_url <- function() {
  # watch=36 is the site's link for votes
  "http://www.senado.gob.mx/index.php?watch=36"
}

#' @title Extract Date from Text
extract_short_date <- function(text_date) {
  text_date %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    toupper() %>%
    gsub("ENERO", "01", .) %>%
    gsub("FEBRERO", "02", .) %>%
    gsub("MARZO", "03", .) %>%
    gsub("ABRIL", "04", .) %>%
    gsub("MAYO", "05", .) %>%
    gsub("JUNIO", "06", .) %>%
    gsub("JULIO", "07", .) %>%
    gsub("AGOSTO", "08", .) %>%
    gsub("SEPTIEMBRE", "09", .) %>%
    gsub("OCTUBRE", "10", .) %>%
    gsub("NOVIEMBRE", "11", .) %>%
    gsub("DICIEMBRE", "12", .) %>%
    gsub("[A-Z]|[ ]+", "" , .) %>%
    as.Date(format = "%d%m%Y")
}


#' @title Will download the data as csv into the specified destination
votes_info <- function() {
  links <- base_votes_url() %>%
    xml2::read_html() %>%
    rvest::html_nodes(".tableA ul li a")
  links_text <- links %>%
    rvest::html_text()
  links_href <- links %>%
    rvest::html_attr("href")
  lg <- links_href %>%
    stringr::str_extract_all("lg=([^=&]*)") %>%
    unlist() %>%
    stringr::str_replace("lg=", "")
  tp <- links_href %>%
    stringr::str_extract_all("tp=([^=&]*)") %>%
    unlist() %>%
    stringr::str_replace("tp=", "")
  np <- links_href %>%
    stringr::str_extract_all("np=([^=&]*)") %>%
    unlist() %>%
    stringr::str_replace("np=", "")
  ano <- links_href %>%
    stringr::str_extract_all("ano=([^=&]*)") %>%
    unlist() %>%
    stringr::str_replace("ano=", "")
  num <- integer(0)
  for (l in unique(lg)) {
    for (a in unique(ano[lg == l])) {
      num <- c(num, 1:(sum(lg == l & ano == a)))
    }
  }
  id <- sprintf("L%s_A%s_N%s_P%s%s", lg, ano, num, tp, np)
  data.frame(
    legis = lg,
    ano = ano,
    numero = num,
    tipo = tp,
    periodo = np,
    # id = id,
    texto = links_text,
    href = links_href,
    stringsAsFactors = FALSE
  )
}

#' @title voting URL
votes_url <- function(legis, ano, periodo, tipo) {
  fields <- paste0(c("sm", "ano", "tp", "np", "lg"),
                   "=",
                   c(1, ano, tipo, periodo, legis))
  paste(c(base_votes_url(), fields), collapse = "&")
}

#' @title download a period
#' @export
fetch_session <- function(legis, ano, periodo, tipo, raw = FALSE) {
  link <- votes_url(legis, ano, periodo, tipo)
  sess <- link %>%
    rvest::html_session()
  a <- sess %>%
    rvest::html_nodes(".tableA.Pardo td div a")
  a_href <- a %>%
    rvest::html_attr("href")
  a_text <- a %>%
    rvest::html_text()
  votes_text <- a_text
  if (!raw) {
    votes_text <- votes_text %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      tolower()
  }
  votes_text <- votes_text %>%
    gsub("[\n\r\t]+", " ", .) %>%
    gsub('"|[ ]+', ' ', .) %>%
    gsub("[ ]+\\.", "\\.", .) %>%
    stringr::str_trim()
  period_votes <- data.frame()
  period_rollcalls <- data.frame()
  for (i in 1:length(a_href)) {
    this_vote_url <- rvest::html_session(paste0("http://www.senado.gob.mx/", a_href[i]))
    this_date <- this_vote_url %>%
      xml2::read_html() %>%
      rvest::html_nodes(".tableA.Pardo tr td div strong:first-child") %>%
      rvest::html_text() %>%
      extract_short_date()
    this_vote <- this_vote_url %>%
      rvest::follow_link("Ver Detalles") %>%
      xml2::read_html() %>%
      rvest::html_nodes(".tableA.Pardo") %>%
      rvest::html_table() %>%
      .[[1]] %>%
      .[ ,2:3] %>%
      `names<-`(c("senador", "voto")) %>%
      dplyr::mutate(voto = tolower(voto)) %>%
      dplyr::mutate(voto = replace(voto, grepl("ausente", voto), "ausente_oficial")) %>%
      dplyr::mutate(voto = replace(voto, grepl("abstención", voto), "abstencion")) %>%
      dplyr::mutate(voto = stringr::str_trim(voto)) %>%
      dplyr::mutate(senador = stringi::stri_trans_general(senador, "Latin-ASCII")) %>%
      dplyr::mutate(fecha = this_date) %>%
      dplyr::mutate(rollcall_id = paste0("L", legis, "_A", ano, "_T", tipo, "_P", periodo, "_N", i)) %>%
      dplyr::select(rollcall_id, fecha, senador, voto)
    this_vote_count <- factor(this_vote$voto, levels = c("pro", "contra", "abstencion", "ausente_oficial")) %>%
      table() %>%
      as.matrix() %>%
      t() %>%
      data.frame() %>%
      dplyr::mutate(texto = votes_text[i]) %>%
      dplyr::mutate(fecha = this_date) %>%
      dplyr::mutate(rollcall_id = paste0("L", legis, "_A", ano, "_T", tipo, "_P", periodo, "_N", i)) %>%
      dplyr::select(rollcall_id, fecha, texto, pro, contra, abstencion)
    period_rollcalls <- rbind(period_rollcalls, this_vote_count)
    period_votes <- rbind(period_votes, this_vote)
  }
  li <- list(
    votes = period_votes,
    rollcalls = period_rollcalls
  )
  class(li) <- "rollcall"
  li
}

#' @title Extract term (legislatura)
#' @export
fetch_legis <- function(legis, raw = FALSE) {
  info <- votes_info()
  info <- info[info$legis == legis, ]
  legis_votes <- data.frame()
  legis_rollcalls <- data.frame()
  for (i in 1:nrow(info)) {
    legis = info$legis[i]
    ano = info$ano[i]
    tipo = info$tipo[i]
    periodo = info$periodo[i]
    legis <- fetch_session(
      legis = legis,
      ano = ano,
      tipo = tipo,
      periodo = periodo,
      raw = raw
    )
    legis_votes <- rbind(legis_votes, data.frame(
      ano = ano,
      tipo = tipo,
      periodo = periodo,
      legis$votes
    ))
    legis_rollcalls <- rbind(legis_rollcalls, data.frame(
      ano = ano,
      tipo = tipo,
      periodo = periodo,
      legis$rollcalls
    ))
  }
  list(
    votes = legis_votes,
    rollcalls = legis_rollcalls
  )
}

#' @title Attendance links
#' @export
attendance_link_info <- function() {
  links <- read_html("http://www.senado.gob.mx/index.php?watch=35") %>%
    html_nodes(".tableA ul li a")
  links_text <- links %>%
    html_text()
  links_href <- links %>%
    html_attr("href")
  lg <- stringr::str_match(links_href, "lg=([^=&]*)")[ ,2]
  tp <- stringr::str_match(links_href, "tp=([^=&]*)")[ ,2]
  np <- stringr::str_match(links_href, "np=([^=&]*)")[ ,2]
  ano <- stringr::str_match(links_href, "ano=([^=&]*)")[ ,2]
  codigo <- sprintf("L%s_A%s_N%s_P%s_ASISTENCIA", lg, ano, tp, np)
  data.frame(
    legislatura = lg,
    ano = ano,
    tipo = tp,
    periodo = np,
    codigo = codigo,
    texto = links_text,
    href = links_href,
    stringsAsFactors = FALSE
  )
}


#' @title Attendence info
#' @export
attendance_base_url <- function() {
  # Start place to modify if whole webpage changes
  "http://www.senado.gob.mx/index.php?watch=35"
}


#' @title Senador info
#' @export
senador_base_url <- function() {
  # Start place to modify if whole webpage changes
  "http://www.senado.gob.mx/index.php?watch=8"
}



#' @title Attendence info
#' @export
fetch_attendance <- function(ano, tp, np, lg) {
  # construct_url
  end_url <- paste(c("ano", "tp", "np", "lg"), c(ano, tp, np, lg), sep = "=")
  full_url <- paste(c(attendance_base_url(), "sm=1", end_url), collapse = "&")
  a <- html_session(full_url) %>%
    html_nodes(".tableA.Pardo td div a")
  a_href <- a %>%
    html_attr("href")
  a_text <- a %>%
    html_text()
  fechas <- a_text %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    toupper() %>%
    gsub("ENERO", "01", .) %>%
    gsub("FEBRERO", "02", .) %>%
    gsub("MARZO", "03", .) %>%
    gsub("ABRIL", "04", .) %>%
    gsub("MAYO", "05", .) %>%
    gsub("JUNIO", "06", .) %>%
    gsub("JULIO", "07", .) %>%
    gsub("AGOSTO", "08", .) %>%
    gsub("SEPTIEMBRE", "09", .) %>%
    gsub("OCTUBRE", "10", .) %>%
    gsub("NOVIEMBRE", "11", .) %>%
    gsub("DICIEMBRE", "12", .) %>%
    gsub("[A-Z]|[ ]+|\\.", "" , .) %>%
    lubridate::dmy()
  asistencia_data_list <- lapply(seq_along(a), function(j) {
    # cat(sprintf("--| link %s: %s\n", j, a_text[j])) # debugging
    html <- read_html(paste0("http://www.senado.gob.mx/", a_href[j]))
    asistencia_data <- html %>%
      html_nodes(".tableA.Pardo") %>%
      html_table(fill = TRUE) %>%
      lapply(function(d) d[complete.cases(d), ]) %>%
      do.call("rbind", .) %>%
      `names<-`(c("SENADOR", "PARTIDO", "ASISTENCIA_INFO")) %>%
      dplyr::mutate(SENADOR = toupper(stringi::stri_trans_general(SENADOR, "Latin-ASCII"))) %>%
      dplyr::mutate(SENADOR = gsub("MA\\.", "MARIA", SENADOR)) %>%
      dplyr::mutate(ASISTENCIA_INFO = toupper(stringi::stri_trans_general(ASISTENCIA_INFO, "Latin-ASCII"))) %>%
      dplyr::mutate(ASISTENCIA = ASISTENCIA_INFO == "ASISTENCIA") %>%
      data.frame(FECHA = fechas[j], .)
    senadores <- asistencia_data$SENADOR
    id_href <- html %>%
      html_nodes(".tableA.Pardo tr td div a") %>%
      html_attr("href")
    asistencia_data <- asistencia_data %>%
      mutate(ID = stringr::str_match(id_href, "id=([^=&]*)")[ ,2])
    asistencia_data
  })
  asistencia <- do.call("rbind", asistencia_data_list)
  unique_senadores <- asistencia %>%
    dplyr::select(SENADOR, ID) %>%
    dplyr::distinct()
  suplente_list <- lapply(unique_senadores$ID, function(id) {
    html_session(paste0(senador_base_url(), "&id=", id)) %>%
      html_nodes(".tableInfo tr td span b") %>%
      html_text()
  })
  unique_senadores$suplente <- unlist(suplente_list) %>%
    gsub("[ ]+", " ", .) %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    toupper()
  asistencia <- asistencia %>%
    left_join(unique_senadores)
  asistencia
}


# 5. DESCARGAR LA INFORMACION DE TODOS LOS PERIODOS DISPONIBLES  ======================
# (PRIMERO DESCARGAR PORQUE HAY ERRORES EN LA PAGINA Y NO CONVIENE SCRAPEAR ONLINE)
# session <- html_session("http://www.senado.gob.mx/index.php?watch=35")
# for (i in 1:nrow(links_info)) {
#   texto <- links_info$texto[i]
#   codigo <- links_info$codigo[i]
#   session %>%
#     follow_link(texto) %>%
#     read_html() %>%
#     write_xml(paste0("source_html/", codigo, ".html"))
# }

#
# # 6. CONSTRUIR LA BASE DE DATOS DE VOTACIONES POR CADA PERIODO LEGISLATIVO =======
# for (i in 1:nrow(links_info)) {
#   # if (i < 5) next # debugging
#   texto <- links_info$texto[i]
#   codigo <- links_info$codigo[i]
#   # html <- read_html(paste0("source_html/", codigo, ".html"))
#   a <- html %>%
#     html_nodes(".tableA.Pardo td div a")
#   a_href <- a %>%
#     html_attr("href")
#   a_text <- a %>%
#     html_text()
#   fechas <- a_text %>%
#     stri_trans_general("Latin-ASCII") %>%
#     toupper() %>%
#     gsub("ENERO", "01", .) %>%
#     gsub("FEBRERO", "02", .) %>%
#     gsub("MARZO", "03", .) %>%
#     gsub("ABRIL", "04", .) %>%
#     gsub("MAYO", "05", .) %>%
#     gsub("JUNIO", "06", .) %>%
#     gsub("JULIO", "07", .) %>%
#     gsub("AGOSTO", "08", .) %>%
#     gsub("SEPTIEMBRE", "09", .) %>%
#     gsub("OCTUBRE", "10", .) %>%
#     gsub("NOVIEMBRE", "11", .) %>%
#     gsub("DICIEMBRE", "12", .) %>%
#     gsub("[A-Z]|[ ]+|\\.", "" , .) %>%
#     lubridate::dmy()
#   # cat(sprintf("Codigo %s: %s\n", i, codigo)) # debugging
#   asistencia_data_list <- lapply(seq_along(a), function(j) {
#     # cat(sprintf("--| link %s: %s\n", j, a_text[j])) # debugging
#     html <- read_html(paste0("http://www.senado.gob.mx/", a_href[j]))
#     asistencia_data <- html %>%
#       html_nodes(".tableA.Pardo") %>%
#       html_table(fill = TRUE) %>%
#       lapply(function(d) d[complete.cases(d), ]) %>%
#       do.call("rbind", .) %>%
#       `names<-`(c("SENADOR", "PARTIDO", "ASISTENCIA_INFO")) %>%
#       dplyr::mutate(SENADOR = toupper(stringi::stri_trans_general(SENADOR, "Latin-ASCII"))) %>%
#       dplyr::mutate(SENADOR = gsub("MA\\.", "MARIA", SENADOR)) %>%
#       dplyr::mutate(ASISTENCIA_INFO = toupper(stringi::stri_trans_general(ASISTENCIA_INFO, "Latin-ASCII"))) %>%
#       dplyr::mutate(ASISTENCIA = ASISTENCIA_INFO == "ASISTENCIA") %>%
#       data.frame(FECHA = fechas[j], .)
#     senadores <- asistencia_data$SENADOR
#     id_href <- html %>%
#       html_nodes(".tableA.Pardo tr td div a") %>%
#       html_attr("href")
#     asistencia_data <- asistencia_data %>%
#       mutate(ID = stringr::str_match(id_href, "id=([^=&]*)")[ ,2])
#     asistencia_data
#   })
#   asistencia <- do.call("rbind", asistencia_data_list)
#   unique_senadores <- asistencia %>%
#     dplyr::select(SENADOR, ID) %>%
#     dplyr::distinct()
#   for (i in seq_along(unique_senadores$ID)) {
#     id <- unique_senadores$ID[i]
#
#   }
#   asistencia
# }

# saveRDS(object = asistencia, file = paste0("source_rds/", codigo, "_ASISTENCIA", ".RDS"))
# lapply(function(d) na.omit(d))
# ensamble <- try({
#   asistencia_data <- asistencia_data %>%
#     do.call("rbind", .) %>%
#     `names<-`(c("SENADOR", "PARTIDO", "ASISTENCIA_INFO")) %>%
#     mutate(SENADOR = toupper(stri_trans_general(SENADOR, "Latin-ASCII"))) %>%
#     mutate(SENADOR = gsub("MA\\.", "MARIA", SENADOR)) %>%
#     mutate(ASISTENCIA = ASISTENCIA_INFO == "ASISTENCIA") %>%
#     data.frame(FECHA = fechas[j], .)
# }, silent = TRUE)
# if (inherits(ensamble, "try-error")) {
#   warning(sprintf("Error at (codigo: %s, link: %s), skipped...", codigo, a_text[j]))
#   return(data.frame())
# } else {
#   return(asistencia_data)
# }
# asistencia <- do.call("rbind", asistencia_data_list)
# saveRDS(object = asistencia, file = paste0("source_rds/", codigo, ".RDS"))
