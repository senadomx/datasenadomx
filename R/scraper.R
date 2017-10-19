#' @importFrom magrittr "%>%"

# Webpage codes
base_url <- function(type) {
  switch(type,
    "prefix" = "http://www.senado.gob.mx/",
    "votes" = "http://www.senado.gob.mx/index.php?watch=36",
    "attendance" = "http://www.senado.gob.mx/index.php?watch=35",
    "senadores" = "http://www.senado.gob.mx/index.php?watch=8",
    stop("wrong type for base_url")
  )
}

# Parse spanish text data into date format (e.g., jueves 23 de marzo de 2015)
parse_date <- function(text_date) {
  # constant for months in spanish
  spanish_months <- c("enero", "febrero", "marzo", "abril",
                      "mayo", "junio", "julio", "agosto",
                      "septiembre", "octubre", "noviembre", "diciembre")
  # parse text into data object
  text_date %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    stringr::str_replace_all(setNames(sprintf("%02d", 1:12), spanish_months)) %>%
    stringr::str_replace_all("[a-z]|[ ]+", "") %>%
    as.Date(format = "%d%m%Y")
}

#' @title Voting information
#' @description Creates a data.frame with information about available voting information and web source.
#' @details The selected names for the output variables are chosen in accordance with the html codes of the web source.
#' @return A data.frame with the following information
#' \itemize{
#'  \item{\code{ano}}{ Year within term e.g., 'primer ano de ejercicio'}
#'  \item{\code{tp}}{  Type of session 'ordinario or extraodinario'}
#'  \item{\code{np}}{  Period within year e.g., 'segundo periodo'}
#'  \item{\code{lg}}{  Legislature e.g., XVIII}
#'  \item{\code{id}}{  Compact id including the previous variables}
#'  \item{\code{description}}{ Original spanish description from web source}
#'  \item{\code{link}}{ Link to the web source}
#' }
#' @export
voting_info <- function() {
  message("Finding all available periods")
  period <- base_url("votes") %>%
    xml2::read_html() %>%
    rvest::html_nodes(".tableA ul li a")
  period_description <- period %>%
    rvest::html_text()
  period_link <- period %>%
    rvest::html_attr("href")
  lg <- stringr::str_match(period_link, "lg=([^=&]*)")[ ,2]
  tp <- stringr::str_match(period_link, "tp=([^=&]*)")[ ,2]
  np <- stringr::str_match(period_link, "np=([^=&]*)")[ ,2]
  ano <- stringr::str_match(period_link, "ano=([^=&]*)")[ ,2]
  message("Fetching info of each period")
  pb <- progress::progress_bar$new(total = length(period_link), clear = FALSE)
  result <- purrr::map(1:length(period_link), function(i) {
    this_html <- paste0(base_url("prefix"), period_link[i]) %>%
      rvest::html_session()
    tmp_txt <- this_html %>%
      xml2::read_html() %>%
      rvest::html_nodes(".tableA.Pardo tr th") %>%
      rvest::html_text()
    tmp_date <- this_html %>%
      xml2::read_html() %>%
      rvest::html_nodes(".tableA.Pardo tr th div") %>%
      rvest::html_text() %>%
      parse_date()
    this_date <- as.Date(as.character( ))
    k <- 1
    for (l in 2:length(tmp_txt)) {
      if (grepl("Resultado", tmp_txt[l])) {
        this_date <- c(this_date, tmp_date[k])
      } else {
        k <-  k + 1
      }
    }
    this_description <- this_html %>%
      rvest::html_nodes(".tableA.Pardo tr td div a") %>%
      rvest::html_text() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      tolower()
    this_link <- this_html %>%
      rvest::html_nodes(".tableA.Pardo tr td div a") %>%
      rvest::html_attr("href")
    this_vote <- this_html %>%
      rvest::html_nodes(".tableV") %>%
      rvest::html_table() %>%
      purrr::map(. %>% .[ ,2]) %>%
      purrr::reduce(rbind) %>%
      `rownames<-`(NULL) %>%
      data.frame() %>%
      `names<-`(c("Yea", "Nay", "Abstain"))
    pb$tick()
    id <- paste(lg[i], ano[i], np[i], tp[i], sprintf("%002d", 1:length(this_date)), sep = "-")
    data.frame(
      ano = ano[i],
      tp = tp[i],
      np = np[i],
      lg = lg[i],
      date = this_date,
      id = id,
      description = this_description,
      link = this_link,
      stringsAsFactors = FALSE
    )
  })

  # row bind list
  result  <- purrr::reduce(result, rbind)

  # output
  result
}





#' @title voting URL
votes_url <- function(legis, ano, periodo, tipo) {
  fields <- paste(
    c("sm", "ano", "tp", "np", "lg"),
    c(1, ano, tipo, periodo, legis),
    sep = "="
  )
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
