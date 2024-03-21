#' getStat_008_prepared_data
#'
#' @description
#' Funkcja `getStat_008_prepared_data` przygotowuje dane o liczbie inwestycji i umów deweloperskich wg daty otwarcia
#' w postaci obiektu `data.frame`.
#'
#' @usage
#' getStat_008_prepared_data(json_content)
#'
#' @param json_content Obiekt JSON zawierający dane o liczbie inwestycji i umów deweloperskich wg daty otwarcia.
#'
#' @return
#' Funkcja zwraca obiekt `data.frame`, w którym nazwy kolumn odpowiadają nazwom kolumn w pliku źródłowym.
#' Wszystkie kolumny liczbowe zostają skonwertowane do formatu numerycznego.
#'
#' @examples
#' data <- '{"MIESIAC": "2021-01", "WOJEWODZTWO": "MAZOWIECKIE", "NM1": "2900",
#' '"NM2": "20700", "NM3": "72"}'
#' json_content <- jsonlite::fromJSON(data)
#' getStat_002_prepared_data(json_content)
#'
#' @details
#' Funkcja nadaje kolumnom w obiekcie JSON nowe nazwy, odpowiadające nazwom kolumn w obiekcie `data.frame`,
#' który zostanie utworzony. Konwertuje obiekt JSON na `data.frame` przy użyciu funkcji `as.data.frame`.
#' Konwertuje trzy kolumny `NM1`, `NM2` oraz `NM3` do formatu numerycznego przy użyciu
#' funkcji `as.numeric`.

getStat_008_prepared_data_1 <- function(json_content) {
  colnames(json_content) <- c("NM1", "NM2")
  df_main <- as.data.frame(json_content)
  df_main$NM1 <- format(as.numeric(df_main$NM1), big.mark=" ")
  df_main$NM2 <- format(as.numeric(df_main$NM2), big.mark=" ")
  return(df_main)
}

getStat_008_prepared_data_2 <- function(json_content) {
  colnames(json_content) <- c("NM3", "MIESIAC")
  json_content <- gsub("\\s", "", json_content)
  df_main <- as.data.frame(json_content)
  df_main$MIESIAC <- tolower(df_main$MIESIAC)
  df_main$NM3 <- as.numeric(df_main$NM3)
  df_main[df_main == "January"] <- "styczeń"
  df_main[df_main == "February"] <- "luty"
  df_main[df_main == "March"] <- "marzec"
  df_main[df_main == "April"] <- "kwiecień"
  df_main[df_main == "May"] <- "maj"
  df_main[df_main == "June"] <- "czerwiec"
  df_main[df_main == "July"] <- "lipiec"
  df_main[df_main == "August"] <- "sierpień"
  df_main[df_main == "September"] <- "wrzesień"
  df_main[df_main == "October"] <- "październik"
  df_main[df_main == "November"] <- "listopad"
  df_main[df_main == "December"] <- "grudzień"
  
    df <- df_main %>% group_by(MIESIAC) %>%
    mutate(KAT = case_when(
      str_detect(MIESIAC,'stycze.') ~ 1,
      str_detect(MIESIAC,'luty') ~ 2,
      str_detect(MIESIAC,'marzec') ~ 3,
      str_detect(MIESIAC,'kwiecie.') ~ 4,
      str_detect(MIESIAC,'maj') ~ 5,
      str_detect(MIESIAC,'czerwiec') ~ 6,
      str_detect(MIESIAC,'lipiec') ~ 7,
      str_detect(MIESIAC,'sierpie.') ~ 8,
      str_detect(MIESIAC,'wrzesie.') ~ 9,
      str_detect(MIESIAC,'pa.dziernik') ~ 10,
      str_detect(MIESIAC,'listopad') ~ 11,
      str_detect(MIESIAC,'grudzie.') ~ 12
    ))

  df_main <- df[order(df$KAT),]
  df$MIESIAC <- factor(df$MIESIAC,levels = df$MIESIAC[order(df$KAT)])

  df_main$MIESIAC <- tolower(df_main$MIESIAC)
  df_main$NM3[is.na(df_main$NM3)] <- 0

  return(df_main)
}

getStat_008_prepared_data_3 <- function(json_content) {
  colnames(json_content) <- c("NM4", "MIESIAC")
  json_content <- gsub("\\s", "", json_content)
  df_main <- as.data.frame(json_content)
  df_main$MIESIAC <- tolower(df_main$MIESIAC)
  df_main$NM4 <- as.numeric(df_main$NM4)
  df_main[df_main == "January"] <- "styczeń"
  df_main[df_main == "February"] <- "luty"
  df_main[df_main == "March"] <- "marzec"
  df_main[df_main == "April"] <- "kwiecień"
  df_main[df_main == "May"] <- "maj"
  df_main[df_main == "June"] <- "czerwiec"
  df_main[df_main == "July"] <- "lipiec"
  df_main[df_main == "August"] <- "sierpień"
  df_main[df_main == "September"] <- "wrzesień"
  df_main[df_main == "October"] <- "październik"
  df_main[df_main == "November"] <- "listopad"
  df_main[df_main == "December"] <- "grudzień"

    df <- df_main %>% group_by(MIESIAC) %>%
    mutate(KAT = case_when(
      str_detect(MIESIAC,'stycze.') ~ 1,
      str_detect(MIESIAC,'luty') ~ 2,
      str_detect(MIESIAC,'marzec') ~ 3,
      str_detect(MIESIAC,'kwiecie.') ~ 4,
      str_detect(MIESIAC,'maj') ~ 5,
      str_detect(MIESIAC,'czerwiec') ~ 6,
      str_detect(MIESIAC,'lipiec') ~ 7,
      str_detect(MIESIAC,'sierpie.') ~ 8,
      str_detect(MIESIAC,'wrzesie.') ~ 9,
      str_detect(MIESIAC,'pa.dziernik') ~ 10,
      str_detect(MIESIAC,'listopad') ~ 11,
      str_detect(MIESIAC,'grudzie.') ~ 12
    ))

  df_main <- df[order(df$KAT),]
  df$MIESIAC <- factor(df$MIESIAC,levels = df$MIESIAC[order(df$KAT)])

  df_main$MIESIAC <- tolower(df_main$MIESIAC)
  df_main$NM4[is.na(df_main$NM4)] <- 0

  return(df_main)
}