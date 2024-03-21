#' getStat_003_prepared_data
#'
#' @description
#' Funkcja `getStat_003_prepared_data` przygotowuje dane o liczbie założonych mieszkaniowych rachunków powierniczych w Polsce
#' w postaci obiektu `data.frame`.
#'
#' @usage
#' getStat_003_prepared_data(json_content)
#'
#' @param json_content Obiekt JSON zawierający dane o liczbie założonych mieszkaniowych rachunków powierniczych w Polsce.
#'
#' @return
#' Funkcja zwraca obiekt `data.frame`, w którym nazwy kolumn odpowiadają nazwom kolumn w pliku źródłowym.
#' Wszystkie kolumny liczbowe zostają skonwertowane do formatu numerycznego.
#'
#' @examples
#' data <- '{"RODZAJ_MRP": "POS", "MIESIAC": "2021-01", "WOJEWODZTWO": "MAZOWIECKIE", "L_MRP": "2900",
#' '"L_OTWARTYCH": "20700", "L_ZAMKNIETYCH": "72"}'
#' json_content <- jsonlite::fromJSON(data)
#' getStat_003_prepared_data(json_content)
#'
#' @details
#' Funkcja nadaje kolumnom w obiekcie JSON nowe nazwy, odpowiadające nazwom kolumn w obiekcie `data.frame`,
#' który zostanie utworzony. Konwertuje obiekt JSON na `data.frame` przy użyciu funkcji `as.data.frame`.
#' Konwertuje trzy kolumny `L_MRP`, `L_OTWARTYCH` oraz `L_ZAMKNIETYCH` do formatu numerycznego przy użyciu
#' funkcji `as.numeric`.

getStat_003_prepared_data_1 <- function(json_content) {
  colnames(json_content) <- c("L_MRP", "L_OTWARTYCH","L_ZAMKNIETYCH")
  df_main <- as.data.frame(json_content)
  # df_main$L_MRP <- as.numeric(df_main$L_MRP)
  # df_main$L_OTWARTYCH <- as.numeric(df_main$L_OTWARTYCH)
  # df_main$L_ZAMKNIETYCH <- as.numeric(df_main$L_ZAMKNIETYCH)
  df_main$L_MRP <- format(as.numeric(df_main$L_MRP), big.mark=" ")
  df_main$L_OTWARTYCH <- format(as.numeric(df_main$L_OTWARTYCH), big.mark=" ")
  df_main$L_ZAMKNIETYCH <- format(as.numeric(df_main$L_ZAMKNIETYCH), big.mark=" ")
  return(df_main)
}


getStat_003_prepared_data_2 <- function(json_content) {
  colnames(json_content) <- c("L_MRP","WOJEWODZTWO")
  df_main <- as.data.frame(json_content)
  df_main <- df_main[complete.cases(df_main$WOJEWODZTWO), ]
  df_main$L_MRP <- as.numeric(df_main$L_MRP)

  return(df_main)
}


getStat_003_prepared_data_3 <- function(json_content) {
  colnames(json_content) <- c("L_MRP", "MIESIAC")
  json_content <- gsub("\\s", "", json_content)
  df_main <- as.data.frame(json_content)
  df_main$MIESIAC <- tolower(df_main$MIESIAC)
  df_main$L_MRP <- as.numeric(df_main$L_MRP)
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
  df_main$L_MRP[is.na(df_main$L_MRP)] <- 0
  return(df_main)
}

getStat_003_prepared_data_4 <- function(json_content) {
  source(paste(getwd(), '/utils/provinceUtil.R', sep=""))

  createDataFrame <- function(json_content) {
    if (length(json_content) == 0) {
      print(1)
      df_main <- data.frame(matrix(ncol=2, nrow=0, dimnames=list(NULL, c("L_MRP", "WOJEWODZTWO"))))
      print(2)
      df_main$WOJEWODZTWO <- as.character(df_main$BANK)
    } else {
      colnames(json_content) <- c("L_MRP","WOJEWODZTWO")
      df_main <- as.data.frame(json_content)
      df_main$L_MRP <- as.numeric(df_main$L_MRP)
    }

    return(df_main)
  }

  df_main <- createDataFrame(json_content)
  # incorrectedWords <- df_main$WOJEWODZTWO
  # correctedWords <- getCorrectedWords(incorrectedWords)
  # correctedProvinces <- unlist(correctedWords)
  # df_main$WOJEWODZTWO <-  correctedProvinces
  # 
  # df_main <- setNames(aggregate(df_main$L_MRP, by = list(df_main$WOJEWODZTWO), FUN = sum), c("WOJEWODZTWO","L_MRP"))
  return(df_main)
}