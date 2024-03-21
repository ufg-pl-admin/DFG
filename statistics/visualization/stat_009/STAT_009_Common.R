#' getStat_009_prepared_data
#'
#' @description
#' Funkcja `getStat_009_prepared_data` przygotowuje dane o liczbie inwestycji i umów deweloperskich w podziale na województwa
#' w postaci obiektu `data.frame`.
#'
#' @usage
#' getStat_009_prepared_data(json_content)
#'
#' @param json_content Obiekt JSON zawierający dane o liczbie inwestycji i umów deweloperskich w podziale na województwa.
#'
#' @return
#' Funkcja zwraca obiekt `data.frame`, w którym nazwy kolumn odpowiadają nazwom kolumn w pliku źródłowym.
#' Wszystkie kolumny liczbowe zostają skonwertowane do formatu numerycznego.
#'
#' @examples
#' data <- '{"RODZAJ_MRP": "POS", "MIESIAC": "2021-01", "WOJEWODZTWO": "MAZOWIECKIE", "L_INWESTYCJI": "2900",
#' '"L_UMOW": "20700"}'
#' json_content <- jsonlite::fromJSON(data)
#' getStat_009_prepared_data(json_content)
#'
#' @details
#' Funkcja nadaje kolumnom w obiekcie JSON nowe nazwy, odpowiadające nazwom kolumn w obiekcie `data.frame`,
#' który zostanie utworzony. Konwertuje obiekt JSON na `data.frame` przy użyciu funkcji `as.data.frame`.
#' Konwertuje dwie kolumny `L_INWESTYCJI` oraz `L_UMOW` do formatu numerycznego przy użyciu
#' funkcji `as.numeric`.

getStat_009_prepared_data_1 <- function(json_content) {
  source(paste(getwd(), '/utils/provinceUtil.R', sep=""))
   colnames(json_content) <- c("WOJEWODZTWO", "L_INWESTYCJI","L_UMOW")
   df_main <- as.data.frame(json_content)
   df_main <- df_main[complete.cases(df_main$WOJEWODZTWO), ]
  df_main$L_INWESTYCJI <- as.numeric(df_main$L_INWESTYCJI)
  df_main$L_UMOW <- as.numeric(df_main$L_UMOW)
  df_main$L_INWESTYCJI[is.na(df_main$L_INWESTYCJI)] <- 0
  df_main$L_UMOW[is.na(df_main$L_UMOW)] <- 0 
      incorrectedWords <- df_main$WOJEWODZTWO
      correctedWords <- getCorrectedWords(incorrectedWords)
      correctedProvinces <- unlist(correctedWords)
      df_main$WOJEWODZTWO <-  correctedProvinces
      
      df_main <- setNames(aggregate(cbind(L_INWESTYCJI, L_UMOW) ~ WOJEWODZTWO, data = df_main, FUN = sum), c("WOJEWODZTWO", "L_INWESTYCJI", "L_UMOW"))


  
  return(df_main)
}

getStat_009_prepared_data_2 <- function(json_content) {
  source(paste(getwd(), '/utils/provinceUtil.R', sep=""))
  colnames(json_content) <- c("WOJEWODZTWO", "L_INWESTYCJI")
  df_main <- as.data.frame(json_content)
  df_main <- df_main[complete.cases(df_main$WOJEWODZTWO), ]
  df_main$L_INWESTYCJI <- as.numeric(df_main$L_INWESTYCJI)
  df_main$L_INWESTYCJI[is.na(df_main$L_INWESTYCJI)] <- 0
    
  incorrectedWords <- df_main$WOJEWODZTWO
  correctedWords <- getCorrectedWords(incorrectedWords)
  correctedProvinces <- unlist(correctedWords)
  df_main$WOJEWODZTWO <-  correctedProvinces
  
  df_main <- setNames(aggregate(df_main$L_INWESTYCJI, by = list(df_main$WOJEWODZTWO), FUN = sum), c("WOJEWODZTWO","L_INWESTYCJI"))

  return(df_main)
}

getStat_009_prepared_data_3 <- function(json_content) {
  source(paste(getwd(), '/utils/provinceUtil.R', sep=""))
  colnames(json_content) <- c("WOJEWODZTWO","L_UMOW")
  df_main <- as.data.frame(json_content)
  df_main <- df_main[complete.cases(df_main$WOJEWODZTWO), ]
  df_main$L_UMOW <- as.numeric(df_main$L_UMOW)
  df_main$L_UMOW[is.na(df_main$L_UMOW)] <- 0
  
  incorrectedWords <- df_main$WOJEWODZTWO
  correctedWords <- getCorrectedWords(incorrectedWords)
  correctedProvinces <- unlist(correctedWords)
  df_main$WOJEWODZTWO <-  correctedProvinces
  
  df_main <- setNames(aggregate(df_main$L_UMOW, by = list(df_main$WOJEWODZTWO), FUN = sum), c("WOJEWODZTWO","L_UMOW"))

  return(df_main)
}