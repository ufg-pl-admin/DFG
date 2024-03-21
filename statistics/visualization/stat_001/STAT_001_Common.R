#' getStat_001_prepared_data
#'
#' @description
#' Funkcja `getStat_001_prepared_data` przygotowuje dane o miesięcznych raportach z punktów kredytowych w Polsce
#' w postaci obiektu `data.frame`.
#'
#' @usage
#' getStat_001_prepared_data(json_content)
#'
#' @param json_content Obiekt JSON zawierający dane o miesięcznych raportach z punktów kredytowych w Polsce.
#'
#' @return
#' Funkcja zwraca obiekt `data.frame`, w którym nazwy kolumn odpowiadają nazwom kolumn w pliku źródłowym.
#' Wszystkie kolumny liczbowe zostają skonwertowane do formatu numerycznego.
#'
#' @examples
#' data <- '{"RODZAJ_MRP": "POS", "MIESIAC_OTWARCIA": "2021-01", "WOJEWODZTWO": "MAZOWIECKIE", "LICZBA_MRP": "2900",
#' '"LICZBA_UMOW_DEW": "20700", "LICZBA_BANKOW": "72"}'
#' json_content <- jsonlite::fromJSON(data)
#' getStat_001_prepared_data(json_content)
#'
#' @details
#' Funkcja nadaje kolumnom w obiekcie JSON nowe nazwy, odpowiadające nazwom kolumn w obiekcie `data.frame`,
#' który zostanie utworzony. Konwertuje obiekt JSON na `data.frame` przy użyciu funkcji `as.data.frame`.
#' Konwertuje trzy kolumny `LICZBA_MRP`, `LICZBA_UMOW` oraz `LICZBA_BANKOW` do formatu numerycznego przy użyciu
#' funkcji `as.numeric`.
getStat_001_prepared_data <- function(json_content) {
    colnames(json_content) <- c("WOJEWODZTWO", "LICZBA_BANKOW")
    df_main <- as.data.frame(json_content)
    df_main$LICZBA_BANKOW <- as.numeric(df_main$LICZBA_BANKOW)
    return(df_main)
}


getStat_001_prepared_data_1 <- function(json_content) {
  colnames(json_content) <- c("LICZBA_BANKOW","LICZBA_MRP", "LICZBA_UMOW_DEW")
  df_main <- as.data.frame(json_content)
  df_main$LICZBA_MRP <- as.numeric(df_main$LICZBA_MRP)
  df_main$LICZBA_UMOW_DEW <- as.numeric(df_main$LICZBA_UMOW_DEW)
  df_main$LICZBA_BANKOW <- as.numeric(df_main$LICZBA_BANKOW)
  return(df_main)
}


getStat_001_prepared_data_2 <- function(json_content) {
  colnames(json_content) <- c("OMRP_ZMRP","OMRP","ZMRP","SUMA")
  df_main <- as.data.frame(json_content)
  df_main$OMRP_ZMRP <- as.numeric(df_main$OMRP_ZMRP)
  df_main$OMRP <- as.numeric(df_main$OMRP)
  df_main$ZMRP <- as.numeric(df_main$ZMRP)
  df_main$SUMA <- as.numeric(df_main$SUMA)
  return(df_main)
}


getStat_001_prepared_data_3 <- function(json_content) {
  colnames(json_content) <- c("MIESIAC_OTWARCIA", "BANKI")
  df_main <- as.data.frame(json_content)
  df_main$BANKI <- as.numeric(df_main$BANKI)
  return(df_main)
}