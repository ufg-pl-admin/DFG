#' getStat_010_prepared_data
#'
#' @description
#' Funkcja `getStat_010_prepared_data` przygotowuje dane o dziesięciu miejscowościach pod względem największej liczby umów deweloperskich i inwestycji
#' w postaci obiektu `data.frame`.
#'
#' @usage
#' getStat_010_prepared_data(json_content)
#'
#' @param json_content Obiekt JSON zawierający dane o dziesięciu miejscowościach pod względem największej liczby umów deweloperskich i inwestycji.
#'
#' @return
#' Funkcja zwraca obiekt `data.frame`, w którym nazwy kolumn odpowiadają nazwom kolumn w pliku źródłowym.
#' Wszystkie kolumny liczbowe zostają skonwertowane do formatu numerycznego.
#'
#' @examples
#' data <- '{"MIEJSCOWOSC": "POZNAŃ", "L_CHRONIONYCH": "2900"}'
#' json_content <- jsonlite::fromJSON(data)
#' getStat_010_prepared_data(json_content)
#'
#' @details
#' Funkcja nadaje kolumnom w obiekcie JSON nowe nazwy, odpowiadające nazwom kolumn w obiekcie `data.frame`,
#' który zostanie utworzony. Konwertuje obiekt JSON na `data.frame` przy użyciu funkcji `as.data.frame`.
#' Konwertuje dwie kolumny `L_INWESTYCJI` oraz `L_UMOW` do formatu numerycznego przy użyciu
#' funkcji `as.numeric`.

getStat_010_prepared_data_1 <- function(json_content) {
  print(json_content)
  colnames(json_content) <- c("NAZWA_MIARY",	"L_CHRONIONYCH")
  df_main <- as.data.frame(json_content)
  df_main$L_CHRONIONYCH <- as.numeric(df_main$L_CHRONIONYCH)
  df_main$L_CHRONIONYCH <- format(as.numeric(df_main$L_CHRONIONYCH), big.mark=" ")
  return(df_main)
}

getStat_010_prepared_data_2 <- function(json_content) {
  print(json_content)
  colnames(json_content) <- c("MIEJSCOWOSC", "L_UMOW")
  df_main <- as.data.frame(json_content)
  df_main$L_UMOW <- as.numeric(df_main$L_UMOW)

  return(df_main)
}

getStat_010_prepared_data_3 <- function(json_content) {
  print(json_content)
  colnames(json_content) <- c("MIEJSCOWOSC", "L_UMOW")
  df_main <- as.data.frame(json_content)
  df_main$L_UMOW <- as.numeric(df_main$L_UMOW)

  return(df_main)
}