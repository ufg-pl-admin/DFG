#' getStat_007_prepared_data
#'
#' @description
#' Funkcja `getStat_007_prepared_data` przygotowuje dane o liczbie umów deweloperskich w Polsce
#' w postaci obiektu `data.frame`.
#'
#' @usage
#' getStat_007_prepared_data(json_content)
#'
#' @param json_content Obiekt JSON zawierający dane o liczbie umów deweloperskich w Polsce.
#'
#' @return
#' Funkcja zwraca obiekt `data.frame`, w którym nazwy kolumn odpowiadają nazwom kolumn w pliku źródłowym.
#' Wszystkie kolumny liczbowe zostają skonwertowane do formatu numerycznego.
#'
#' @examples
#' data <- '{"MIESIAC": "2021-01", "WOJEWODZTWO": "MAZOWIECKIE", "AD1": "2900",
#' '"AD2": "20700", "AD3": "72"}'
#' json_content <- jsonlite::fromJSON(data)
#' getStat_002_prepared_data(json_content)
#'
#' @details
#' Funkcja nadaje kolumnom w obiekcie JSON nowe nazwy, odpowiadające nazwom kolumn w obiekcie `data.frame`,
#' który zostanie utworzony. Konwertuje obiekt JSON na `data.frame` przy użyciu funkcji `as.data.frame`.
#' Konwertuje trzy kolumny `AD1`, `AD2` oraz `AD3` do formatu numerycznego przy użyciu
#' funkcji `as.numeric`.

getStat_007_prepared_data_1 <- function(json_content) {
  colnames(json_content) <- c("AD1", "AD2", "AD3")
  df_main <- as.data.frame(json_content)
  df_main$AD1 <- as.numeric(df_main$AD1)
  df_main$AD2 <- as.numeric(df_main$AD2)
  df_main$AD3 <- as.numeric(df_main$AD3)
  
  df_main$AD1 <- format(as.numeric(df_main$AD1), big.mark=" ")
  df_main$AD2 <- format(as.numeric(df_main$AD2), big.mark=" ")
  df_main$AD3 <- format(as.numeric(df_main$AD3), big.mark=" ")
  
  return(df_main)
}

getStat_007_prepared_data_2 <- function(json_content) {
  # colnames(json_content) <- c("NR_ZADANIA", "Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")
  # df_main <- as.data.frame(json_content)
  # df_main_2 <- data.frame(t(df_main[-1]))
  # colnames(df_main_2) <- df_main[, 1] 
  # df_main_2 <- tibble::rownames_to_column(df_main_2, "MIESIAC")
  colnames(json_content) <- c("MIESIAC", "Ad4", "Ad5", "Ad6")
  df_main <- as.data.frame(json_content)
  df_main$Ad4 <- as.numeric(df_main$Ad4)
  df_main$Ad5<- as.numeric(df_main$Ad5)
  df_main$Ad6 <- as.numeric(df_main$Ad6)
  df_main$Ad4[is.na(df_main$Ad4)] <- 0
  df_main$Ad5[is.na(df_main$Ad5)] <- 0
  df_main$Ad6[is.na(df_main$Ad6)] <- 0
  
  
  return(df_main)
}

getStat_007_prepared_data_3 <- function(json_content) {
  colnames(json_content) <- c("LACZNA_WARTOSC", "SREDNIA_WARTOSC", "MEDIANA_WARTOSC")
  df_main <- as.data.frame(json_content)
  df_main$LACZNA_WARTOSC <- as.numeric(df_main$LACZNA_WARTOSC)
  df_main$SREDNIA_WARTOSC <- as.numeric(df_main$SREDNIA_WARTOSC)
  df_main$MEDIANA_WARTOSC <- as.numeric(df_main$MEDIANA_WARTOSC)
  
  convert_to_millions_or_billions <- function(value) {
    if (value >= 1000000000) {
      amount <- formatC(value / 1000000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " mld")
    } else if (value >= 1000000) {
      amount <- formatC(value / 1000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " mln")
    } else {
      amount <- formatC(value, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, "")
    }
  }
  
  df_main$LACZNA_WARTOSC <- sapply(df_main$LACZNA_WARTOSC, convert_to_millions_or_billions)
  
  convert_to_thousands <- function(value) {
    amount <- formatC(value / 1000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
    paste0(amount, " tys.")
  }
  
  df_main$SREDNIA_WARTOSC <- sapply(df_main$SREDNIA_WARTOSC, convert_to_thousands)
  df_main$MEDIANA_WARTOSC <- sapply(df_main$MEDIANA_WARTOSC, convert_to_thousands)
  
  return(df_main)
}