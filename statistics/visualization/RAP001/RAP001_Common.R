#' Pobierz i przygotuj dane dla RAP001_1
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP001_1. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main_2 Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP001_prepared_data_1 <- function(json_content) {
  colnames(json_content) <- c("NR_DTR", "SUMA_WYMAGANYCH", "SUMA_AKTUALNYCH", "L_GLOWNYCH", "L_PRZEDAWNIONYCH",
                              "L_UMORZONYCH", "L_ANULOWANYCH", "L_ROZLICZONYCH", "DNI", "DOBROWOLNE", "SADOWE", "EGZEKUCYJNE")
  df_main_2 <- as.data.frame(json_content)
  df_main_2$NR_DTR <- as.numeric(df_main_2$NR_DTR)
  df_main_2$SUMA_WYMAGANYCH <- as.numeric(df_main_2$SUMA_WYMAGANYCH)
  df_main_2$SUMA_AKTUALNYCH <- as.numeric(df_main_2$SUMA_AKTUALNYCH)
  df_main_2$L_GLOWNYCH <- as.numeric(df_main_2$L_GLOWNYCH)
  df_main_2$L_GLOWNYCH[is.na(df_main_2$L_GLOWNYCH)] <- 0
  df_main_2$L_GLOWNYCH <- format(as.numeric(df_main_2$L_GLOWNYCH), big.mark=" ")
  df_main_2$L_PRZEDAWNIONYCH <- as.numeric(df_main_2$L_PRZEDAWNIONYCH)
  df_main_2$L_PRZEDAWNIONYCH[is.na(df_main_2$L_PRZEDAWNIONYCH)] <- 0
  df_main_2$L_PRZEDAWNIONYCH <- format(as.numeric(df_main_2$L_PRZEDAWNIONYCH), big.mark=" ")
  df_main_2$L_UMORZONYCH <- as.numeric(df_main_2$L_UMORZONYCH)
  df_main_2$L_UMORZONYCH[is.na(df_main_2$L_UMORZONYCH)] <- 0
  df_main_2$L_UMORZONYCH <- format(as.numeric(df_main_2$L_UMORZONYCH), big.mark=" ")
  df_main_2$L_ANULOWANYCH <- as.numeric(df_main_2$L_ANULOWANYCH)
  df_main_2$L_ANULOWANYCH[is.na(df_main_2$L_ANULOWANYCH)] <- 0
  df_main_2$L_ANULOWANYCH <- format(as.numeric(df_main_2$L_ANULOWANYCH), big.mark=" ")
  df_main_2$L_ROZLICZONYCH <- as.numeric(df_main_2$L_ROZLICZONYCH)
  df_main_2$L_ROZLICZONYCH[is.na(df_main_2$L_ROZLICZONYCH)] <- 0
  df_main_2$L_ROZLICZONYCH <- format(as.numeric(df_main_2$L_ROZLICZONYCH), big.mark=" ")
  df_main_2$DNI <- as.numeric(df_main_2$DNI)
  df_main_2$DNI[is.na(df_main_2$DNI)] <- 0
  df_main_2$DOBROWOLNE <- as.numeric(df_main_2$DOBROWOLNE)
  df_main_2$DOBROWOLNE[is.na(df_main_2$DOBROWOLNE)] <- 0
  df_main_2$DOBROWOLNE <- format(as.numeric(df_main_2$DOBROWOLNE), big.mark=" ")
  df_main_2$SADOWE <- as.numeric(df_main_2$SADOWE)
  df_main_2$SADOWE[is.na(df_main_2$SADOWE)] <- 0
  df_main_2$SADOWE <- format(as.numeric(df_main_2$SADOWE), big.mark=" ")
  df_main_2$EGZEKUCYJNE <- as.numeric(df_main_2$EGZEKUCYJNE)
  df_main_2$EGZEKUCYJNE[is.na(df_main_2$EGZEKUCYJNE)] <- 0
  df_main_2$EGZEKUCYJNE <- format(as.numeric(df_main_2$EGZEKUCYJNE), big.mark=" ")
  df_main_2$SUMA_WYMAGANYCH[is.na(df_main_2$SUMA_WYMAGANYCH)] <- 0
  df_main_2$SUMA_AKTUALNYCH[is.na(df_main_2$SUMA_AKTUALNYCH)] <- 0
  
  
  convert_to_millions_or_billions_or_thousands <- function(value) {
    if (value >= 1000000000) {
      amount <- formatC(value / 1000000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " mld")
    } else if (value >= 1000000) {
      amount <- formatC(value / 1000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " mln")
    } else if (value >= 1000) {
      amount <- formatC(value / 1000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " tys.")
    } else {
      amount <- formatC(value, digits = 0, format = "f", big.mark=" ")
      paste0(amount, "")
    }
  }
  df_main_2$SUMA_WYMAGANYCH <- sapply(df_main_2$SUMA_WYMAGANYCH, convert_to_millions_or_billions_or_thousands)
  df_main_2$SUMA_AKTUALNYCH <- sapply(df_main_2$SUMA_AKTUALNYCH, convert_to_millions_or_billions_or_thousands)
  df_main_2$DNI <- paste0(as.matrix(formatC(df_main_2$DNI, big.mark=" ")), ' dni')

return(df_main_2)
}


#' Pobierz i przygotuj dane dla RAP001_2
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP001_2. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP001_prepared_data_2 <- function(json_content) {
  
  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("WOJEWODZTWO", "SUMA_OGOLEM"))))
      df_main$WOJEWODZTWO <- as.character(df_main$WOJEWODZTWO)
      
    } else {
      
      colnames(json_content) <- c("WOJEWODZTWO", "SUMA_OGOLEM")
      df_main <- as.data.frame(json_content)
      df_main$WOJEWODZTWO[is.na(df_main$WOJEWODZTWO)] <- 'pozostałe'
      df_main$SUMA_OGOLEM <- as.numeric(df_main$SUMA_OGOLEM)
      
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP001_3
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP001_3. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP001_prepared_data_3 <- function(json_content) {
  colnames(json_content) <- c("ROK", "SUMA_OGOLEM")
  df_main <- as.data.frame(json_content)
  df_main$SUMA_OGOLEM <- as.numeric(df_main$SUMA_OGOLEM)
  return(df_main)
}