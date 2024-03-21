#' Pobierz i przygotuj dane dla RAP004_1
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP004_1. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP004_prepared_data_1 <- function(json_content) {
  #print(json_content)
  colnames(json_content) <- c("L_UMOW", "L_AKTYWNY", "L_ZAMKNIETY", "L_OMRP", "L_ZMRP", "L_PRZEDDEW", "L_ZADINW", "UMOWA_DEWELOPERSKA", "ROZW_U_DEWELOPERSKA", "NABYWCA")
  df_main <- as.data.frame(json_content)
  df_main[is.na(df_main)] <- 0
  df_main$L_UMOW <- format(as.numeric(df_main$L_UMOW), big.mark=" ")
  df_main$L_AKTYWNY <- format(as.numeric(df_main$L_AKTYWNY), big.mark=" ")
  df_main$L_ZAMKNIETY <- format(as.numeric(df_main$L_ZAMKNIETY), big.mark=" ")
  df_main$L_OMRP <- format(as.numeric(df_main$L_OMRP), big.mark=" ")
  df_main$L_ZMRP <- format(as.numeric(df_main$L_ZMRP), big.mark=" ")
  df_main$L_PRZEDDEW <- format(as.numeric(df_main$L_PRZEDDEW), big.mark=" ")
  df_main$L_ZADINW <- format(as.numeric(df_main$L_ZADINW), big.mark=" ")
  df_main$UMOWA_DEWELOPERSKA <- format(as.numeric(df_main$UMOWA_DEWELOPERSKA), big.mark=" ")
  df_main$ROZW_U_DEWELOPERSKA <- format(as.numeric(df_main$ROZW_U_DEWELOPERSKA), big.mark=" ")
  df_main$NABYWCA <- format(as.numeric(df_main$NABYWCA), big.mark=" ")
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP004_2
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP004_2. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP004_prepared_data_2 <- function(json_content) {

  colnames(json_content) <- c("L_WPLAT", "KWOTA_WPLAT", "L_WYPLAT_Z", 
                              "KWOTA_Z", "L_WYPLAT_SPOZA", "KWOTA_SPOZA", 
                              "L_ROZLICZONYCH", "KWOTA_ROZLICZONA", "L_NIEROZLICZONYCH", 
                              "KWOTA_NIEROZLICZONA", "L_ZALEGLYCH", "KWOTA_ZALEGLA")
  df_main <- as.data.frame(json_content)
  df_main[is.na(df_main)] <- 0
  df_main$L_WPLAT <- as.numeric(df_main$L_WPLAT)
  df_main$L_WPLAT[is.na(df_main$L_WPLAT)] <- 0
  df_main$KWOTA_WPLAT <- as.numeric(df_main$KWOTA_WPLAT)
  df_main$KWOTA_WPLAT[is.na(df_main$KWOTA_WPLAT)] <- 0
  df_main$L_WYPLAT_Z <- as.numeric(df_main$L_WYPLAT_Z)
  df_main$L_WYPLAT_Z[is.na(df_main$L_WYPLAT_Z)] <- 0
  df_main$KWOTA_Z <- as.numeric(df_main$KWOTA_Z)
  df_main$KWOTA_Z[is.na(df_main$KWOTA_Z)] <- 0
  df_main$L_WYPLAT_SPOZA <- as.numeric(df_main$L_WYPLAT_SPOZA)
  df_main$L_WYPLAT_SPOZA[is.na(df_main$L_WYPLAT_SPOZA)] <- 0
  df_main$KWOTA_SPOZA <- as.numeric(df_main$KWOTA_SPOZA)
  df_main$KWOTA_SPOZA[is.na(df_main$KWOTA_SPOZA)] <- 0
  df_main$L_ROZLICZONYCH <- as.numeric(df_main$L_ROZLICZONYCH)
  df_main$L_ROZLICZONYCH[is.na(df_main$L_ROZLICZONYCH)] <- 0
  df_main$L_NIEROZLICZONYCH <- as.numeric(df_main$L_NIEROZLICZONYCH)
  df_main$L_NIEROZLICZONYCH[is.na(df_main$L_NIEROZLICZONYCH)] <- 0
  df_main$L_ZALEGLYCH <- as.numeric(df_main$L_ZALEGLYCH)
  df_main$L_ZALEGLYCH[is.na(df_main$L_ZALEGLYCH)] <- 0
  df_main$KWOTA_ROZLICZONA <- as.numeric(df_main$KWOTA_ROZLICZONA)
  df_main$KWOTA_ROZLICZONAT[is.na(df_main$KWOTA_ROZLICZONA)] <- 0
  df_main$KWOTA_NIEROZLICZONA <- as.numeric(df_main$KWOTA_NIEROZLICZONA)
  df_main$KWOTA_NIEROZLICZONA[is.na(df_main$KWOTA_NIEROZLICZONA)] <- 0
  df_main$KWOTA_ZALEGLA <- as.numeric(df_main$KWOTA_ZALEGLA)
  df_main$KWOTA_ZALEGLA[is.na(df_main$KWOTA_ZALEGLA)] <- 0
  
  df_main$L_WPLAT <- format(as.numeric(df_main$L_WPLAT), big.mark=" ")
  df_main$KWOTA_WPLAT <- as.numeric(df_main$KWOTA_WPLAT)
  df_main$L_WYPLAT_Z <- format(as.numeric(df_main$L_WYPLAT_Z), big.mark=" ")
  df_main$KWOTA_Z <- as.numeric(df_main$KWOTA_Z)
  df_main$L_WYPLAT_SPOZA <- format(as.numeric(df_main$L_WYPLAT_SPOZA), big.mark=" ")
  df_main$KWOTA_SPOZA <- as.numeric(df_main$KWOTA_SPOZA)
  df_main$L_ROZLICZONYCH <- format(as.numeric(df_main$L_ROZLICZONYCH), big.mark=" ")
  df_main$L_NIEROZLICZONYCH <- format(as.numeric(df_main$L_NIEROZLICZONYCH), big.mark=" ")
  df_main$L_ZALEGLYCH <- format(as.numeric(df_main$L_ZALEGLYCH), big.mark=" ")
  df_main$KWOTA_ROZLICZONA <- as.numeric(df_main$KWOTA_ROZLICZONA)
  df_main$KWOTA_NIEROZLICZONA <- as.numeric(df_main$KWOTA_NIEROZLICZONA)
  df_main$KWOTA_ZALEGLA <- as.numeric(df_main$KWOTA_ZALEGLA)
  
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
  
  df_main$KWOTA_WPLAT <- sapply(df_main$KWOTA_WPLAT, convert_to_millions_or_billions_or_thousands)
  df_main$KWOTA_Z <- sapply(df_main$KWOTA_Z, convert_to_millions_or_billions_or_thousands)
  df_main$KWOTA_SPOZA <- sapply(df_main$KWOTA_SPOZA, convert_to_millions_or_billions_or_thousands)
  df_main$KWOTA_ROZLICZONA <- sapply(df_main$KWOTA_ROZLICZONA, convert_to_millions_or_billions_or_thousands)
  df_main$KWOTA_NIEROZLICZONA <- sapply(df_main$KWOTA_NIEROZLICZONA, convert_to_millions_or_billions_or_thousands)
  df_main$KWOTA_ZALEGLA <- sapply(df_main$KWOTA_ZALEGLA, convert_to_millions_or_billions_or_thousands)
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP004_3
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP004_3. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP004_prepared_data_3 <- function(json_content) {
  #print(json_content)
  df_main <- as.data.frame(json_content)
  colnames(df_main) <- c("L_KOMPLETNE", "L_BRAKDEWELOPER", "L_OCZEKUJEDEWELOPER")
  df_main$L_KOMPLETNE <- as.numeric(df_main$L_KOMPLETNE)
  df_main$L_BRAKDEWELOPER <- as.numeric(df_main$L_BRAKDEWELOPER)
  df_main$L_OCZEKUJEDEWELOPER <- as.numeric(df_main$L_OCZEKUJEDEWELOPER)
  df_main[is.na(df_main)] <- 0
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP004_4
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP004_4. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP004_prepared_data_4 <- function(json_content) {

  colnames(json_content) <- c("L_KOMPLETNE", "L_BRAKDEWELOPER", "L_BRAKBANK", "L_OCZEKUJEDEWELOPER", "L_OCZEKUJEBANK")
  df_main <- as.data.frame(json_content)
  df_main[is.na(df_main)] <- 0
  df_main$L_KOMPLETNE <- as.numeric(df_main$L_KOMPLETNE)
  df_main$L_BRAKDEWELOPER <- as.numeric(df_main$L_BRAKDEWELOPER)
  df_main$L_BRAKBANK <- as.numeric(df_main$L_BRAKBANK)
  df_main$L_OCZEKUJEDEWELOPER <- as.numeric(df_main$L_OCZEKUJEDEWELOPER)
  df_main$L_OCZEKUJEBANK <- as.numeric(df_main$L_OCZEKUJEBANK)
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP004_5
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP004_5. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP004_prepared_data_5 <- function(json_content) {

  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=3, nrow=1, dimnames=list(NULL, c("L_BRAKZASILENIA", "L_BRAKZWROTU", "L_WSTRZBANK"))))
      df_main[is.na(df_main)] <- 0
      
    } else {
      colnames(json_content) <- c("L_BRAKZASILENIA", "L_BRAKZWROTU", "L_WSTRZBANK")
      df_main <- as.data.frame(json_content)
      df_main$L_BRAKZASILENIA <- as.numeric(df_main$L_BRAKZASILENIA)
      df_main$L_BRAKZWROTU <- as.numeric(df_main$L_BRAKZWROTU)
      df_main$L_WSTRZBANK <- as.numeric(df_main$L_WSTRZBANK)
      df_main[is.na(df_main)] <- 0
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP004_6
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP004_6. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP004_prepared_data_6 <- function(json_content) {

  colnames(json_content) <- c("L_WPLMRP", "L_INSKLA", "L_DEAN", "L_WSTRZD", "WERSJA_WPLMRP", "WERSJA_INSKLA", "WERSJA_DEAN", "WERSJA_WSTRZD")
  df_main <- as.data.frame(json_content)
  df_main[is.na(df_main)] <- 0
  df_main$L_WPLMRP <- as.numeric(df_main$L_WPLMRP)
  df_main$L_INSKLA <- as.numeric(df_main$L_INSKLA)
  df_main$L_DEAN <- as.numeric(df_main$L_DEAN)
  df_main$L_WSTRZD <- as.numeric(df_main$L_WSTRZD)
  df_main$WERSJA_WPLMRP <- as.numeric(df_main$WERSJA_WPLMRP)
  df_main$WERSJA_INSKLA <- as.numeric(df_main$WERSJA_INSKLA)
  df_main$WERSJA_DEAN <- as.numeric(df_main$WERSJA_DEAN)
  df_main$WERSJA_WSTRZD <- as.numeric(df_main$WERSJA_WSTRZD)
  return(df_main)
}