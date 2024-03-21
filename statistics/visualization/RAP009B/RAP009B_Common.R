#' Pobierz i przygotuj dane dla RAP009B_1
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP009B_1. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRap_009B_1_prepared_data <- function(json_content) {
  colnames(json_content) <- c("LICZBA_ANOMALII", "SR_CZAS_ANOMALII", "UDOSTEPNIONA", "ZAKONCZONA")
  df_main <- as.data.frame(json_content)
  df_main[is.na(df_main)] <- 0
  #df_main$SR_CZAS_ANOMALII [is.na(df_main$SR_CZAS_ANOMALII )] <- 0
  
  df_main$LICZBA_ANOMALII <- as.numeric(df_main$LICZBA_ANOMALII)
  df_main$UDOSTEPNIONA <- as.numeric(df_main$UDOSTEPNIONA)
  df_main$ZAKONCZONA <- as.numeric(df_main$ZAKONCZONA)

  
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP009B_2
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP009B_2. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRap_009B_2_prepared_data <- function(json_content) {

  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=16,nrow=0, dimnames=list(NULL, c("DATA_ANOMALII","ZAWMRP","ZAUMDE","ZMUMDE","WPLMRP","DANEAN","INSKLA","WYTMRP","WSTRZD","PRZEKS","ZAMMRP","ROUMDE","UPAPOS","WYPMRP","ZRWNAB","UPADEW"))))
      #df_main$NAZWA_REGULY <- as.character(df_main$NAZWA_REGULY)
      
    } else {
      
      colnames(json_content) <- c("DATA_ANOMALII","ZAWMRP","ZAUMDE","ZMUMDE","WPLMRP","DANEAN","INSKLA","WYTMRP","WSTRZD","PRZEKS","ZAMMRP","ROUMDE","UPAPOS","WYPMRP","ZRWNAB","UPADEW")
      df_main <- as.data.frame(json_content)
      df_main$ZAWMRP <- as.numeric(df_main$ZAWMRP)
      df_main$ZAUMDE <- as.numeric(df_main$ZAUMDE)
      df_main$ZMUMDE <- as.numeric(df_main$ZMUMDE)
      df_main$WPLMRP <- as.numeric(df_main$WPLMRP)
      df_main$DANEAN <- as.numeric(df_main$DANEAN)
      df_main$INSKLA <- as.numeric(df_main$INSKLA)
      df_main$WYTMRP <- as.numeric(df_main$WYTMRP)
      df_main$WSTRZD <- as.numeric(df_main$WSTRZD)
      df_main$PRZEKS <- as.numeric(df_main$PRZEKS)
      df_main$ZAMMRP <- as.numeric(df_main$ZAMMRP)
      df_main$ROUMDE <- as.numeric(df_main$ROUMDE)
      df_main$UPAPOS <- as.numeric(df_main$UPAPOS)
      df_main$WYPMRP <- as.numeric(df_main$WYPMRP)
      df_main$ZRWNAB <- as.numeric(df_main$ZRWNAB)  
      df_main$UPADEW <- as.numeric(df_main$UPADEW)
      
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP009B_3
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP009B_3. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRap_009B_3_prepared_data <- function(json_content) {
  
  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("STATUS_UDOSTEPNIENIA", "LICZBA_ANOMALII"))))
      df_main$STATUS_UDOSTEPNIENIA <- as.character(df_main$STATUS_UDOSTEPNIENIA)
      
    } else {
      
  colnames(json_content) <- c("STATUS_UDOSTEPNIENIA", "LICZBA_ANOMALII")
  df_main <- as.data.frame(json_content)
  df_main$LICZBA_ANOMALII <- as.numeric(df_main$LICZBA_ANOMALII)
 # df_main$STATUS_UDOSTEPNIENIA <- as.character(df_main$STATUS_UDOSTEPNIENIA)
  df_main[df_main == "null"] <- NA
  df_main$STATUS_UDOSTEPNIENIA[is.na(df_main$STATUS_UDOSTEPNIENIA)] <- "Pozostałe"
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP009B_4
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP009B_4. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRap_009B_4_prepared_data <- function(json_content) {

  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("KOD_ANOMALII", "LICZBA_ANOMALII"))))
      df_main$KOD_ANOMALII <- as.character(df_main$KOD_ANOMALII)
      
    } else {
      
      colnames(json_content) <- c("KOD_ANOMALII", "LICZBA_ANOMALII")
      df_main <- as.data.frame(json_content)
      df_main$LICZBA_ANOMALII <- as.numeric(df_main$LICZBA_ANOMALII)
      
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}
