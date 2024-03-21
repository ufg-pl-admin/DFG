#' Pobierz i przygotuj dane dla RAP005BDS_5
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP005BDS_5. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
 
getRap_005_1_prepared_data <- function(json_content) {
  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("DATA_WATKU","LICZBA_WATKOW","LICZBA_ODPOWIEDZI"))))
      df_main$DATA_WATKU <- as.character(df_main$DATA_WATKU)
      
    } else {
      colnames(json_content) <- c("DATA_WATKU","LICZBA_WATKOW","LICZBA_ODPOWIEDZI")
      df_main <- as.data.frame(json_content)
      df_main$LICZBA_WATKOW <- as.numeric(df_main$LICZBA_WATKOW)
      df_main$LICZBA_ODPOWIEDZI <- as.numeric(df_main$LICZBA_ODPOWIEDZI)
      print(df_main)
    }

    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP005BDS_3
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP005BDS_3. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRap_005_2_prepared_data <- function(json_content) {
  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("STATUS_WIADOMOSCI","KATEGORIA","LICZBA_WATKOW","LICZBA_ODPOWIEDZI"))))
      df_main$STATUS_WIADOMOSCI <- as.character(df_main$STATUS_WIADOMOSCI)
      df_main$KATEGORIA <- as.character(df_main$KATEGORIA)
    } else {
  colnames(json_content) <- c("STATUS_WIADOMOSCI","KATEGORIA","LICZBA_WATKOW","LICZBA_ODPOWIEDZI")
  df_main <- as.data.frame(json_content)
  df_main$LICZBA_WATKOW <- as.numeric(df_main$LICZBA_WATKOW)
  df_main$LICZBA_ODPOWIEDZI <- as.numeric(df_main$LICZBA_ODPOWIEDZI)
    }
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP005BDS_1 i RAP005BDS_2
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP005BDS_1 i RAP005BDS_2. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRap_005_3_prepared_data <- function(json_content) {
  colnames(json_content) <- c("NIEZGODNOSC_DANYCH",
                              "ROZLICZENIE_SKLADEK",
                              "PROBLEMY_TECH",
                              "INNE",
                              "ZOBOWIAZANIA_DFG",
                              "ANOMALIE",
                              "WSZYSTKIE_WATKI")
  df_main <- as.data.frame(json_content)
  df_main$NIEZGODNOSC_DANYCH <- as.numeric(df_main$NIEZGODNOSC_DANYCH)
  df_main$ROZLICZENIE_SKLADEK <- as.numeric(df_main$ROZLICZENIE_SKLADEK)
  df_main$PROBLEMY_TECH <- as.numeric(df_main$PROBLEMY_TECH)
  df_main$INNE <- as.numeric(df_main$INNE)
  df_main$ZOBOWIAZANIA_DFG <- as.numeric(df_main$ZOBOWIAZANIA_DFG)
  df_main$ANOMALIE <- as.numeric(df_main$ANOMALIE)
  df_main$WSZYSTKIE_WATKI <- as.numeric(df_main$WSZYSTKIE_WATKI)
  


  return(df_main)
}



#' Pobierz i przygotuj dane dla RAP005BDS_4
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP005BDS_4. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRap_005_4_prepared_data <- function(json_content) {
  
  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("KATEGORIA","LICZBA_WATKOW","LICZBA_ODPOWIEDZI"))))
      df_main$KATEGORIA <- as.character(df_main$KATEGORIA)
      
    } else {
  colnames(json_content) <- c("KATEGORIA","LICZBA_WATKOW","LICZBA_ODPOWIEDZI")
  df_main <- as.data.frame(json_content)
  df_main$LICZBA_WATKOW <- as.numeric(df_main$LICZBA_WATKOW)
  df_main$LICZBA_ODPOWIEDZI <- as.numeric(df_main$LICZBA_ODPOWIEDZI)
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  return(df_main)
}

