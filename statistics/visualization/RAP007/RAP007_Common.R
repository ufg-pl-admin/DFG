
#'
#' @param json_content Obiekt JSON zawierający dane o miesięcznych raportach z punktów kredytowych w Polsce.
#'
#' @return
#' Funkcja zwraca obiekt `data.frame`, w którym nazwy kolumn odpowiadają nazwom kolumn w pliku źródłowym.
#' Wszystkie kolumny liczbowe zostają skonwertowane do formatu numerycznego.
#'

getRAP007_prepared_data_1 <- function(json_content) {
  #print(json_content)
  colnames(json_content) <- c("L_NIEZGODNOSCI", "L_MRP", "L_DEV", "L_ROZ", "L_OBSLUGUJACYCH", "L_BANKOW", "L_DEWELOPEROW", "L_INEGRACJI", "SR_CZAS")
  df_main <- as.data.frame(json_content)
  df_main <- format(df_main, decimal.mark=".")
  df_main$L_NIEZGODNOSCI <- as.numeric(df_main$L_NIEZGODNOSCI)
  df_main$L_MRP <- as.numeric(df_main$L_MRP)
  df_main$L_DEV <- as.numeric(df_main$L_DEV)
  df_main$L_ROZ <- as.numeric(df_main$L_ROZ)
  df_main$L_OBSLUGUJACYCH <- as.numeric(df_main$L_OBSLUGUJACYCH)
  df_main$L_BANKOW <- as.numeric(df_main$L_BANKOW)
  df_main$L_DEWELOPEROW <- as.numeric(df_main$L_DEWELOPEROW)
  df_main$L_INEGRACJI <- as.numeric(df_main$L_INEGRACJI)
  df_main$L_MRP[is.na(df_main$L_MRP)] <- 0
  df_main$L_DEV[is.na(df_main$L_DEV)] <- 0
  df_main$L_ROZ[is.na(df_main$L_ROZ)] <- 0
  df_main$L_BANKOW[is.na(df_main$L_BANKOW)] <- 0
  df_main$L_DEWELOPEROW[is.na(df_main$L_DEWELOPEROW)] <- 0
  df_main$SR_CZAS[is.na(df_main$SR_CZAS)] <- 0
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP007_2 i RAP007_3
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP007_3. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRAP007_prepared_data_2 <- function(json_content) {
  colnames(json_content) <- c("L_WWER",
                              "L_ROZW",
                              "L_ODRZ",
                              "L_DOPR",
                              "L_DODOPR",
                              "L_REKL",
                              "L_ZAMK",
                              "L_AUTZAMK")
  df_main <- as.data.frame(json_content)
  df_main$L_WWER <- as.numeric(df_main$L_WWER)
  df_main$L_ROZW <- as.numeric(df_main$L_ROZW)
  df_main$L_ODRZ <- as.numeric(df_main$L_ODRZ)
  df_main$L_DOPR <- as.numeric(df_main$L_DOPR)
  df_main$L_DODOPR <- as.numeric(df_main$L_DODOPR)
  df_main$L_REKL <- as.numeric(df_main$L_REKL)
  df_main$L_ZAMK <- as.numeric(df_main$L_ZAMK)
  df_main$L_AUTZAMK <- as.numeric(df_main$L_AUTZAMK)
  df_main$L_WWER[is.na(df_main$L_WWER)] <- 0
  df_main$L_ROZW[is.na(df_main$L_ROZW)] <- 0
  df_main$L_ODRZ[is.na(df_main$L_ODRZ)] <- 0
  df_main$L_DOPR[is.na(df_main$L_DOPR)] <- 0
  df_main$L_DODOPR[is.na(df_main$L_DODOPR)] <- 0
  df_main$L_REKL[is.na(df_main$L_REKL)] <- 0
  df_main$L_ZAMK[is.na(df_main$L_ZAMK)] <- 0
  df_main$L_AUTZAMK[is.na(df_main$L_AUTZAMK)] <- 0
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP007_4
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP007_4. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRAP007_prepared_data_3 <- function(json_content) {
  colnames(json_content) <- c("L_WWER",
                              "L_ROZW",
                              "L_ODRZ",
                              "L_DOPR",
                              "L_DODOPR",
                              "L_REKL",
                              "L_ZAMK",
                              "L_AUTZAMK")
  df_main <- as.data.frame(json_content)
  df_main$L_WWER <- as.numeric(df_main$L_WWER)
  df_main$L_ROZW <- as.numeric(df_main$L_ROZW)
  df_main$L_ODRZ <- as.numeric(df_main$L_ODRZ)
  df_main$L_DOPR <- as.numeric(df_main$L_DOPR)
  df_main$L_DODOPR <- as.numeric(df_main$L_DODOPR)
  df_main$L_REKL <- as.numeric(df_main$L_REKL)
  df_main$L_ZAMK <- as.numeric(df_main$L_ZAMK)
  df_main$L_AUTZAMK <- as.numeric(df_main$L_AUTZAMK)
  df_main$L_WWER[is.na(df_main$L_WWER)] <- 0
  df_main$L_ROZW[is.na(df_main$L_ROZW)] <- 0
  df_main$L_ODRZ[is.na(df_main$L_ODRZ)] <- 0
  df_main$L_DOPR[is.na(df_main$L_DOPR)] <- 0
  df_main$L_DODOPR[is.na(df_main$L_DODOPR)] <- 0
  df_main$L_REKL[is.na(df_main$L_REKL)] <- 0
  df_main$L_ZAMK[is.na(df_main$L_ZAMK)] <- 0
  df_main$L_AUTZAMK[is.na(df_main$L_AUTZAMK)] <- 0
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP007_5
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP007_5. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRAP007_prepared_data_4 <- function(json_content) {
  colnames(json_content) <- c("L_WWER",
                              "L_ROZW",
                              "L_ODRZ",
                              "L_DOPR",
                              "L_DODOPR",
                              "L_REKL",
                              "L_ZAMK",
                              "L_AUTZAMK")
  df_main <- as.data.frame(json_content)
  df_main$L_WWER <- as.numeric(df_main$L_WWER)
  df_main$L_ROZW <- as.numeric(df_main$L_ROZW)
  df_main$L_ODRZ <- as.numeric(df_main$L_ODRZ)
  df_main$L_DOPR <- as.numeric(df_main$L_DOPR)
  df_main$L_DODOPR <- as.numeric(df_main$L_DODOPR)
  df_main$L_REKL <- as.numeric(df_main$L_REKL)
  df_main$L_ZAMK <- as.numeric(df_main$L_ZAMK)
  df_main$L_AUTZAMK <- as.numeric(df_main$L_AUTZAMK)
  df_main$L_WWER[is.na(df_main$L_WWER)] <- 0
  df_main$L_ROZW[is.na(df_main$L_ROZW)] <- 0
  df_main$L_ODRZ[is.na(df_main$L_ODRZ)] <- 0
  df_main$L_DOPR[is.na(df_main$L_DOPR)] <- 0
  df_main$L_DODOPR[is.na(df_main$L_DODOPR)] <- 0
  df_main$L_REKL[is.na(df_main$L_REKL)] <- 0
  df_main$L_ZAMK[is.na(df_main$L_ZAMK)] <- 0
  df_main$L_AUTZAMK[is.na(df_main$L_AUTZAMK)] <- 0
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP007_6
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP007_6. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRAP007_prepared_data_5 <- function(json_content) {
  colnames(json_content) <- c("L_WWER",
                              "L_ROZW",
                              "L_ODRZ",
                              "L_DOPR",
                              "L_DODOPR",
                              "L_REKL",
                              "L_ZAMK",
                              "L_AUTZAMK")
  df_main <- as.data.frame(json_content)
  df_main$L_WWER <- as.numeric(df_main$L_WWER)
  df_main$L_ROZW <- as.numeric(df_main$L_ROZW)
  df_main$L_ODRZ <- as.numeric(df_main$L_ODRZ)
  df_main$L_DOPR <- as.numeric(df_main$L_DOPR)
  df_main$L_DODOPR <- as.numeric(df_main$L_DODOPR)
  df_main$L_REKL <- as.numeric(df_main$L_REKL)
  df_main$L_ZAMK <- as.numeric(df_main$L_ZAMK)
  df_main$L_AUTZAMK <- as.numeric(df_main$L_AUTZAMK)
  df_main$L_WWER[is.na(df_main$L_WWER)] <- 0
  df_main$L_ROZW[is.na(df_main$L_ROZW)] <- 0
  df_main$L_ODRZ[is.na(df_main$L_ODRZ)] <- 0
  df_main$L_DOPR[is.na(df_main$L_DOPR)] <- 0
  df_main$L_DODOPR[is.na(df_main$L_DODOPR)] <- 0
  df_main$L_REKL[is.na(df_main$L_REKL)] <- 0
  df_main$L_ZAMK[is.na(df_main$L_ZAMK)] <- 0
  df_main$L_AUTZAMK[is.na(df_main$L_AUTZAMK)] <- 0
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP007_7
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP007_7. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRAP007_prepared_data_6 <- function(json_content) {

  createDataFrame <- function(json_content) {
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("MIESIAC", "L_NIEZGODNOSCI"))))
      df_main$MIESIAC <- as.character(df_main$MIESIAC)
    } else {
      colnames(json_content) <- c("MIESIAC", "L_NIEZGODNOSCI")
      df_main <- as.data.frame(json_content)
      df_main$L_NIEZGODNOSCI <- as.numeric(df_main$L_NIEZGODNOSCI)

    }
  
  return(df_main)
  }
  df_main <- createDataFrame(json_content)
  return(df_main)
}