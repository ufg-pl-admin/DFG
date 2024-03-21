#' Pobierz i przygotuj dane dla RAP002_1
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP002_1. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP002_prepared_data_1 <- function(json_content) {

  colnames(json_content) <- c("L_URUCHOMIEN", "L_ZASILEN")
  df_main <- as.data.frame(json_content)
  df_main$L_URUCHOMIEN <- as.numeric(df_main$L_URUCHOMIEN)
  #df_main$SR_URUCHOMIEN <- as.numeric(df_main$SR_URUCHOMIEN)
  df_main$L_ZASILEN <- as.numeric(df_main$L_ZASILEN)
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP002_2
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP002_2. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP002_prepared_data_2 <- function(json_content) {

  colnames(json_content) <- c("L_SKORYGOWANYCH", "P_SKORYGOWANYCH")
  df_main <- as.data.frame(json_content)
  df_main$L_SKORYGOWANYCH[is.na(df_main$L_SKORYGOWANYCH)] <- 0
  df_main$P_SKORYGOWANYCH [is.na(df_main$P_SKORYGOWANYCH )] <- 0
  df_main$L_SKORYGOWANYCH <- as.numeric(df_main$L_SKORYGOWANYCH)
  df_main$P_SKORYGOWANYCH <- as.numeric(df_main$P_SKORYGOWANYCH)
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP002_3
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP002_3. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP002_prepared_data_3 <- function(json_content) {

  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("TYP_ZASILENIA", "LICZBA_OSTRZEZEN"))))
      df_main$TYP_ZASILENIA <- as.character(df_main$TYP_ZASILENIA)
      
    } else {
      
      colnames(json_content) <- c("TYP_ZASILENIA", "LICZBA_OSTRZEZEN")
      df_main <- as.data.frame(json_content)
      df_main$LICZBA_OSTRZEZEN <- as.numeric(df_main$LICZBA_OSTRZEZEN)
      
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP002_4
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP002_4. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP002_prepared_data_4 <- function(json_content) {

  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("TYP_ZASILENIA", "LICZBA_BLADOW"))))
      df_main$TYP_ZASILENIA <- as.character(df_main$TYP_ZASILENIA)
      
    } else {
      
      colnames(json_content) <- c("TYP_ZASILENIA", "LICZBA_BLADOW")
      df_main <- as.data.frame(json_content)
      df_main$LICZBA_BLADOW <- as.numeric(df_main$LICZBA_BLADOW)
      
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP002_5
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP002_5. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP002_prepared_data_5 <- function(json_content) {

  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("NAZWA_REGULY", "L_SKORYGOWANYCH", "L_URUCHOMIEN"))))
      new_row <- c('-', '0', '0')
      new_row_df <- data.frame(matrix(new_row, ncol = 3, byrow = TRUE, dimnames = list(NULL, colnames(df_main))))
      df_main <- rbind(df_main, new_row_df)

    } else {
      
      colnames(json_content) <- c("NAZWA_REGULY", "L_SKORYGOWANYCH", "L_URUCHOMIEN")
      df_main <- as.data.frame(json_content)
      df_main$L_URUCHOMIEN <- as.numeric(df_main$L_URUCHOMIEN)
      df_main$L_SKORYGOWANYCH <- as.numeric(df_main$L_SKORYGOWANYCH)
      
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)

  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP002_6
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP002_6. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP002_prepared_data_6 <- function(json_content) {

  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("NAZWA_REGULY", "L_SKORYGOWANYCH", "L_URUCHOMIEN"))))
      df_main$NAZWA_REGULY <- as.character(df_main$NAZWA_REGULY)
      new_row <- c('-', '0', '0')
      new_row_df <- data.frame(matrix(new_row, ncol = 3, byrow = TRUE, dimnames = list(NULL, colnames(df_main))))
      df_main <- rbind(df_main, new_row_df)
      
    } else {
      
      colnames(json_content) <- c("NAZWA_REGULY", "L_SKORYGOWANYCH", "L_URUCHOMIEN")
      df_main <- as.data.frame(json_content)
      df_main$L_URUCHOMIEN <- as.numeric(df_main$L_URUCHOMIEN)
      df_main$L_SKORYGOWANYCH <- as.numeric(df_main$L_SKORYGOWANYCH)
      
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP002_7
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP002_7. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP002_prepared_data_7 <- function(json_content) {
  
  createDataFrame <- function(json_content) {
  if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("BANK", "L_SKORYGOWANYCH"))))
      df_main$BANK <- as.character(df_main$BANK)
  } else {
    colnames(json_content) <- c("BANK", "L_SKORYGOWANYCH")
    df_main <- as.data.frame(json_content)
    df_main$L_SKORYGOWANYCH <- as.numeric(df_main$L_SKORYGOWANYCH)
    df_main$BANK <- as.character(df_main$BANK)

  }
    return(df_main)

  }
  df_main <- createDataFrame(json_content)
  df_main <- subset(df_main, df_main$L_SKORYGOWANYCH > 0)

  return(df_main)
}