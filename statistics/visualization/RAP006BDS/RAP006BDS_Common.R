#' Pobierz i przygotuj dane dla RAP006BDS_1
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP006BDS_1. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRAP006BDS_prepared_data_1 <- function(json_content) {
  colnames(json_content) <- c("L1", "L2", "LP2", "L3", "LP3", "L4", "LP4", "L5", "LP5", "L6", "LP6")
  df_main <- as.data.frame(json_content)

  df_main <- as.data.frame(df_main)
  df_main$L1 <- format(as.numeric(df_main$L1), big.mark=" ")
  df_main$L2 <- format(as.numeric(df_main$L2), big.mark=" ")

  df_main$LP2 <- with(df_main, {
    LP2_numeric <- as.numeric(LP2)
    ifelse(round(LP2_numeric, 1) == floor(LP2_numeric), # Sprawdzenie, czy nie ma kropki dziesiętnej lub liczba dziesiętna wynosi zero
           paste0(formatC(LP2_numeric, digits = 0, format = "d", big.mark = ","), "%"), # Format bez ułamka dziesiętnego
           paste0(formatC(LP2_numeric, digits = 1, format = "f", decimal.mark = ","), "%") # Format z ułamkiem dziesiętnym
    )
  })
  df_main$L3 <- format(as.numeric(df_main$L3), big.mark=" ")

  df_main$LP3 <- with(df_main, {
    LP3_numeric <- as.numeric(LP3)
    ifelse(round(LP3_numeric, 1) == floor(LP3_numeric),
           paste0(formatC(LP3_numeric, digits = 0, format = "d", big.mark = ","), "%"),
           paste0(formatC(LP3_numeric, digits = 1, format = "f", decimal.mark = ","), "%")
    )
  })
  df_main$L4 <- format(as.numeric(df_main$L4), big.mark=" ")

  df_main$LP4 <- with(df_main, {
    LP4_numeric <- as.numeric(LP4)
    ifelse(round(LP4_numeric, 1) == floor(LP4_numeric),
           paste0(formatC(LP4_numeric, digits = 0, format = "d", big.mark = ","), "%"),
           paste0(formatC(LP4_numeric, digits = 1, format = "f", decimal.mark = ","), "%")
    )
  })
  df_main$L5 <- format(as.numeric(df_main$L5), big.mark=" ")

  df_main$LP5 <- with(df_main, {
    LP5_numeric <- as.numeric(LP5)
    ifelse(round(LP5_numeric, 1) == floor(LP5_numeric),
           paste0(formatC(LP5_numeric, digits = 0, format = "d", big.mark = ","), "%"),
           paste0(formatC(LP5_numeric, digits = 1, format = "f", decimal.mark = ","), "%")
    )
  })
  df_main$L6 <- format(as.numeric(df_main$L6), big.mark=" ")
 # df_main$LP6 <- paste0(formatC(as.numeric(df_main$LP6), digits = 1, format = "f", decimal.mark = ","), "%")
  df_main$LP6 <- with(df_main, {
    LP6_numeric <- as.numeric(LP6)
    ifelse(round(LP6_numeric, 1) == floor(LP6_numeric),
           paste0(formatC(LP6_numeric, digits = 0, format = "d", big.mark = ","), "%"),
           paste0(formatC(LP6_numeric, digits = 1, format = "f", decimal.mark = ","), "%")
    )
  })

  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP006BDS_2
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP006BDS_2. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRAP006BDS_prepared_data_2 <- function(json_content) {
  colnames(json_content) <- c("W1", "W2")
  df_main <- as.data.frame(json_content)
  df_main$W1 <- as.numeric(df_main$W1)
  df_main$W2 <- as.numeric(df_main$W2)
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP006BDS_3
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP006BDS_3. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRAP006BDS_prepared_data_3 <- function(json_content) {

  colnames(json_content) <- c("Z1", "P2", "P3", "P4", "P5", "P6")
  df_main <- as.data.frame(json_content)

  df_main$Z1 <- format(as.numeric(df_main$Z1), big.mark=" ")

  df_main$P2 <- with(df_main, {
    P2_numeric <- as.numeric(P2)
    ifelse(round(P2_numeric, 1) == floor(P2_numeric), # Sprawdzenie, czy nie ma kropki dziesiętnej lub liczba dziesiętna wynosi zero
           paste0(formatC(P2_numeric, digits = 0, format = "d", big.mark = ","), "%"), # Format bez ułamka dziesiętnego
           paste0(formatC(P2_numeric, digits = 1, format = "f", decimal.mark = ","), "%") # Format z ułamkiem dziesiętnym
    )
  }) 

  df_main$P3 <- with(df_main, {
    P3_numeric <- as.numeric(P3)
    ifelse(round(P3_numeric, 1) == floor(P3_numeric),
           paste0(formatC(P3_numeric, digits = 0, format = "d", big.mark = ","), "%"),
           paste0(formatC(P3_numeric, digits = 1, format = "f", decimal.mark = ","), "%") 
    )
  })
  
  df_main$P4 <- with(df_main, {
    P4_numeric <- as.numeric(P4)
    ifelse(round(P4_numeric, 1) == floor(P4_numeric),
           paste0(formatC(P4_numeric, digits = 0, format = "d", big.mark = ","), "%"),
           paste0(formatC(P4_numeric, digits = 1, format = "f", decimal.mark = ","), "%") 
    )
  }) 

  df_main$P5 <- with(df_main, {
    P5_numeric <- as.numeric(P5)
    ifelse(round(P5_numeric, 1) == floor(P5_numeric),
           paste0(formatC(P5_numeric, digits = 0, format = "d", big.mark = ","), "%"),
           paste0(formatC(P5_numeric, digits = 1, format = "f", decimal.mark = ","), "%") 
    )
  })
  df_main$P6 <- with(df_main, {
    P6_numeric <- as.numeric(P6)
    ifelse(round(P6_numeric, 1) == floor(P6_numeric),
           paste0(formatC(P6_numeric, digits = 0, format = "d", big.mark = ","), "%"),
           paste0(formatC(P6_numeric, digits = 1, format = "f", decimal.mark = ","), "%") 
    )
  })
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP006BDS_4
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP006BDS_4. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRAP006BDS_prepared_data_4 <- function(json_content) {
  #print(json_content)
  colnames(json_content) <- c("OKRES", "LICZBA_ZASILEN", "LICZBA_BLEDOW", "LICZBA_OSTRZEZEN")

  df_main <- as.data.frame(json_content)
  df_main$LICZBA_ZASILEN <- as.numeric(df_main$LICZBA_ZASILEN)
  df_main$LICZBA_BLEDOW <- as.numeric(df_main$LICZBA_BLEDOW)
  df_main$LICZBA_OSTRZEZEN <- as.numeric(df_main$LICZBA_OSTRZEZEN)
  df_main$OKRES <- factor(df_main$OKRES, levels = unique(df_main$OKRES), ordered = TRUE)
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP006BDS_5
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP006BDS_5. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRAP006BDS_prepared_data_5 <- function(json_content) {
  #print(json_content)
  colnames(json_content) <- c("DATA_ZASILENIA", "PRZYJETE", "PRZYJETE_Z_OSTRZEZENIEM", "ODRZUCONE", "W_WERYFIKACJI", "ROBOCZE", "LACZNIE")
  df_main <- as.data.frame(json_content)
  df_main$PRZYJETE <- as.numeric(df_main$PRZYJETE)
  df_main$PRZYJETE_Z_OSTRZEZENIEM <- as.numeric(df_main$PRZYJETE_Z_OSTRZEZENIEM)
  df_main$ODRZUCONE <- as.numeric(df_main$ODRZUCONE)
  df_main$W_WERYFIKACJI <- as.numeric(df_main$W_WERYFIKACJI)
  df_main$ROBOCZE <- as.numeric(df_main$ROBOCZE)
  df_main$LACZNIE <- as.numeric(df_main$LACZNIE)
  df_main$DATA_ZASILENIA <- factor(df_main$DATA_ZASILENIA, levels = unique(df_main$DATA_ZASILENIA), ordered = TRUE)
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP006BDS_6
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP006BDS_6. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRAP006BDS_prepared_data_6 <- function(json_content) {
  #print(json_content)

  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("REGULA", "LICZBA_ZASILEN", "LICZBA_BLADOW"))))
      df_main$REGULA <- as.character(df_main$REGULA)
      
    } else {
      
      colnames(json_content) <- c("REGULA", "LICZBA_ZASILEN", "LICZBA_BLADOW")
      df_main <- as.data.frame(json_content)
      df_main$LICZBA_ZASILEN<- as.numeric(df_main$LICZBA_ZASILEN)
      df_main$LICZBA_BLADOW <- as.numeric(df_main$LICZBA_BLADOW)
      
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}

#' Pobierz i przygotuj dane dla RAP006BDS_7
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP006BDS_7. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export
getRAP006BDS_prepared_data_7 <- function(json_content) {
  #print(json_content)
  createDataFrame <- function(json_content) {
    
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("REGULA", "LICZBA_ZASILEN", "LICZBA_OSTRZEZEN"))))
      df_main$REGULA <- as.character(df_main$REGULA)
      
    } else {
      
      colnames(json_content) <- c("REGULA", "LICZBA_ZASILEN", "LICZBA_OSTRZEZEN")
      df_main <- as.data.frame(json_content)
      df_main$LICZBA_ZASILEN <- as.numeric(df_main$LICZBA_ZASILEN)
      df_main$LICZBA_OSTRZEZEN <- as.numeric(df_main$LICZBA_OSTRZEZEN)
      
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)
  
  return(df_main)
}