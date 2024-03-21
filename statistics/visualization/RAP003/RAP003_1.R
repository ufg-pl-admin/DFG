#' Pobierz i przygotuj dane dla RAP003_1
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP003_1. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRap_003_prepared_data <- function(json_content) {
  colnames(json_content) <- c('LICZBA_WPLMRP',
                              'WPLATY_WPLMRP',
                              'ROZLICZONE_WPLMRP',
                              'LICZBA_INSKLA',
                              'SREDNI_CZAS_INSKLA',
                              'KOMPLETNOSC_INSKLA_WPLMRP',
                              'WPLATY_BEZ_INSKLA',
                              'LICZBA_WPLATY_BEZ_INSKLA',
                              'LICZBA_DANEAN',
                              'SREDNI_CZAS_DANEAN',
                              'KOMPLETNOSC_DANEAN_WPLMRP',
                              'WPLATY_BEZ_DANEAN',
                              'LICZBA_WPLATY_BEZ_DANEAN',
                              'ROZLICZONE_INSKLA_DANEAN',
                              'LICZBA_ROZLICZONYCH_INSKLA_DANEAN')
  df <- as.data.frame(json_content)
  df$LICZBA_WPLMRP <- as.numeric(df$LICZBA_WPLMRP)
  df$WPLATY_WPLMRP <- as.numeric(df$WPLATY_WPLMRP)
  df$LICZBA_INSKLA <- as.numeric(df$LICZBA_INSKLA)
  df$WPLATY_BEZ_INSKLA <- as.numeric(df$WPLATY_BEZ_INSKLA)
  df$LICZBA_WPLATY_BEZ_INSKLA <- as.numeric(df$LICZBA_WPLATY_BEZ_INSKLA)
  df$WPLATY_BEZ_DANEAN<- as.numeric(df$WPLATY_BEZ_DANEAN)
  df$LICZBA_WPLATY_BEZ_DANEAN<- as.numeric(df$LICZBA_WPLATY_BEZ_DANEAN)
  df$ROZLICZONE_INSKLA_DANEAN<- as.numeric(df$ROZLICZONE_INSKLA_DANEAN)
  df$LICZBA_ROZLICZONYCH_INSKLA_DANEAN<- as.numeric(df$LICZBA_ROZLICZONYCH_INSKLA_DANEAN)
  
  return(df)
}

#' Tworzy obiekt data.frame z liczbą zasileń WPLMRP, INSKLA i DANEAN.
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dziewiętnastoma kolumnami,
#' zawierającymi liczbę z weryfikacją uzupełnienia danych w  WPLMRP o INSKLA oraz DANEAN.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP003_prepared_data.
#' @return Obiekt data.frame z liczbą zasileń WPLMRP, INSKLA i DANEAN.
#'
#' @examples
#' DATA_RAP003_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP003_prepared_data
#' @export

DATA_RAP003_1 <- function(json_content) {
  
  df_main <- getRap_003_prepared_data(json_content)
  df <- data.frame(
    C1 = c("Łączna liczba poprawnie przekazanych WPLMRP",
           "Kwota składek z WPLMRP",
           "Procent rozliczonych składek WPLMRP",
           "Łączna liczba poprawnie przekazanych INSKLA",
           "Średni czas przekazania INSKLA (od WPLMRP)",
           "Procent kompletności INSKLA do WPLMRP",
           "Kwota zaległych składek bez INSKLA",
           "Liczba zaległych składek bez INSKLA",
           "Łączna liczba poprawnie przekazanych DANEAN",
           "Średni czas przekazania DANEAN (od WPLMRP)",
           "Procent kompletności DANEAN do WPLMRP",
           "Kwota zaległych składek bez DANEAN",
           "Liczba zaległych składek bez DANEAN",
           "Liczba składek rozliczonych",
           "Kwota składek rozliczonych"),
    C2 = c(df_main$LICZBA_WPLMRP,
           df_main$WPLATY_WPLMRP,
           df_main$ROZLICZONE_WPLMRP,
           df_main$LICZBA_INSKLA,
           df_main$SREDNI_CZAS_INSKLA,
           df_main$KOMPLETNOSC_INSKLA_WPLMRP,
           df_main$WPLATY_BEZ_INSKLA,
           df_main$LICZBA_WPLATY_BEZ_INSKLA,
           df_main$LICZBA_DANEAN,
           df_main$SREDNI_CZAS_DANEAN,
           df_main$KOMPLETNOSC_DANEAN_WPLMRP,
           df_main$WPLATY_BEZ_DANEAN,
           df_main$LICZBA_WPLATY_BEZ_DANEAN,
           df_main$LICZBA_ROZLICZONYCH_INSKLA_DANEAN,
           df_main$ROZLICZONE_INSKLA_DANEAN)
  )

  return(df)
}

#' Tworzy obiekt data.frame z liczbą zasileń WPLMRP, INSKLA i DANEAN.
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dziewiętnastoma kolumnami,
#' zawierającymi liczbę z weryfikacją uzupełnienia danych w WPLMRP o INSKLA oraz DANEAN.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP003_prepared_data.
#' @return Obiekt grobtable z ikoną oraz pięcioma tabelami opisującymi benchamrkiem miar zasileń.
#'
#' @examples
#' INFOGRAPHIC_RAP003_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP003_prepared_data
#' @export

INFOGRAPHIC_RAP003_1<- function(json_content) {
  
  df_main <- getRap_003_prepared_data(json_content)
  
  
  
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

  convert_to_millions_or_billions_or_thousands_currency <- function(value) {
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
      amount <- formatC(value, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, "")
    }
  }
  
  df_main$WPLATY_WPLMRP<- sapply(df_main$WPLATY_WPLMRP, convert_to_millions_or_billions_or_thousands_currency)
  df_main$WPLATY_BEZ_INSKLA<- sapply(df_main$WPLATY_BEZ_INSKLA, convert_to_millions_or_billions_or_thousands_currency)
  df_main$WPLATY_BEZ_DANEAN<- sapply(df_main$WPLATY_BEZ_DANEAN, convert_to_millions_or_billions_or_thousands_currency)
  df_main$ROZLICZONE_INSKLA_DANEAN<- sapply(df_main$ROZLICZONE_INSKLA_DANEAN, convert_to_millions_or_billions_or_thousands_currency)
  df_main$LICZBA_ROZLICZONYCH_INSKLA_DANEAN<- sapply(df_main$LICZBA_ROZLICZONYCH_INSKLA_DANEAN, convert_to_millions_or_billions_or_thousands)
  df_main$LICZBA_WPLATY_BEZ_DANEAN<- sapply(df_main$LICZBA_WPLATY_BEZ_DANEAN, convert_to_millions_or_billions_or_thousands)
  df_main$LICZBA_WPLATY_BEZ_INSKLA<- sapply(df_main$LICZBA_WPLATY_BEZ_INSKLA, convert_to_millions_or_billions_or_thousands)


  df1 <- data.frame(var1=c(df_main$LICZBA_WPLMRP,"łączna liczba 
poprawnie przekazanych 
 WPLMRP"))
  df2 <- data.frame(var1=c(df_main$WPLATY_WPLMRP,"kwota
      składek
           z WPLMRP           "))
  df3 <- data.frame(var1=c(df_main$ROZLICZONE_WPLMRP,"procent
  rozliczonych składek
WPLMRP"))
  
  
  df4 <- data.frame(var1=c(df_main$LICZBA_INSKLA,"łączna liczba 
poprawnie przekazanych 
 INSKLA"))
  df5 <- data.frame(var1=c(df_main$SREDNI_CZAS_INSKLA,"średni czas
  przekazania
INSKLA (od WPLMRP)"))
  df6 <- data.frame(var1=c(df_main$KOMPLETNOSC_INSKLA_WPLMRP,"procent
  kompletności
INSKLA do WPLMRP"))
  df7 <- data.frame(var1=c(df_main$WPLATY_BEZ_INSKLA,"kwota
  zaległych składek
           bez INSKLA           "))
  df8 <- data.frame(var1=c(df_main$LICZBA_WPLATY_BEZ_INSKLA,"liczba
  zaległych składek
bez INSKLA"))
  

  
  df9 <- data.frame(var1=c(df_main$LICZBA_DANEAN,"łączna liczba 
poprawnie przekazanych 
 DANEAN"))
  df10 <- data.frame(var1=c(df_main$SREDNI_CZAS_DANEAN,"średni czas
  przekazania
DANEAN (od WPLMRP)"))
  df11 <- data.frame(var1=c(df_main$KOMPLETNOSC_DANEAN_WPLMRP,"procent
  kompletności
DANEAN do WPLMRP"))
  df12 <- data.frame(var1=c(df_main$WPLATY_BEZ_DANEAN,"kwota
  zaległych składek
           bez DANEAN           "))
  df13 <- data.frame(var1=c(df_main$LICZBA_WPLATY_BEZ_DANEAN,"liczba
  zaległych składek
bez DANEAN"))

    df14 <- data.frame(var1=c(df_main$LICZBA_ROZLICZONYCH_INSKLA_DANEAN,"liczba
składek rozliczonych"))
  
  df15 <- data.frame(var1=c(df_main$ROZLICZONE_INSKLA_DANEAN,"kwota
składek rozliczonych"))
  
  
#   df5 <- data.frame(var1=c(df_main$OMRP_ZMRP,"liczba banków
#   prowadzących  
# otwarte i zamknięte MRP"))
#   
  
  df_list <- list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15)
  
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(0,6),"mm")
    ),
    colhead = list(
      bg_params = list(fill = "dodgerblue3", col = NA),
      fg_params = list(fontsize = 15, col = "white", just="left")
    )
    
    
  )
  
  
  j <-0
  
  for (i in df_list){
    
    j<- j+1
    
    assign(paste0("table_grob_",j), tableGrob(i, theme = custom_theme, rows=NULL,cols=NULL))
    
    
  }
  
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_2, 1, 1, "core-fg")
  ind3 <- find_cell(table_grob_3, 1, 1, "core-fg")
  ind4 <- find_cell(table_grob_4, 1, 1, "core-fg")
  ind5 <- find_cell(table_grob_5, 1, 1, "core-fg")
  ind6 <- find_cell(table_grob_6, 1, 1, "core-fg")
  ind7 <- find_cell(table_grob_7, 1, 1, "core-fg")
  ind8 <- find_cell(table_grob_8, 1, 1, "core-fg")
  ind9 <- find_cell(table_grob_9, 1, 1, "core-fg")
  ind10 <- find_cell(table_grob_10, 1, 1, "core-fg")
  ind11 <- find_cell(table_grob_11, 1, 1, "core-fg")
  ind12 <- find_cell(table_grob_12, 1, 1, "core-fg")
  ind13 <- find_cell(table_grob_13, 1, 1, "core-fg")
  ind14 <- find_cell(table_grob_14, 1, 1, "core-fg")
  ind15 <- find_cell(table_grob_15, 1, 1, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_3$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_4$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_5$grobs[ind5][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_6$grobs[ind6][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_7$grobs[ind7][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_8$grobs[ind8][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_9$grobs[ind9][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_10$grobs[ind10][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_11$grobs[ind11][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_12$grobs[ind12][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_13$grobs[ind13][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_14$grobs[ind14][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_15$grobs[ind15][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  
  
  
  grob_list <- list(table_grob_1,table_grob_2,table_grob_3,table_grob_4,table_grob_5,table_grob_6,table_grob_7,table_grob_8,
                    table_grob_9,table_grob_10,table_grob_11,table_grob_12,table_grob_13,table_grob_14,table_grob_15)
  
  
  k <-0
  
  for (i in grob_list){
    
    k<- k+1
    assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
    
  }
  
 
  grid_1 <- plot_grid(plot_1, plot_2, plot_3, ncol=3)
  grid_2 <- plot_grid(plot_4, plot_5, plot_6, plot_7, plot_8, ncol=5)
  grid_3 <- plot_grid(plot_9, plot_10, plot_11, plot_12, plot_13, ncol=5)
  grid_4 <- plot_grid(plot_14, plot_15, ncol=2)
  
  plot <- plot_grid(grid_1,grid_2, grid_3, grid_4, nrow=4)
  
  
  return(plot)
}