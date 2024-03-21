#' Pobierz i przygotuj dane dla RAP003BDS_2
#'
#' Funkcja przygotowuje dane dla infografiki i tabeli danych dla RAP003BDS_2. Przetwarza dane JSON,
#' konwertuje wartości i formatuje je w odpowiedni sposób.
#'
#' @param json_content Ramka danych w formacie JSON do przetworzenia.
#' @return df_main Ramka danych z przetworzonymi wartościami.
#'
#' @export

getRap_003_prepared_data_2 <- function(json_content) {
  colnames(json_content) <- c('PROCENT_ROZLICZONYCH', 'PROCENT_NIEROZLICZONYCH_DEW', 'PROCENT_NIEROZLICZONYCH_BANK', 
                              'SREDNI_CZAS_INSKLA', 'SREDNI_CZAS_DANEAN')
  df <- as.data.frame(json_content)
  
  
  return(df)
}

#' Tworzy obiekt data.frame z benchamrkiem miar zasileń WPLMRP, INSKLA i DANEAN.
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i pięcioma kolumnami,
#' zawierającymi benchmark systemu dla liczby uzupełnienia danych w  WPLMRP o INSKLA oraz DANEAN.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP003BDS_prepared_data_2.
#' @return Obiekt data.frame z benchamrkiem miar zasileń WPLMRP, INSKLA i DANEAN.
#'
#' @examples
#' DATA_RAP003BDS_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP003BDS_prepared_data_2
#' @export

DATA_RAP003BDS_2 <- function(json_content) {
  
  df_main <- getRap_003_prepared_data_2(json_content)
  df <- data.frame(
    C1 = c("Procent wpłat rozliczonych", "Procent wpłat nierozliczonych przez Dewelopera",
           "Średni czas przekazania INSKLA", "Procent wpłat nierozliczonych przez Bank",
           "Średni czas przekazania DANEAN"),
    C2 = c(df_main$PROCENT_ROZLICZONYCH, df_main$PROCENT_NIEROZLICZONYCH_DEW,  
           df_main$SREDNI_CZAS_INSKLA, df_main$PROCENT_NIEROZLICZONYCH_BANK, df_main$SREDNI_CZAS_DANEAN)  
  )
  
  return(df)
}


#' Tworzy obiekt data.frame z benchamrkiem miar zasileń WPLMRP, INSKLA i DANEAN.
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i pięcioma kolumnami,
#' zawierającymi benchmark systemu dla liczby uzupełnienia danych w  WPLMRP o INSKLA oraz DANEAN.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP003BDS_prepared_data_2.
#' @return Obiekt grobtable z ikoną oraz pięcioma tabelami opisującymi benchamrkiem miar zasileń.
#'
#' @examples
#' INFOGRAPHIC_RAP003BDS_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP003BDS_prepared_data_2
#' @export

INFOGRAPHIC_RAP003BDS_2<- function(json_content) {
  
  df_main <- getRap_003_prepared_data_2(json_content)
  
  
  
  
  
  df1 <- data.frame(var1=c(df_main$PROCENT_ROZLICZONYCH,"procent
 wpłat 
 rozliczonych"))
  df2 <- data.frame(var1=c(df_main$PROCENT_NIEROZLICZONYCH_DEW,"procent wpłat
 nierozliczonych
przez Dewelopera"))
  df3 <- data.frame(var1=c(df_main$PROCENT_NIEROZLICZONYCH_BANK,"procent wpłat
  nierozliczonych 
przez Bank"))
  df4 <- data.frame(var1=c(df_main$SREDNI_CZAS_INSKLA,"średni czas
przekazania
INSKLA"))
  df5 <- data.frame(var1=c(df_main$SREDNI_CZAS_DANEAN,"średni czas
przekazania
DANEAN"))
  
  
  #   df5 <- data.frame(var1=c(df_main$OMRP_ZMRP,"liczba banków
  #   prowadzących  
  # otwarte i zamknięte MRP"))
  #   
  
  df_list <- list(df1,df2,df3,df4,df5)
  
  
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
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_3$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_4$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_5$grobs[ind5][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  
  
  grob_list <- list(table_grob_1,table_grob_2,table_grob_3,table_grob_4,table_grob_5)
  
  
  k <-0
  
  for (i in grob_list){
    
    k<- k+1
    assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
    
  }
  
  
  plot <- plot_grid(plot_1, plot_2, plot_3, plot_4, plot_5, ncol=5)
  
  
  return(plot)
}