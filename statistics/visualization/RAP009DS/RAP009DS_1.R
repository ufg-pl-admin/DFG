#' Tworzy obiekt data.frame z podsumowaniem uruchomień anomalii
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dwiema kolumnami,
#' zawierającymi miary łącznej liczby uruchomień, średniej liczby uruchomień i liczby uruchomień w podziale na statusy anomalii.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP002DS_prepared_data_1.
#' @return Obiekt data.frame z liczbą uruchomień reguł walidacyjnych.
#'
#' @examples
#' DATA_RAP009DS_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP009DS_prepared_data_1
#' @export
DATA_RAP009DS_1 <- function(json_content) {
  
  df_main <- getRap_009DS_1_prepared_data(json_content)
  df <- data.frame(
    C1 = c("Średni czas obsługi anomalii", "Łączna liczba anomalii",
           "Łączna liczba anomalii w statusie udostępnienia Udostępniona","Łączna liczba anomalii w statusie udostępnienia Zakończona obsługa"),
    C2 = c(df_main$SR_CZAS_ANOMALII, df_main$LICZBA_ANOMALII,
           df_main$UDOSTEPNIONA, df_main$ZAKONCZONA)
  )
  
  return(df)
}

#' Tworzy obiekt data.frame z podsumowaniem uruchomień anomalii
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i sześcioma kolumnami,
#' zawierającymi miary łącznej liczby uruchomień, średniej liczby uruchomień i liczby uruchomień w podziale na statusy anomalii.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP009DS_prepared_data_1.
#' @return Obiekt ggplot z pięcioma tabelami opisującymi podsumowanie uruchomień anomalii.
#'
#' @examples
#' INFOGRAPHIC_RAP009DS_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP009DS_prepared_data_1
#' @export

INFOGRAPHIC_RAP009DS_1<- function(json_content) {
  
  df_main <- getRap_009DS_1_prepared_data(json_content)

  df1 <- data.frame(var1=c(df_main$LICZBA_ANOMALII,"łączna\n liczba\n anomalii"))
  df2 <- data.frame(var1=c(df_main$SR_CZAS_ANOMALII,"średni czas\n obsługi\n anomalii"))


  df3 <- data.frame(var1=c(df_main$UDOSTEPNIONA,"łączna liczba anomalii\n w statusie udostępnienia\n Udostępniona"))
  df4 <- data.frame(var1=c(df_main$ZAKONCZONA,"łączna liczba anomalii\n w statusie udostępnienia\n Zakończona obsługa"))

  
  df_list <- list(df1, df2, df3, df4)

  
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
  
  ind1 <- find_cell(table_grob_1, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_2, 1, 1, "core-fg")
  ind3 <- find_cell(table_grob_3, 1, 1, "core-fg")
  ind4 <- find_cell(table_grob_4, 1, 1, "core-fg")
  table_grob_1$grobs[ind1][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_3$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_4$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  
  
  
  # title <- textGrob("PODSUMOWANIE URUCHOMIEŃ ANOMALII DLA TWOJEGO PODMIOTU",gp=gpar(fontsize=16, fontface="bold", "URW DIN-DEMI"))
  # 
  # padding <- unit(3.5,"line")
  # table_grob_1 <- gtable_add_rows(table_grob_1,
  #                                 heights = grobHeight(title) + padding,
  #                                 pos = 0)
  # 
  # 
  # table_grob_1 <- gtable_add_grob(table_grob_1, list(title),
  #                                 t=1, l=1, 
  #                                 r=ncol(table_grob_1))
  
  grob_list <- list(table_grob_1,table_grob_2, table_grob_3, table_grob_4)
  
  
  k <-0
  
  for (i in grob_list){
    
    k<- k+1
    
    
    assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
    
  }

  plot <- plot_grid(plot_1, plot_2, plot_3, plot_4, ncol=4, scale = 1.1)


  return(plot)
}
