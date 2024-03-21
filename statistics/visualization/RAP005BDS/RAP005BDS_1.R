#' Tworzy obiekt data.frame z podsumowaniem miar wszystkich wątków zgrupowana w kategoriach
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dziewięcioma kolumnami,
#' zawierającymi liczbę wszystkich wątków i ich zgrupowaną reprezentację w kategoriach.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP005BDS_prepared_data_3.
#' @return Obiekt data.frame z liczbą miar wszystkich wątków.
#'
#' @examples
#' DATA_RAP005BDS_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP005BDS_prepared_data_3
#' @export
DATA_RAP005BDS_1 <- function(json_content) {
  
  df_main <- getRap_005_3_prepared_data(json_content)
  df <- data.frame(
    C1 = c("Liczba wszystkich wątków",
           "Liczba wątków kategorii Zobowiązania wobec DFG",
           "Liczba wątków kategorii Rozliczanie składek",
           "Liczba wątków kategorii Problemy techniczne",
           "Liczba wątków kategorii Niezgodność danych",
           "Liczba wątków kategorii Inne",
           "Liczba wątków kategorii Anomalie"),
    C2 = c(df_main$WSZYSTKIE_WATKI,
           df_main$ZOBOWIAZANIA_DFG,
           df_main$ROZLICZENIE_SKLADEK,
           df_main$PROBLEMY_TECH,
           df_main$NIEZGODNOSC_DANYCH,
           df_main$INNE,
           df_main$ANOMALIE)
  )
  
  return(df)
}

#' Tworzy obiekt data.frame z podsumowaniem miar wszystkich wątków zgrupowana w kategoriach
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dziewięcioma kolumnami,
#' zawierającymi liczbę wszystkich wątków i ich zgrupowaną reprezentację w kategoriach.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP005BDS_prepared_data_3.
#' @return Obiekt ggplot z dwoma tabelami grob podsumowanie miar wszystkich wątków.
#'
#' @examples
#' INFOGRAPHIC_RAP005BDS_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP005BDS_prepared_data_3
#' @export
INFOGRAPHIC_RAP005BDS_1<- function(json_content) {
  
  df_main <- getRap_005_3_prepared_data(json_content)
  
  
  
  df1 <- data.frame(var1=c(df_main$ZOBOWIAZANIA_DFG,"liczba
  wątków kategorii
  Zobowiązania wobec DFG"))
  df2 <- data.frame(var1=c(df_main$ROZLICZENIE_SKLADEK,"liczba
  wątków kategorii
  Rozliczanie składek"))
  df3 <- data.frame(var1=c(df_main$PROBLEMY_TECH,"liczba
  wątków kategorii
  Problemy techniczne"))
  

  df4 <- data.frame(var1=c(df_main$NIEZGODNOSC_DANYCH,"liczba
  wątków kategorii
  Niezgodność danych"))
  df5 <- data.frame(var1=c(df_main$INNE,"liczba
  wątków kategorii
 Inne"))
  df6 <- data.frame(var1=c(df_main$ANOMALIE,"liczba
  wątków kategorii
 Anomalie"))

  df7 <- data.frame(var1 = c(df_main$WSZYSTKIE_WATKI, "liczba\n wszystkich\n wątków "))
  
  
  
  df_list <- list(df1,df2,df3,df4,df5,df6,df7)
  
  
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
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_2, 1, 1, "core-fg")
  table_grob_2$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_3, 1, 1, "core-fg")
  table_grob_3$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_4, 1, 1, "core-fg")
  table_grob_4$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_5, 1, 1, "core-fg")
  table_grob_5$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_6, 1, 1, "core-fg")
  table_grob_6$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_7, 1, 1, "core-fg")
  table_grob_7$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  
  # title <- textGrob("MIARY WSZYSTKICH WĄTKÓW PER KATEGORIA",gp=gpar(fontsize=16, fontface="bold", "URW DIN-DEMI"))
  # 
  # padding <- unit(3.5,"line")
  # table_grob_9 <- gtable_add_rows(table_grob_9,
  #                                 heights = grobHeight(title) + padding,
  #                                 pos = 0)
  # 
  # 
  # table_grob_9 <- gtable_add_grob(table_grob_9, list(title),
  #                                 t=1, l=1, 
  #                                 r=ncol(table_grob_9))
  
  grob_list <- list(table_grob_1,
                    table_grob_2,
                    table_grob_3,
                    table_grob_4,
                    table_grob_5,
                    table_grob_6,
                    table_grob_7)
  
  
  k <- 0
  
  for (i in grob_list){
    
    k <- k + 1
    
    
    assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
    
  }
 
 
  grid_1 <- plot_grid(plot_1, plot_2, plot_3, ncol = 3)
  grid_2 <- plot_grid(plot_4, plot_5, plot_6, ncol = 3)

  plot <- plot_grid(plot_7,grid_1, grid_2, ncol=1, scale=1.1)
  
  
  return(plot)
  
}
