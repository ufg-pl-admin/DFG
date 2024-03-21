#' Tworzy obiekt ggplot z tabelami opisującymi liczbę umów deweloperskich
#'
#' Funkcja tworzy i zwraca obiekt ggplot z ikoną oraz trzema tabelami
#' opisującymi liczbę umów deweloperskich, umieszczonymi obok siebie.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_007_prepared_data_3.
#' @return Obiekt ggplot z ikoną oraz trzema tabelami opisującymi liczbę umów deweloperskich.
#'
#' @examples
#' INFOGRAPHIC_STAT_007_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getStat_007_prepared_data_3
#' @export
DATA_STAT_007_3 <- function(json_content) {
  df_main_2 <- getStat_007_prepared_data_3(json_content)
  df <- data.frame(
    C1 = c("łączna wartość umów deweloperskich objętych ochroną DFG", "średnia wartość umowy deweloperskiej objętej ochroną DFG",
           "mediana wartości umowy deweloperskiej objętej ochroną DFG"),
    C2 = c(df_main_2$LACZNA_WARTOSC, df_main_2$SREDNIA_WARTOSC, df_main_2$MEDIANA_WARTOSC)
  )
  
  return(df)
}

INFOGRAPHIC_STAT_007_3 <- function(json_content) {
  df_main_2 <- getStat_007_prepared_data_3(json_content)
  
  df1 <- data.frame(var1=c(df_main_2$LACZNA_WARTOSC,"łączna wartość\numów deweloperskich\nzarejestrowanych w systemie DFG"))
  df2 <- data.frame(var1=c(df_main_2$SREDNIA_WARTOSC,"średnia wartość\numowy deweloperskiej\nzarejestrowanej w systemie DFG"))
  df3 <- data.frame(var1=c(df_main_2$MEDIANA_WARTOSC,"mediana wartości\numowy deweloperskiej\nzarejestrowanej w systemie DFG"))
  
  
  df_list <- list(df1,df2,df3)
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(0, 6), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "dodgerblue3", col = NA),
      fg_params = list(fontface = "bold", fontfamily="URWDIN-Regular", fontsize = 15, col = "white", just="left")
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
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontfamily="URWDIN-Demi",fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontfamily="URWDIN-Demi",fontface="bold", col = "#B43C23")
  table_grob_3$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontfamily="URWDIN-Demi",fontface="bold", col = "#B43C23")
  
  
  grob_list <- list(table_grob_1,table_grob_2,table_grob_3)
  
  
  k <-0
  
  for (i in grob_list){
    
    k<- k+1
    assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
    
  }
  
  icon_path_1 <- paste(getwd(), '/icons/zwrot_srodkow_nabywcow4.svg', sep="")
  icon_1 <- ggdraw() + draw_image(icon_path_1, scale = 1.4)
  icon_path_2 <- paste(getwd(), '/icons/monety_czerwone.svg', sep="")
  icon_2 <- ggdraw() + draw_image(icon_path_2, scale = 0.7)
  
  plot <- plot_grid(plot_1, icon_1, plot_2, icon_2, plot_3, ncol = 5, rel_widths = c(1.4,1,1.4,1,1.4))#,rel_heights=c(1.4,1,1.4,1,1.4))#scale=1)
  
  return(plot)
}