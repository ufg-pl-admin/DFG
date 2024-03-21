#' Tworzy obiekt ggplot z tabelami opisującymi liczbę umów deweloperskich
#'
#' Funkcja tworzy i zwraca obiekt ggplot z ikoną oraz trzema tabelami
#' opisującymi liczbę umów deweloperskich, umieszczonymi obok siebie.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_007_prepared_data_1.
#' @return Obiekt ggplot z ikoną oraz trzema tabelami opisującymi liczbę umów deweloperskich.
#'
#' @examples
#' INFOGRAPHIC_STAT_007_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getStat_007_prepared_data_1
#' @export
DATA_STAT_007_1 <- function(json_content) {
  df_main_2 <- getStat_007_prepared_data_1(json_content)
  df <- data.frame(
    C1 = c("liczba umów deweloperskich objętych ochroną DFG", "liczba zawartych umów deweloperskich",
           "liczba zakończonych umów deweloperskich"),
    C2 = c(df_main_2$AD1, df_main_2$AD2, df_main_2$AD3)
  )
  return(df)
}

INFOGRAPHIC_STAT_007_1 <- function(json_content) {
  df_main_2 <- getStat_007_prepared_data_1(json_content)
  
  df1 <- data.frame(var1=c(df_main_2$AD1,"liczba\numów deweloperskich\nobjętych ochroną DFG"))
  df2 <- data.frame(var1=c("w tym:"))
  df3 <- data.frame(var1=c(df_main_2$AD2,"liczba zawartych\numów deweloperskich"))
  df4 <- data.frame(var1=c(df_main_2$AD3,"liczba zakończonych\numów deweloperskich"))
  

  
  df_list <- list(df1,df2,df3,df4)
  
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
  ind2 <- find_cell(table_grob_3, 1, 1, "core-fg")
  ind3 <- find_cell(table_grob_4, 1, 1, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_3$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_4$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  
  
  grob_list <- list(table_grob_1,table_grob_2,table_grob_3,table_grob_4)
  
  
  k <-0
  
  for (i in grob_list){
    
    k<- k+1
    assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
    
  }
  
  icon_path_1 <- paste(getwd(), '/icons/umowy.svg', sep="")#paste(getwd(), '/icons/dom-jednorodzinny-w-budowie.svg', sep="")
  icon_1 <- ggdraw() + draw_image(icon_path_1, scale = 0.7)
  icon_path_2 <- paste(getwd(), '/icons/klucze-do-mieszkania.svg', sep="")#paste(getwd(), '/icons/mieszkania-blok-w-budowie.svg', sep="")
  icon_2 <- ggdraw() + draw_image(icon_path_2, scale = 0.7)
  
  plot <- plot_grid(icon_1, plot_1, plot_2, plot_3, plot_4, icon_2, ncol = 6, rel_widths = c(1,1.4,1.4,1.4,1.4,1))#scale=1)
  
  return(plot)
}