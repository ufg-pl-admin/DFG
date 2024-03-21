#' Tworzy obiekt ggplot z tabelami opisującymi liczbę inwestycji wg daty otwarcia
#'
#' Funkcja tworzy i zwraca obiekt ggplot z ikoną oraz pięcioma tabelami
#' opisującymi liczbę inwestycji wg daty otwarcia, umieszczonymi obok siebie.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_008_prepared_data_1.
#' @return Obiekt ggplot z ikoną oraz dwoma tabelami opisującymi liczbę inwestycji wg daty otwarcia.
#'
#' @examples
#' INFOGRAPHIC_STAT_008_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getStat_008_prepared_data_1
#' @export
DATA_STAT_008_1 <- function(json_content) {
  df_main_2 <- getStat_008_prepared_data_1(json_content)
  df <- data.frame(
    C1 = c("liczba uruchomionych MRP objętych ochroną DFG", "liczba zawartych umów deweloperskich objętych ochroną DFG"),
    C2 = c(df_main_2$NM1, df_main_2$NM2)
  )
  
  return(df)
}

INFOGRAPHIC_STAT_008_1 <- function(json_content) {
  df_main_2 <- getStat_008_prepared_data_1(json_content)
  
  df1 <- data.frame(var1=c(df_main_2$NM1,"liczba uruchomionych MRP\nobjętych ochroną DFG"))
  df2 <- data.frame(var1=c(df_main_2$NM2,"liczba zawartych umów\ndeweloperskich objętych ochroną DFG"))
  
  
  df_list <- list(df1,df2)
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, fontfamily="URW DIN-DEMI", just="left"),
      padding = unit(c(0, 6), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontsize = 15, fontfamily="URW DIN-DEMI", col = "white", just="left")
    ),
    rowhead = list(
      fg_params = list(fontface = "plain", fontsize = 15)
    ),
    rowsep = list(col = "red", lty = 2, lwd = 2), 
    colsep = list(col = NA)
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
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")

  grob_list <- list(table_grob_1,table_grob_2)
  
  
  k <-0
  
  for (i in grob_list){
    
    k<- k+1
    assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
    
  }
  
  icon_path <- paste(getwd(), '/icons/gwarancja-dfg.svg', sep="")
  icon <- ggdraw() + draw_image(icon_path, scale = 0.7)
  
  plot <- plot_grid(plot_1, icon, plot_2, ncol = 3, rel_widths = c(1, 0.3,1), rel_heights = c(1, 0.3,1))
  
  return(plot)
}