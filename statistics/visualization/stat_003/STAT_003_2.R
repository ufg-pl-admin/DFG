#' Tworzy obiekt ggplot z tabelami opisującymi liczbę założonych MRP
#'
#' Funkcja tworzy i zwraca obiekt ggplot z ikoną oraz pięcioma tabelami
#' opisującymi liczbę założonych MRP, umieszczonymi obok siebie.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_003_prepared_data_2.
#' @return Obiekt ggplot z ikoną oraz pięcioma tabelami opisującymi liczbę założonych MRP.
#'
#' @examples
#' INFOGRAPHIC_STAT_003_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getStat_002_prepared_data_2
#' @export
DATA_STAT_003_2 <- function(json_content) {
  df_main_2 <- getStat_003_prepared_data_1(json_content)
  df <- data.frame(
    C1 = c("liczba założonych MRP", "liczba założonych otwartych MRP",
           "liczba założonych zamkniętych MRP"),
    C2 = c(df_main_2$L_MRP, df_main_2$L_OTWARTYCH, df_main_2$L_ZAMKNIETYCH)
  )
  
  return(df)
}

INFOGRAPHIC_STAT_003_2 <- function(json_content) {
  df_main_2 <- getStat_003_prepared_data_1(json_content)
  
  df1 <- data.frame(var1=c(df_main_2$L_MRP,"liczba\nzałożonych MRP"))
  df2 <- data.frame(var1=c("w tym:"))
  df3 <- data.frame(var1=c(df_main_2$L_OTWARTYCH,"liczba założonych\notwartych MRP"))
  df4 <- data.frame(var1=c(df_main_2$L_ZAMKNIETYCH,"liczba założonych\nzamkniętych MRP"))
  
  
  df_list <- list(df1,df2,df3,df4)
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(0, 6), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "dodgerblue3", col = NA),
      fg_params = list(fontsize = 15, col = "white", just="left")
    )
    
  )
  custom_theme_2 <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Regular", fontsize = 15, just="left")
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
    
    table_grob_2 <- tableGrob(df2, theme = custom_theme_2, rows=NULL,cols=NULL)
    
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
  
  icon_path <- paste(getwd(), '/icons/kredyt.svg', sep="")
  icon <- ggdraw() + draw_image(icon_path, scale = 0.9)
  
  plot <- plot_grid(icon, plot_1, plot_2, plot_3, plot_4, ncol = 5, rel_widths = c(1.4,1.2,1,2.3,2.3))#scale=1)
  
  return(plot)
}