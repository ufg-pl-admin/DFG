#' Tworzy obiekt data.frame z liczbą banków prowadzących MRP
#'
#' Funkcja tworzy i zwraca obiekt data.frame z czterema wierszami i dwiema kolumnami,
#' zawierającymi liczbę banków prowadzących MRP, podzielonych na różne kategorie.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_001_prepared_data_2.
#' @return Obiekt data.frame z liczbą banków prowadzących MRP.
#'
#' @examples
#' DATA_STAT_001_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom getStat_001_prepared_data_2
#' @export
DATA_STAT_001_2 <- function(json_content) {
  df_main_2 <- getStat_001_prepared_data_2(json_content)
  df <- data.frame(
    C1 = c("liczba banków prowadzących MRP", "liczba banków prowadzących wyłącznie otwarte MRP",
           "liczba banków prowadzących wyłącznie zamknięte MRP", "liczba banków prowadzących otwarte i zamknięte MRP"),
    C2 = c(df_main_2$SUMA, df_main_2$OMRP, df_main_2$ZMRP, df_main_2$OMRP_ZMRP)
  )
  
  return(df)
}

#' Tworzy obiekt ggplot z tabelami opisującymi liczbę banków prowadzących MRP
#'
#' Funkcja tworzy i zwraca obiekt ggplot z ikoną oraz pięcioma tabelami
#' opisującymi liczbę banków prowadzących MRP, umieszczonymi obok siebie.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_001_prepared_data_2.
#' @return Obiekt ggplot z ikoną oraz pięcioma tabelami opisującymi liczbę banków prowadzących MRP.
#'
#' @examples
#' INFOGRAPHIC_STAT_001_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getStat_001_prepared_data_2
#' @export
INFOGRAPHIC_STAT_001_2 <- function(json_content) {
  
  
   
   
  # font_path <- paste(getwd(), '/fonts', sep="")
  # font_paths(font_path)
  # font_add("URWDIN-Regular", regular = "URWDIN-Regular.ttf")
  # font_add("URWDIN-Demi", regular = "URWDIN-Demi.ttf")
  # 


  # windowsFonts(URWDIN=windowsFont("URWDIN-Regular"))
  # windowsFonts(URWDIND=windowsFont("URWDIN-Demi"))  

  
  df_main_2 <- getStat_001_prepared_data_2(json_content)
  
  df1 <- data.frame(var1=c(df_main_2$SUMA,"liczba banków 
prowadzących 
 MRP"))
  df2 <- data.frame(var1=c("w tym:"))
  df3 <- data.frame(var1=c(df_main_2$OMRP,"liczba banków
  prowadzących 
wyłącznie otwarte MRP"))
  df4 <- data.frame(var1=c(df_main_2$ZMRP,"liczba banków
  prowadzących 
wyłącznie zamknięte MRP"))
  df5 <- data.frame(var1=c(df_main_2$OMRP_ZMRP,"liczba banków
  prowadzących  
otwarte i zamknięte MRP"))
  
  
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
    
    
  }
  
  
  table_grob_2 <- tableGrob(df2, theme = custom_theme_2, rows=NULL,cols=NULL)
  
  
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_3, 1, 1, "core-fg")
  ind3 <- find_cell(table_grob_4, 1, 1, "core-fg")
  ind4 <- find_cell(table_grob_5, 1, 1, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_3$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_4$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_5$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  
  
  
  grob_list <- list(table_grob_1,table_grob_2,table_grob_3,table_grob_4,table_grob_5)
  
  
  k <-0
  
  for (i in grob_list){
    
    k<- k+1
    assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
    
  }
  
  icon_path <- paste(getwd(), '/icons/bank.svg', sep="")#paste(getwd(), '/icons/umowy.svg', sep="")
  icon <- ggdraw() + draw_image(icon_path, scale = 0.9)
  
  plot <- plot_grid(icon, plot_1, plot_2, plot_3, plot_4, plot_5, ncol=6, rel_widths = c(1.4,1.2,1,2.3,2.3,2.3))
  
  return(plot)
}

















