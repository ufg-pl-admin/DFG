#' Tworzy obiekt ggplot z tabelami opisującymi liczbę deweloperów w systemie
#'
#' Funkcja tworzy i zwraca obiekt ggplot z ikoną oraz tabelą
#' opisującą liczbę deweloperów w systemie, umieszczonymi obok siebie.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_004_prepared_data_1.
#' @return Obiekt ggplot z ikoną oraz tabelą opisującą liczbę deweloperów w systemie.
#'
#' @examples
#' INFOGRAPHIC_STAT_004_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getStat_004_prepared_data_1
#' @export
DATA_STAT_004_1 <- function(json_content) {
  
  colnames(json_content) <- c("LICZBA_DEWELOPEROW")
  df_main <- as.data.frame(json_content)
  df_main$LICZBA_DEWELOPEROW <- as.numeric(df_main$LICZBA_DEWELOPEROW)
  
  
  df <- data.frame(
    C1 = c("liczba deweloperów posiadających rachunki MRP"),
    C2 = c(df_main$LICZBA_DEWELOPEROW)
  )
 
  
  return(df)
}



INFOGRAPHIC_STAT_004_1 <- function(json_content) {
  
  colnames(json_content) <- c("LICZBA_DEWELOPEROW")
  df_main <- as.data.frame(json_content)
  df_main$LICZBA_DEWELOPEROW <- as.numeric(df_main$LICZBA_DEWELOPEROW)
  
  df <- data.frame(var1=c(df_main$LICZBA_DEWELOPEROW,"liczba\ndeweloperów\nprowadzących\ninwestycje"))
  df1 <- data.frame(var1=" ")
  

  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(0,6),"mm")
    ),
    colhead = list(
      bg_params = list(fill = "dodgerblue3", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, col = "white", just="left")
    )
    
  )
  

  table_grob <- tableGrob(df, theme = custom_theme, rows=NULL,cols=NULL)
  table_grob_1 <- tableGrob(df1, theme = custom_theme, rows=NULL,cols=NULL)

  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  
  
  ind <- find_cell(table_grob, 1, 1, "core-fg")
  table_grob$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  
  
  
  icon_path <- paste(getwd(), '/icons/deweloper.svg', sep="")#paste(getwd(), '/icons/umowy.svg', sep="")
  icon <- ggdraw() + draw_image(icon_path, scale = 0.65)
  icon  <- ggplotGrob(icon)

  
  plot_1 <- ggplot() + theme_void() + annotation_custom(table_grob)
  plot_2 <- ggplot() + theme_void() + annotation_custom(table_grob_1)

  
  plot <- plot_grid(plot_2,icon,plot_1,plot_2,ncol=4)

  return(plot)
}

