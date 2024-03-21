#' Tworzy obiekt data.frame z miarami inwestycji w podziale na statusy DFG
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i czterema kolumnami,
#' zawierającymi miary inwestycji w podziale na statusy DFG.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_7.
#' @return Obiekt data.frame z miarami inwestycji w podziale na statusy DFG.
#'
#' @examples
#' DATA_RAP004B_7(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP004B_prepared_data_7
#' @export
DATA_RAP004B_7 <- function(json_content) {
  df_main_2 <- getRAP004B_prepared_data_5(json_content)

  df <- data.frame(C1 = c('liczba inwestycji o statusie DFG Brak zasilenia',
                          'liczba inwestycji o statusie DFG Brak zwrotu środków',
                          'liczba inwestycji o statusie DFG Wstrzymanie wypłat'),
                   C2 = c(df_main_2$L_BRAKZASILENIA,
                          df_main_2$L_BRAKZWROTU,
                          df_main_2$L_WSTRZBANK))
  
  return(df)
}

#' Tworzy obiekt data.frame z miarami inwestycji w podziale na statusy DFG
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i czterema kolumnami,
#' zawierającymi miary inwestycji w podziale na statusy DFG.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_7.
#' @return Obiekt grob_table prezentujący liczbę inwestycji w podziale na statusy DFG.
#'
#' @examples
#' INFOGRAPHIC_RAP004B_7(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP004B_prepared_data_7
#' @export
INFOGRAPHIC_RAP004B_7 <- function(json_content) {
  df_main_2 <-  getRAP004B_prepared_data_5(json_content)
  
  df_main_2$L_BRAKZASILENIA <- format(as.numeric(df_main_2$L_BRAKZASILENIA), big.mark=" ")
  df_main_2$L_BRAKZWROTU <- format(as.numeric(df_main_2$L_BRAKZWROTU), big.mark=" ")
  df_main_2$L_WSTRZBANK <- format(as.numeric(df_main_2$L_WSTRZBANK), big.mark=" ")

  MIARY_STATUSOW_1 <- data.frame(ROW_1 = c(df_main_2$L_BRAKZASILENIA, 'liczba\ninwestycji o statusie DFG\nBrak zasilenia'),
                                 ROW_2 = c(df_main_2$L_BRAKZWROTU, 'liczba\ninwestycji o statusie DFG\nBrak zwrotu środków'),
                                 ROW_3 = c(df_main_2$L_WSTRZBANK, 'liczba\ninwestycji o statusie DFG\nWstrzymanie wypłat'))

  
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-REGULAR"),
      padding = unit(c(10, 6), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-DEMI")
    ),
    rowhead = list(
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-DEMI")
    ),
    rowsep = list(col = "red", lty = 2, lwd = 2), 
    colsep = list(col = NA)
  )
  
  table_grob_1 <- tableGrob(MIARY_STATUSOW_1, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_1, 1, 2, "core-fg")
  ind3 <- find_cell(table_grob_1, 1, 3, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  # title <- textGrob("MIARY STATUSÓW DFG", gp=gpar(fontsize=16, fontface="bold", "URW DIN-DEMI"))
  # 
  # padding <- unit(3.5,"line")
  # table_grob_1 <- gtable_add_rows(table_grob_1, 
  #                                 heights = grobHeight(title) + padding,
  #                                 pos = 0)
  # 
  # table_grob_1 <- gtable_add_grob(table_grob_1, list(title),
  #                                 t=1, l=1, 
  #                                 r=ncol(table_grob_1))
  
  plot <- plot_grid(table_grob_1, ncol = 1, scale=1)
  
  return(plot)
}