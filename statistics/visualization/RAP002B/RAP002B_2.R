#' Tworzy obiekt data.frame z liczbą skorygowanych zasileń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dwiema kolumnami,
#' zawierającymi liczbą skorygowanych zasileń i średnią liczbą skorygowanych zasileń.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP002B_prepared_data_2.
#' @return Obiekt data.frame z liczbą skorygowanych zasileń.
#'
#' @examples
#' DATA_RAP002B_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP002B_prepared_data_2
#' @export
DATA_RAP002B_2 <- function(json_content) {
  df_main_2 <- getRAP002B_prepared_data_2(json_content)
  df_main_2$P_SKORYGOWANYCH <- percent(df_main_2$P_SKORYGOWANYCH, acuracy = 0.01)
  
  df <- data.frame(C1 = c('łączna liczba skorygowanych zasileń', 'średnia liczba skorygowanych zasileń'),
                   C2 = c(df_main_2$L_SKORYGOWANYCH, df_main_2$P_SKORYGOWANYCH))

  return(df)
}

#' Tworzy obiekt data.frame z liczbą skorygowanych zasileń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dwiema kolumnami,
#' zawierającymi liczbą skorygowanych zasileń i średnią liczbą skorygowanych zasileń.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP002B_prepared_data_2.
#' @return Obiekt ggplot z ikoną oraz pięcioma tabelami opisującymi liczbę skorygowanych zasileń.
#'
#' @examples
#' INFOGRAPHIC_RAP002B_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP002B_prepared_data_2
#' @export
INFOGRAPHIC_RAP002B_2 <- function(json_content) {
  df_main_2 <-  getRAP002B_prepared_data_2(json_content)
  
  df_main_2$L_SKORYGOWANYCH <- format(as.numeric(df_main_2$L_SKORYGOWANYCH), big.mark=" ")
  df_main_2$P_SKORYGOWANYCH <- percent(df_main_2$P_SKORYGOWANYCH, acuracy = 0.01) 
  
  URUCHOMIENIA_REGUL <- data.frame(ROW_1 = c(df_main_2$L_SKORYGOWANYCH, 'liczba\nskorygowanych zasileń'),
                                   ROW_2 = c(df_main_2$P_SKORYGOWANYCH, 'procent\nskorygowanych zasileń'))
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(18, 6), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Regular", fontsize = 15, just="left")
    ),
    rowhead = list(
      fg_params = list(fontface = "plain", fontsize = 15)
    ),
    rowsep = list(col = "red", lty = 2, lwd = 2), 
    colsep = list(col = NA)
  )
  
  table_grob_1 <- tableGrob(URUCHOMIENIA_REGUL, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_1, 1, 2, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")

  plot <- plot_grid(table_grob_1, ncol = 1, scale=1)
  
  return(plot)
}