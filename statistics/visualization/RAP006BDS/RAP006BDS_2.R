#' Tworzy obiekt data.frame z podsumowaniem zrealizowanych zasileń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dwoma kolumnami,
#' zawierającymi miary łącznej liczby zasileń błędnych i z ostrzeżeniem.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_2.
#' @return Obiekt data.frame z liczbą uruchomień zasileń.
#'
#' @examples
#' DATA_RAP006BDS_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP006BDS_prepared_data_2
#' @export
DATA_RAP006BDS_2 <- function(json_content) {
  df_main_2 <- getRAP006BDS_prepared_data_2(json_content)
  df <- data.frame(C1 = c("liczba błędów", df_main_2$W1),
                   C2 = c("liczba ostrzeżeń", df_main_2$W2))
  
  return(df)
}

#' Tworzy obiekt data.frame z podsumowaniem zrealizowanych zasileń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dwoma kolumnami,
#' zawierającymi miary łącznej liczby zasileń błędnych i z ostrzeżeniem.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_2.
#' @return Obiekt ggplot z tabelą opisującą podsumowanie uruchomień zasileń.
#'
#' @examples
#' INFOGRAPHIC_RAP006BDS_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP006BDS_prepared_data_2
#' @export

INFOGRAPHIC_RAP006BDS_2 <- function(json_content) {
  df_main <- getRAP006BDS_prepared_data_2(json_content)
  
  LABEL <- function(value) {
    if (value >= 1000) {
      amount <- format(value, format = "f", big.mark=" ")
    } else {
      amount <- format(value, format = "f", big.mark="")
    }
  }
  

PODSUMOWANIE_WALIDACJI <- data.frame(ROW_6 = c(sapply(df_main$W2, LABEL), "liczba\nostrzeżeń"),
                                     ROW_01 = c("       ", "     "),
                                     ROW_7 = c(sapply(df_main$W1, LABEL), "liczba\nbłędów"))

custom_theme <- ttheme_minimal(
  core = list(
    bg_params = list(fill = "white", col = NA),
    fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 23, just="left"),
    padding = unit(c(30, 6), "mm")
  ),
  colhead = list(
    bg_params = list(fill = "white", col = NA),
    fg_params = list(fontface = "plain", fontfamily="URWDIN-Regular", fontsize = 17, just="left")
  ),
  rowhead = list(
    fg_params = list(fontface = "plain", fontsize = 17)
  ),
  rowsep = list(col = "red", lty = 2, lwd = 2), 
  colsep = list(col = NA)
)

table_grob_3 <- tableGrob(PODSUMOWANIE_WALIDACJI, theme = custom_theme, rows = NULL, cols=NULL)

find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}
ind <- find_cell(table_grob_3, 1, 1, "core-fg")
ind2 <- find_cell(table_grob_3, 1, 3, "core-fg")
table_grob_3$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 30, fontface="bold", col = "#B43C23")
table_grob_3$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 30, fontface="bold", col = "#B43C23")


plot <- plot_grid(table_grob_3, ncol = 1, scale=1)

return(plot)
}