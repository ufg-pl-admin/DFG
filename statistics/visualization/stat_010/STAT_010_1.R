#' Tworzy obiekt ggplot z tabelami opisującymi liczbę umów deweloperskich i inwestycji
#'
#' Funkcja tworzy i zwraca obiekt ggplot z ikoną oraz pięcioma tabelami
#' opisującymi liczbę umów deweloperskich i inwestycji, umieszczonymi obok siebie.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_010_prepared_data_1.
#' @return Obiekt ggplot z ikoną oraz dwiema tabelami opisującymi liczbę umów deweloperskich i inwestycji.
#'
#' @examples
#' INFOGRAPHIC_STAT_010_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getStat_010_prepared_data_1
#' @export

DATA_STAT_010_1 <- function(json_content) {
  colnames(json_content) <- c("NAZWA_MIARY",	"L_CHRONIONYCH")
  df_main_2 <- as.data.frame(json_content)
  df_main_2$L_CHRONIONYCH <- as.numeric(df_main_2$L_CHRONIONYCH)

  df <- data.frame(
    C1 = c("liczba inwestycji objętych ochroną DFG", "liczba umów deweloperskich objętych ochroną DFG"),
    C2 = c(df_main_2[1,2], df_main_2[2,2])
  )
  
  return(df)
}

INFOGRAPHIC_STAT_010_1 <- function(json_content) {
  df_main_2 <- getStat_010_prepared_data_1(json_content)
  
  LICZBA_CHRONIONYCH <- data.frame(ROW_1 = c(df_main_2[1,2], 'liczba inwestycji\nobjętych ochroną DFG'))
  LICZBA_UMOW <- data.frame(ROW_2 = c(df_main_2[2,2], 'liczba umów deweloperskich\nobjętych ochroną DFG'))
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain",fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(0, 6), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontfamily="URWDIN-Regular", fontsize = 15, col = "white", just="left")
    ),
    rowhead = list(
      fg_params = list(fontface = "plain", fontsize = 15)
    ),
    rowsep = list(col = "red", lty = 2, lwd = 2), 
    colsep = list(col = NA)
  )
  
  table_grob_1 <- tableGrob(LICZBA_CHRONIONYCH, theme = custom_theme, rows = NULL, cols=NULL)
  table_grob_2 <- tableGrob(LICZBA_UMOW, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_2, 1, 1, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  
  icon_path_1 <- paste(getwd(), '/icons/gwarancja-dfg.svg', sep="")
  icon_1 <- ggdraw() + draw_image(icon_path_1, scale = 0.7)
  icon_path_2 <- paste(getwd(), '/icons/umowy.svg', sep="")
  icon_2 <- ggdraw() + draw_image(icon_path_2, scale = 0.7)
  
  plot <- plot_grid(icon_1, table_grob_1, icon_2, table_grob_2, ncol = 4, rel_widths = c(0.3, 1, 0.3, 1), rel_heights = c(0.3, 1, 0.3, 1))
  
  return(plot)
}