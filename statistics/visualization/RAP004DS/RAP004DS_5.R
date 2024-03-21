#' Tworzy obiekt data.frame z miarami kompletności danych w podziale na statusy dla umów deweloperskich
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i pięcioma kolumnami,
#' zawierającymi miary kompletności danych w podziale na statusy dla umów deweloperskich.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004DS_prepared_data_5.
#' @return Obiekt data.frame z miarami kompletności danych w podziale na statusy dla umów deweloperskich.
#'
#' @examples
#' DATA_RAP004DS_5(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP004DS_prepared_data_5
#' @export
DATA_RAP004DS_5 <- function(json_content) {
  df_main_2 <- getRAP004DS_prepared_data_4(json_content)

  df <- data.frame(C1 = c('liczba umów deweloperskich o statusie Kompletne',
                          'liczba umów deweloperskich o statusie Brak danych od Dewelopera', 
                          'liczba umów deweloperskich o statusie Brak danych od Banku',
                          'liczba umów deweloperskich o statusie Oczekuje na dane od Dewelopera',
                          'liczba umów deweloperskich o statusie Oczekuje na dane od Banku'),
                   C2 = c(df_main_2$L_KOMPLETNE, df_main_2$L_BRAKDEWELOPER, 
                          df_main_2$L_BRAKBANK, df_main_2$L_OCZEKUJEDEWELOPER, df_main_2$L_OCZEKUJEBANK))
  return(df)
}

#' Tworzy obiekt data.frame z miarami kompletności danych w podziale na statusy dla umów deweloperskich
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i pięcioma kolumnami,
#' zawierającymi miary kompletności danych w podziale na statusy dla umów deweloperskich.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004DS_prepared_data_5.
#' @return Obiekt grob_table z pięcioma tabelami kompletność danych dla umów deweloperskich.
#'
#' @examples
#' INFOGRAPHIC_RAP004DS_5(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP004DS_prepared_data_5
#' @export
INFOGRAPHIC_RAP004DS_5 <- function(json_content) {
  df_main_2 <-  getRAP004DS_prepared_data_4(json_content)
  
  MIARY_KOMPLETNOSCI_1 <- data.frame(ROW_1 = c(df_main_2$L_KOMPLETNE, 'liczba\numów deweloperskich o statusie\nKompletne'),
                                     ROW_2 = c(df_main_2$L_BRAKDEWELOPER, 'liczba\numów deweloperskich o statusie\nBrak danych od Dewelopera'),
                                     ROW_3 = c(df_main_2$L_BRAKBANK, 'liczba\numów deweloperskich o statusie\nBrak danych od Banku'))
  
  custom_theme_1 <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-REGULAR"),
      padding = unit(c(12, 6), "mm")
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
  
  table_grob_1 <- tableGrob(MIARY_KOMPLETNOSCI_1, theme = custom_theme_1, rows = NULL, cols=NULL)
  
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
  
  # title <- textGrob("MIARY DLA KOMPLETNOŚCI ZASILEŃ W ZAKRESIE DANYCH O UMOWACH DEWELOPERSKICH",gp=gpar(fontsize=16, fontface="bold", "URW DIN-DEMI"))
  # 
  # padding <- unit(3.5,"line")
  # table_grob_1 <- gtable_add_rows(table_grob_1, 
  #                                 heights = grobHeight(title) + padding,
  #                                 pos = 0)
  # 
  # table_grob_1 <- gtable_add_grob(table_grob_1, list(title),
  #                                 t=1, l=1, 
  #                                 r=ncol(table_grob_1))
  
  MIARY_KOMPLETNOSCI_2 <- data.frame(ROW_1 = c(df_main_2$L_OCZEKUJEDEWELOPER, 'liczba\numów deweloperskich o statusie\nOczekuje na dane od Dewelopera'),
                                     ROW_2 = c(df_main_2$L_OCZEKUJEBANK, 'liczba\numów deweloperskich o statusie\nOczekuje na dane od Banku'))
  custom_theme_2 <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-REGULAR"),
      padding = unit(c(12, 6), "mm")
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
  
  table_grob_2 <- tableGrob(MIARY_KOMPLETNOSCI_2, theme = custom_theme_2, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_2, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_2, 1, 2, "core-fg")
  table_grob_2$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  plot <- plot_grid(table_grob_1, table_grob_2, ncol = 1, scale=1)
  
  return(plot)
}