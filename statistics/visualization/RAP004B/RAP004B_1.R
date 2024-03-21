#' Tworzy obiekt data.frame z miarami mrp i umów deweloperskich
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dziesięcioma kolumnami,
#' zawierającymi miary mrp i umów deweloperskich.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_1.
#' @return Obiekt data.frame z miarami liczb i kwot składek rozliczeń.
#'
#' @examples
#' DATA_RAP004B_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP004B_prepared_data_1
#' @export
DATA_RAP004B_1 <- function(json_content) {
  df_main_2 <- getRAP004B_prepared_data_1(json_content)

  df <- data.frame(C1 = c('liczba umów MRP',
                          'liczba rachunków o statusie Aktywny',
                          'liczba rachunków o statusie Zamknięty',
                          'liczba otwartych MRP',
                          'liczba zamkniętych MRP',
                          'liczba inwestycji o rodzaju Przedsięwzięcie deweloperskie',
                          'liczba inwestycji o rodzaju Zadanie inwestycyjne',
                          'liczba wszystkich umów deweloperskich',
                          'liczba aktywnych umów deweloperskich',
                          'liczba nabywców z aktywnych umów'),
                   C2 = c(df_main_2$L_UMOW,
                          df_main_2$L_AKTYWNY,
                          df_main_2$L_ZAMKNIETY,
                          df_main_2$L_OMRP,
                          df_main_2$L_ZMRP,
                          df_main_2$L_PRZEDDEW,
                          df_main_2$L_ZADINW,
                          df_main_2$ROZW_U_DEWELOPERSKA,
                          df_main_2$UMOWA_DEWELOPERSKA,
                          df_main_2$NABYWCA))
  
  return(df)
}

#' Tworzy obiekt data.frame z miarami mrp i umów deweloperskich
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dziesięcioma kolumnami,
#' zawierającymi miary mrp i umów deweloperskich.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_1.
#' @return Obiekt grob_table z trzema tabelami opisującymi miary mrp i umów deweloperskich.
#'
#' @examples
#' INFOGRAPHIC_RAP004B_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP004B_prepared_data_1
#' @export

INFOGRAPHIC_RAP004B_1 <- function(json_content) {
  df_main_2 <-  getRAP004B_prepared_data_1(json_content)
  
  
 MIARY_MRP_1 <- data.frame(ROW_1 = c(df_main_2$L_UMOW, 'liczba\numów\nMRP'),
                          ROW_2 = c(df_main_2$L_OMRP, 'liczba\notwartych\nMRP'),
                          ROW_3 = c(df_main_2$L_ZMRP, 'liczba\nzamkniętych\nMRP'),
                          ROW_4 = c(df_main_2$L_AKTYWNY, 'liczba rachunków\no statusie\nAktywny'),
                          ROW_5 = c(df_main_2$L_ZAMKNIETY, 'liczba rachunków\no statusie\nZamknięty'))


  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-REGULAR"),
      padding = unit(c(20, 6), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-DEMI")
    ),
    rowhead = list(
      fg_params = list(fontface = "plain", fontsize = 15)
    ),
    rowsep = list(col = "red", lty = 2, lwd = 2),
    colsep = list(col = NA)
  )

table_grob_1 <- tableGrob(MIARY_MRP_1, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_1, 1, 2, "core-fg")
  ind3 <- find_cell(table_grob_1, 1, 3, "core-fg")
  ind4 <- find_cell(table_grob_1, 1, 4, "core-fg")
  ind5 <- find_cell(table_grob_1, 1, 5, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind5][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  # title <- textGrob("MIARY MRP I UMÓW DEWELOPERSKICH",gp=gpar(fontsize=16, fontface="bold", "URW DIN-DEMI"))
  # 
  # padding <- unit(3.5,"line")
  # table_grob_1 <- gtable_add_rows(table_grob_1, 
  #                                 heights = grobHeight(title) + padding,
  #                                 pos = 0)
  # 
  # table_grob_1 <- gtable_add_grob(table_grob_1, list(title),
  #                                 t=1, l=1, 
  #                                 r=ncol(table_grob_1))
  
  MIARY_MRP_2 <- data.frame(ROW_1 = c(df_main_2$ROZW_U_DEWELOPERSKA, 'liczba wszystkich\numów deweloperskich'),
                            ROW_2 = c(df_main_2$UMOWA_DEWELOPERSKA, 'liczba aktywnych\numów deweloperskich'),
                            ROW_3 = c(df_main_2$NABYWCA, 'liczba nabywców\nz aktywnych umów'))

  custom_theme_2 <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-REGULAR"),
      padding = unit(c(16, 6), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-DEMI")
    ),
    rowhead = list(
      fg_params = list(fontface = "plain", fontsize = 15)
    ),
    rowsep = list(col = "red", lty = 2, lwd = 2), 
    colsep = list(col = NA)
  )
  
  table_grob_2 <- tableGrob(MIARY_MRP_2, theme = custom_theme_2, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_2, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_2, 1, 2, "core-fg")
  ind3 <- find_cell(table_grob_2, 1, 3, "core-fg")
  table_grob_2$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  
MIARY_MRP_3 <- data.frame(ROW_1 = c(df_main_2$L_PRZEDDEW, 'liczba inwestycji\no rodzaju\nPrzedsięwzięcie deweloperskie'),
                            ROW_2 = c(df_main_2$L_ZADINW, 'liczba inwestycji\no rodzaju\nZadanie inwestycyjne'))
custom_theme_3 <- ttheme_minimal(
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
    fg_params = list(fontface = "plain", fontsize = 15)
  ),
  rowsep = list(col = "red", lty = 2, lwd = 2),
  colsep = list(col = NA)
)

table_grob_3 <- tableGrob(MIARY_MRP_3, theme = custom_theme_2, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_3, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_3, 1, 2, "core-fg")
  table_grob_3$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_3$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  plot <- plot_grid(table_grob_1, table_grob_2,  table_grob_3, ncol = 1, scale=1)
  
  return(plot)
}