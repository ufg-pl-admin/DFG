#' Tworzy obiekt data.frame z miarami liczb i kwot składek rozliczeń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dziesięcioma kolumnami,
#' zawierającymi miary liczb i kwot składek rozliczeń.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004_prepared_data_2.
#' @return Obiekt data.frame z miarami liczb i kwot składek rozliczeń.
#'
#' @examples
#' DATA_RAP004_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP004_prepared_data_2
#' @export

DATA_RAP004_2 <- function(json_content) {
  df_main_2 <- getRAP004_prepared_data_2(json_content)

  df <- data.frame(C1 = c('liczba wpłat', 'liczba wypłat z MRP',
                          'liczba wypłat spoza MRP','kwota wpłat', 
                          'kwota wypłat z MRP','Kwota wypłat spoza MRP', 
                          'liczba składek o statusie Rozliczona','liczba składek o statusie Nierozliczona', 
                          'liczba składek o statusie Zaległa','kwota składek o statusie Rozliczona',
                          'kwota składek o statusie Nierozliczona','kwota składek o statusie Zaległa'),
                   C2 = c(df_main_2$L_WPLAT, df_main_2$L_WYPLAT_Z, 
                          df_main_2$L_WYPLAT_SPOZA, df_main_2$KWOTA_WPLAT,
                          df_main_2$KWOTA_Z, df_main_2$KWOTA_SPOZA,
                          df_main_2$L_ROZLICZONYCH, df_main_2$L_NIEROZLICZONYCH,
                          df_main_2$L_ZALEGLYCH, df_main_2$KWOTA_ROZLICZONA, 
                          df_main_2$KWOTA_NIEROZLICZONA, df_main_2$KWOTA_ZALEGLA))
  
  return(df)
}

#' Tworzy obiekt data.frame z miarami liczb i kwot składek rozliczeń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dziesięcioma kolumnami,
#' zawierającymi miary liczb i kwot składek rozliczeń.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004_prepared_data_2.
#' @return Obiekt grob_table z trzema tabelami opisującymi miary liczb i kwot składek rozliczeń.
#'
#' @examples
#' INFOGRAPHIC_RAP004_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP004_prepared_data_2
#' @export

INFOGRAPHIC_RAP004_2 <- function(json_content) {
  df_main_2 <-  getRAP004_prepared_data_2(json_content)
  
  MIARY_ROZLICZEN_1 <- data.frame(ROW_1 = c(df_main_2$L_WPLAT, 'liczba\nwpłat', df_main_2$KWOTA_WPLAT, 'kwota\nwpłat', df_main_2$L_ROZLICZONYCH, 'liczba składek\no statusie Rozliczona', df_main_2$KWOTA_ROZLICZONA, 'kwota składek\no statusie Rozliczona'),
                                  ROW_2 = c(df_main_2$L_WYPLAT_Z, 'liczba\nwypłat z MRP', df_main_2$KWOTA_Z, 'kwota\nwypłat z MRP',df_main_2$L_NIEROZLICZONYCH, 'liczba składek\no statusie Nierozliczona', df_main_2$KWOTA_NIEROZLICZONA, 'kwota składek\no statusie Nierozliczona'),
                                  ROW_3 = c(df_main_2$L_WYPLAT_SPOZA, 'liczba\nwypłat spoza MRP', df_main_2$KWOTA_SPOZA, 'kwota\nwypłat spoza MRP',df_main_2$L_ZALEGLYCH, 'liczba składek\no statusie Zaległa', df_main_2$KWOTA_ZALEGLA, 'kwota składek\no statusie Zaległa'))
  
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-REGULAR"),
      padding = unit(c(18, 10), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-DEMI")
    ),
    rowhead = list(
      fg_params = list(fontface = "plain", fontsize = 15, "URW DIN-DEMI"),
      padding = unit(c(18, 8), "mm")
    ),
    rowsep = list(col = "red", lty = 2, lwd = 2), 
    colsep = list(col = NA)
  )
  
  table_grob_1 <- tableGrob(MIARY_ROZLICZEN_1, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_1, 1, 2, "core-fg")
  ind3 <- find_cell(table_grob_1, 1, 3, "core-fg")
  ind4 <- find_cell(table_grob_1, 3, 1, "core-fg")
  ind5 <- find_cell(table_grob_1, 3, 2, "core-fg")
  ind6 <- find_cell(table_grob_1, 3, 3, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind5][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind6][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  # title <- textGrob("MIARY DLA ROZLICZEŃ",gp=gpar(fontsize=16, fontface="bold", "URW DIN-DEMI"))
  # 
  # padding <- unit(3.5,"line")
  # table_grob_1 <- gtable_add_rows(table_grob_1, 
  #                                 heights = grobHeight(title) + padding,
  #                                 pos = 0)
  # 
  # table_grob_1 <- gtable_add_grob(table_grob_1, list(title),
  #                                 t=1, l=1, 
  #                                 r=ncol(table_grob_1))
  
  # MIARY_ROZLICZEN_2 <- data.frame(ROW_1 = c(df_main_2$L_ROZLICZONYCH, 'liczba składek\no statusie Rozliczona', df_main_2$KWOTA_ROZLICZONA, 'kwota składek\no statusie Rozliczona'),
  #                                 ROW_2 = c(df_main_2$L_NIEROZLICZONYCH, 'liczba składek\no statusie Nierozliczona', df_main_2$KWOTA_NIEROZLICZONA, 'kwota składek\no statusie Nierozliczona'),
  #                                 ROW_3 = c(df_main_2$L_ZALEGLYCH, 'liczba składek\no statusie Zaległa', df_main_2$KWOTA_ZALEGLA, 'kwota składek\no statusie Zaległa'))
  # 
  # 
  # custom_theme_2 <- ttheme_minimal(
  #   core = list(
  #     bg_params = list(fill = "white", col = NA),
  #     fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-REGULAR"),
  #     padding = unit(c(18, 6), "mm")
  #   ),
  #   colhead = list(
  #     bg_params = list(fill = "white", col = NA),
  #     fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-DEMI")
  #   ),
  #   rowhead = list(
  #     fg_params = list(fontface = "plain", fontsize = 15, "URW DIN-DEMI")
  #   ),
  #   rowsep = list(col = "red", lty = 2, lwd = 2), 
  #   colsep = list(col = NA)
  # )
  # 
  # table_grob_2 <- tableGrob(MIARY_ROZLICZEN_2, theme = custom_theme_2, rows = NULL, cols=NULL)
  # 
  # find_cell <- function(table, row, col, name="core-fg"){
  #   l <- table$layout
  #   which(l$t==row & l$l==col & l$name==name)
  # }
  ind7 <- find_cell(table_grob_1, 5, 1, "core-fg")
  ind8 <- find_cell(table_grob_1, 5, 2, "core-fg")
  ind9 <- find_cell(table_grob_1, 5, 3, "core-fg")
  ind10 <- find_cell(table_grob_1, 7, 1, "core-fg")
  ind11 <- find_cell(table_grob_1, 7, 2, "core-fg")
  ind12 <- find_cell(table_grob_1, 7, 3, "core-fg")
  table_grob_1$grobs[ind7][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind8][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind9][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind10][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind11][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind12][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")

  separators <- replicate(ncol(table_grob_1) -1,
                          segmentsGrob(x1 = unit(0, "npc"), gp=gpar(col = "#B43C23", fill = "#B43C23",lty=2, lwd = 2,alpha = 1)),
                          simplify=FALSE)

  table_grob_1 <- gtable::gtable_add_grob(table_grob_1, grobs = separators,
                                          t = 1, b = nrow(table_grob_1), l = seq_len(ncol(table_grob_1) -1)+1)
  
  # separators <- replicate(ncol(table_grob_1) - 1,
  #                         rectGrob(gp = gpar(col = "#B43C23", fill = "#B43C23", lwd = 2)),
  #                         simplify = FALSE)
  # 
  # for (i in seq_len(ncol(table_grob_1) - 1)) {
  #   table_grob_1 <- gtable::gtable_add_grob(table_grob_1, grobs = separators[[i]],
  #                                           t = 1, b = nrow(table_grob_1), l = i + 1, r = i + 1)
  # }
  
  plot <- plot_grid(table_grob_1, ncol = 1, scale=1)
  
  return(plot)
}