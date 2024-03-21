#' Tworzy obiekt data.frame z miarami historii zmian obiektów DFG
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i siedmioma kolumnami,
#' zawierającymi miary historii zmian obiektów DFG.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004DS_prepared_data_9.
#' @return Obiekt data.frame z miarami historii zmian obiektów DFG.
#'
#' @examples
#' DATA_RAP004DS_9(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP004DS_prepared_data_9
#' @export
DATA_RAP004DS_9 <- function(json_content) {
  df_main_2 <- getRAP004DS_prepared_data_6(json_content)

  df <- data.frame(C1 = c('liczba obiektów WPLMRP','liczba obiektów INSKLA','liczba obiektów DANEAN',
                          'liczba obiektów WSTRZD','Liczba wersji obiektów WPLMRP','Liczba wersji obiektów INSKLA',
                          'Liczba wersji obiektów DANEAN','Liczba wersji obiektów WSTRZD'),
                   C2 = c(df_main_2$L_WPLMRP, df_main_2$L_INSKLA, 
                          df_main_2$L_DEAN, df_main_2$L_WSTRZD, df_main_2$WERSJA_WPLMRP, 
                          df_main_2$WERSJA_INSKLA, df_main_2$WERSJA_DEAN, df_main_2$WERSJA_WSTRZD))
  
  return(df)
}

#' Tworzy obiekt data.frame z miarami historii zmian obiektów DFG
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i siedmioma kolumnami,
#' zawierającymi miary historii zmian obiektów DFG.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004DS_prepared_data_9.
#' @return Obiekt grob_table prezentujący historię zmian obiektów DFG.
#'
#' @examples
#' INFOGRAPHIC_RAP004DS_9(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP004DS_prepared_data_9
#' @export
INFOGRAPHIC_RAP004DS_9 <- function(json_content) {
  df_main_2 <-  getRAP004DS_prepared_data_6(json_content)
  
  df_main_2$L_WPLMRP <- format(as.numeric(df_main_2$L_WPLMRP), big.mark=" ")
  df_main_2$L_INSKLA <- format(as.numeric(df_main_2$L_INSKLA), big.mark=" ")
  df_main_2$L_DEAN <- format(as.numeric(df_main_2$L_DEAN), big.mark=" ")
  df_main_2$L_WSTRZD <- format(as.numeric(df_main_2$L_WSTRZD), big.mark=" ")
  
  MIARY_HISTORII_1 <- data.frame(ROW_1 = c(df_main_2$L_WPLMRP, 'liczba obiektów\nWPLMRP', df_main_2$WERSJA_WPLMRP, 'liczba wersji obiektów\nWPLMRP'),
                                 ROW_2 = c(df_main_2$L_INSKLA, 'liczba obiektów\nINSKLA', df_main_2$WERSJA_INSKLA, 'liczba wersji obiektów\nINSKLA'),
                                 ROW_3 = c(df_main_2$L_DEAN, 'liczba obiektów\nDANEAN', df_main_2$WERSJA_DEAN, 'liczba wersji obiektów\nDANEAN'),
                                 ROW_4 = c(df_main_2$L_WSTRZD, 'liczba obiektów\nWSTRZD', df_main_2$WERSJA_WSTRZD, 'liczba wersji obiektów\nWSTRZD'))
  
  
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
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-DEMI")
    ),
    rowsep = list(col = "red", lty = 2, lwd = 2), 
    colsep = list(col = NA)
  )
  
  table_grob_1 <- tableGrob(MIARY_HISTORII_1, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_1, 1, 2, "core-fg")
  ind3 <- find_cell(table_grob_1, 1, 3, "core-fg")
  ind4 <- find_cell(table_grob_1, 1, 4, "core-fg")
  ind5 <- find_cell(table_grob_1, 3, 2, "core-fg")
  ind6 <- find_cell(table_grob_1, 3, 3, "core-fg")
  ind7 <- find_cell(table_grob_1, 3, 4, "core-fg")
  ind8 <- find_cell(table_grob_1, 3, 1, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind5][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind6][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind7][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind8][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")

  separators <- replicate(ncol(table_grob_1) -1,
                          segmentsGrob(x1 = unit(0, "npc"), gp=gpar(col = "#B43C23", fill = "#B43C23",lty=2, lwd = 2,alpha = 1)),
                          simplify=FALSE)
  
  table_grob_1 <- gtable::gtable_add_grob(table_grob_1, grobs = separators,
                                          t = 1, b = nrow(table_grob_1), l = seq_len(ncol(table_grob_1) -1)+1)
  
  plot <- plot_grid(table_grob_1, ncol = 1, scale=1)
  
  return(plot)
}