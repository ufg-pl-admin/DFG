#' Tworzy obiekt data.frame z miarami spraw niezgodności
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i pięcioma kolumnami,
#' zawierającymi miary spraw niezgodności.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP007BDS_prepared_data_1.
#' @return Obiekt data.frame z miarami spraw niezgodności.
#'
#' @examples
#' DATA_RAP007DBS_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP007_prepared_data_1
#' @export
DATA_RAP007BDS_1 <- function(json_content) {
  df_main_2 <- getRAP007BDS_prepared_data_1(json_content)
  df <- data.frame(C1 = c("Liczba spraw niezgodności",
                          "Liczba spraw dla umów MRP (ZAWMRP)",
                          "Liczba spraw dla umów deweloperskich (ZAUMDE + ZMUMDE)",
                          "Liczba spraw dla rozliczeń (WPLMRP)",
                          "Liczba interakcji",
                          "Średni czas obsługi niezgodności"),
                   C2 = c(df_main_2$L_NIEZGODNOSCI,
                          df_main_2$L_MRP,
                          df_main_2$L_DEV,
                          df_main_2$L_ROZ,
                          df_main_2$L_INEGRACJI,
                          df_main_2$SR_CZAS))
  df_main_2$SR_CZAS <- format(df_main_2$SR_CZAS, decimal.mark=".")
  return(df)
}

#' Tworzy obiekt data.frame z miarami spraw niezgodności
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i pięcioma kolumnami,
#' zawierającymi miary spraw niezgodności.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP007BDS_prepared_data_1.
#' @return Obiekt grob_table prezentujący miary dla spraw niezgodności.
#'
#' @examples
#' INFOGRAPHIC_RAP007BDS_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP007BDS_prepared_data_1
#' @export
INFOGRAPHIC_RAP007BDS_1 <- function(json_content) {
  df_main_2 <-  getRAP007BDS_prepared_data_1(json_content)
  
  MIARY_SPRAW_1 <- data.frame(ROW_1 = c(df_main_2$L_NIEZGODNOSCI, "liczba\nspraw\nniezgodności"))
  
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = c(rep("bold", 2), "plain"), fontsize = 15, "URW DIN-REGULAR")
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
  
  table_grob_1 <- tableGrob(MIARY_SPRAW_1, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  # title <- textGrob("MIARY SPRAW NIEZGODNOŚCI",gp=gpar(fontsize=16, fontface="bold", "URW DIN-DEMI"))
  # 
  # padding <- unit(3.5,"line")
  # table_grob_1 <- gtable_add_rows(table_grob_1, 
  #                                 heights = grobHeight(title) + padding,
  #                                 pos = 0)
  # 
  # 
  # table_grob_1 <- gtable_add_grob(table_grob_1, list(title),
  #                                 t=1, l=1, 
  #                                 r=ncol(table_grob_1))
  
  MIARY_SPRAW_2 <- data.frame(ROW_1=c(df_main_2$L_MRP, 'liczba\nspraw niezgodności\ndla umów MRP'),
                              ROW_2=c(df_main_2$L_DEV, 'liczba\nspraw niezgodności\ndla umów deweloperskich'),
                              ROW_3=c(df_main_2$L_ROZ, 'liczba\nspraw niezgodności\ndla rozliczeń'))
  
  custom_theme_2 <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URW DIN-REGULAR")
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
  
  table_grob_2 <- tableGrob(MIARY_SPRAW_2, theme = custom_theme_2, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_2, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_2, 1, 2, "core-fg")
  ind3 <- find_cell(table_grob_2, 1, 3, "core-fg")
  # ind4 <- find_cell(table_grob_2, 3, 1, "core-fg")
  # ind5 <- find_cell(table_grob_2, 3, 2, "core-fg")
  # ind6 <- find_cell(table_grob_2, 3, 3, "core-fg")
  table_grob_2$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  # table_grob_2$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 16, fontface="bold", col = "#B43C23")
  # table_grob_2$grobs[ind5][[1]][["gp"]] <- gpar(fontsize = 16, fontface="bold", col = "#B43C23")
  # table_grob_2$grobs[ind6][[1]][["gp"]] <- gpar(fontsize = 16, fontface="bold", col = "#B43C23")
  
  MIARY_SPRAW_3 <- data.frame(ROW_1 = c(df_main_2$L_INEGRACJI, 'liczba\ninterakcji'),
                              ROW_02 = c("        ", "        "),
                              ROW_3 = c(df_main_2$SR_CZAS, 'średni czas\nobsługi niezgodności'))
  
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = c(rep("bold", 2), "plain"), fontsize = 15, "URW DIN-REGULAR")
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
  
  table_grob_3 <- tableGrob(MIARY_SPRAW_3, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
   ind <- find_cell(table_grob_3, 1, 1, "core-fg")
   ind2 <- find_cell(table_grob_3, 1, 3, "core-fg")
   table_grob_3$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
   table_grob_3$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")  

  grid.newpage()
  plot <- plot_grid(table_grob_1, table_grob_2, table_grob_3, ncol = 1, scale=1)
  
  return(plot)
}