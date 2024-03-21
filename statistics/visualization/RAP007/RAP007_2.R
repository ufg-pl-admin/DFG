#' Tworzy obiekt data.frame z miarami spraw niezgodności w podziale na statusy
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i siedmioma kolumnami,
#' zawierającymi miary spraw niezgodności w podziale na statusy.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP007_prepared_data_2.
#' @return Obiekt data.frame z miarami spraw niezgodności w podziale na statusy.
#'
#' @examples
#' DATA_RAP007_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP007_prepared_data_2
#' @export
DATA_RAP007_2 <- function(json_content) {
  df_main_2 <- getRAP007_prepared_data_2(json_content)
  df <- data.frame(C1 = c("Liczba spraw niezgodności o statusie W weryfikacji",
                          "Liczba spraw niezgodności o statusie Rozwiązana",
                          "Liczba spraw niezgodności o statusie Odrzucona",
                          "Liczba spraw niezgodności o statusie Doprecyzowana",
                          "Liczba spraw niezgodności o statusie Do doprecyzowania",
                          "Liczba spraw niezgodności o statusie Reklamacja",
                          "Liczba spraw niezgodności o statusie Zamknięta", 
                          "Liczba spraw niezgodności o statusie Zamknięta automatycznie"),
                   C2 = c(df_main_2$L_WWER, 
                          df_main_2$L_ROZW, 
                          df_main_2$L_ODRZ,
                          df_main_2$L_DOPR,
                          df_main_2$L_DODOPR,

                          df_main_2$L_REKL, 
                          df_main_2$L_ZAMK, 
                          df_main_2$L_AUTZAMK))
  
  return(df)
}

#' Tworzy obiekt data.frame z miarami spraw niezgodności w podziale na statusy
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i siedmioma kolumnami,
#' zawierającymi miary spraw niezgodności w podziale na statusy.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP007_prepared_data_2.
#' @return Obiekt grob_table prezentujący miary spraw niezgodności w podziale na statusy.
#'
#' @examples
#' INFOGRAPHIC_RAP007_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP007_prepared_data_2
#' @export
INFOGRAPHIC_RAP007_2 <- function(json_content) {
  df_main_2 <-  getRAP007_prepared_data_2(json_content)
  
  
  MIARY_SPRAW_1 <- data.frame(ROW_1=c(df_main_2$L_WWER, 'Liczba\nspraw niezgodności\no statusie W weryfikacji'),
                              ROW_2=c(df_main_2$L_ROZW, 'Liczba\nspraw niezgodności\no statusie Rozwiązana'),
                              ROW_3=c(df_main_2$L_ODRZ, 'Liczba\nspraw niezgodności\no statusie Odrzucona'),
                              ROW_4=c(df_main_2$L_DODOPR, 'Liczba\nspraw niezgodności\no statusie Do doprecyzowania'))
  
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = c(rep("bold", 2), "plain"), fontsize = 15, "URWDIN-Regular"),
      padding = unit(c(8, 6), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URWDIN-Demi")
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
  ind2 <- find_cell(table_grob_1, 1, 2, "core-fg")
  ind3 <- find_cell(table_grob_1, 1, 3, "core-fg")
  ind4 <- find_cell(table_grob_1, 1, 4, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  # title <- textGrob("MIARY SPRAW NIEZGODNOSCI PER STATUS",gp=gpar(fontsize=16, fontface="bold", "URWDIN-Demi"))
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
  
  MIARY_SPRAW_2 <- data.frame(ROW_1=c(df_main_2$L_DOPR, 'Liczba\nspraw niezgodności\no statusie Doprecyzowana'),
                              ROW_2=c(df_main_2$L_REKL, 'Liczba\nspraw niezgodności\no statusie Reklamacja'),
                              ROW_3=c(df_main_2$L_ZAMK, 'Liczba\nspraw niezgodności\no statusie Zamknięta'),
                              ROW_4=c(df_main_2$L_AUTZAMK, 'Liczba\nspraw niezgodności\no statusie Zamknięta automatycznie'))
  
  
  custom_theme_2 <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URWDIN-Regular"),
      padding = unit(c(8, 6), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 15, "URWDIN-Demi")
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
  ind4 <- find_cell(table_grob_2, 1, 4, "core-fg")
  table_grob_2$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")

  
  grid.newpage()
  plot <- plot_grid(table_grob_1, table_grob_2, ncol = 1, scale=1)
  
  return(plot)
}