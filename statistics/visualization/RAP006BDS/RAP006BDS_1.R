#' Tworzy obiekt data.frame z podsumowaniem zrealizowanych zasileń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i jedenastoma kolumnami,
#' zawierającymi miary łącznej liczby zasileń podmiotu, zasileń w podziale na statusy i ich stosunek do głównej liczby zasileń.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_1.
#' @return Obiekt data.frame z liczbą uruchomień zasileń.
#'
#' @examples
#' DATA_RAP006BDS_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP006BDS_prepared_data_1
#' @export
DATA_RAP006BDS_1 <- function(json_content) {
  df_main_2 <- getRAP006BDS_prepared_data_1(json_content)
  df <- data.frame(C1 = c("łączna liczba przekazanych rekordów zasileń", "liczba zasileń przyjętych",
                          "udział zasileń przyjętych", "łączna liczba zasileń przyjętych z ostrzeżeniem",
                          "udział zasileń przyjętych z ostrzeżeniem", "łączna liczba zasileń odrzuconych",
                          "udział zasileń odrzuconych", "łączna liczba zasileń w weryfikacji", "udział zasileń w weryfikacji",
                          "łączna liczba zasileń roboczych", "udział zasileń roboczych"),
                  C2 = c(df_main_2$L1, df_main_2$L2, df_main_2$LP2, df_main_2$L3, df_main_2$LP3,
                        df_main_2$L4, df_main_2$LP4, df_main_2$L5, df_main_2$LP5, df_main_2$L6, df_main_2$LP6))

  return(df)
}

#' Tworzy obiekt data.frame z podsumowaniem zrealizowanych zasileń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i jedenastoma kolumnami,
#' zawierającymi miary łącznej liczby zasileń podmiotu, zasileń w podziale na statusy i ich stosunek do głównej liczby zasileń.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_1.
#' @return Obiekt ggplot z dwoma tabelami opisującymi podsumowanie uruchomień zasileń.
#'
#' @examples
#' INFOGRAPHIC_RAP006BDS_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP006BDS_prepared_data_1
#' @export

INFOGRAPHIC_RAP006BDS_1 <- function(json_content) {
  df_main_2 <-  getRAP006BDS_prepared_data_1(json_content)
  
  PODSUMOWANIE_ZASILEN_1 <- data.frame(ROW_0 = c(df_main_2$L1, "łączna liczba\n przekazanych rekordów zasileń"))
  
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(0, 6), "mm")
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

  table_grob_1 <- tableGrob(PODSUMOWANIE_ZASILEN_1, theme = custom_theme, rows = NULL, cols=NULL)

  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")

  PODSUMOWANIE_ZASILEN_2 <- data.frame(ROW_1=c(df_main_2$L2, 'liczba\nzasileń\nprzyjętych', df_main_2$LP2, 'udział\nzasileń\nprzyjętych'),
                                       ROW_2=c(df_main_2$L3, 'liczba\nzasileń przyjętych\nz ostrzeżeniem', df_main_2$LP3, 'udział\nzasileń przyjętych\nz ostrzeżeniem'),
                                       ROW_3=c(df_main_2$L4, 'liczba\nzasileń\nodrzuconych', df_main_2$LP4, 'udział\nzasileń\nodrzuconych'),
                                       ROW_4=c(df_main_2$L5, 'liczba\nzasileń\nw weryfikacji', df_main_2$LP5, 'udział\nzasileń\nw weryfikacji'),
                                       ROW_5=c(df_main_2$L6, 'liczba\nzasileń\nroboczych', df_main_2$LP6, 'udział\nzasileń\nroboczych'))

  custom_theme_2 <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(8, 6), "mm")
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
  
  table_grob_2 <- tableGrob(PODSUMOWANIE_ZASILEN_2, theme = custom_theme_2, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_2, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_2, 1, 2, "core-fg")
  ind3 <- find_cell(table_grob_2, 1, 3, "core-fg")
  ind4 <- find_cell(table_grob_2, 1, 4, "core-fg")
  ind5 <- find_cell(table_grob_2, 1, 5, "core-fg")
  ind6 <- find_cell(table_grob_2, 3, 1, "core-fg")
  ind7 <- find_cell(table_grob_2, 3, 2, "core-fg")
  ind8 <- find_cell(table_grob_2, 3, 3, "core-fg")
  ind9 <- find_cell(table_grob_2, 3, 4, "core-fg")
  ind10 <- find_cell(table_grob_2, 3, 5, "core-fg")
  table_grob_2$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind5][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind6][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind7][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind8][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind9][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind10][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")

  separators <- replicate(ncol(table_grob_2) -1,
                          segmentsGrob(x1 = unit(0, "npc"), gp=gpar(col = "#B43C23",lty=2)),
                          simplify=FALSE)
  ## add vertical lines on the left side of columns (after 2nd)
table_grob_2 <- gtable::gtable_add_grob(table_grob_2, grobs = separators,
                               t = 1, b = nrow(table_grob_2), l = seq_len(ncol(table_grob_2) -1)+1)
  
#  grid.newpage()
  plot <- plot_grid(table_grob_1, table_grob_2, ncol = 1, scale=1)

return(plot)
}