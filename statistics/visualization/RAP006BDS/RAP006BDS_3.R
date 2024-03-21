#' Tworzy obiekt data.frame z podsumowaniem zrealizowanych zasileń wszystkich banków
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i sześcioma kolumnami,
#' zawierającymi miary łącznej liczby zasileń wszystkich banków.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_3.
#' @return Obiekt data.frame z liczbą zrealizowanych zasileń wszystkich banków.
#'
#' @examples
#' DATA_RAP006BDS_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP006BDS_prepared_data_3
#' @export
DATA_RAP006BDS_3 <- function(json_content) {
  df_main_2 <- getRAP006BDS_prepared_data_3(json_content)

  df <- data.frame(C1 = c("łączna liczba przekazanych rekordów zasileń", "udział zasileń przyjętych",
                          "udział zasileń przyjętych z ostrzeżeniem", "udział zasileń odrzuconych",
                          "udział zasileń w weryfikacji", "udział zasileń roboczych"),
                   C2 = c(df_main_2$Z1, df_main_2$P2, df_main_2$P3,
                          df_main_2$P4, df_main_2$P5,
                          df_main_2$P6))
  
  return(df)
}

#' Tworzy obiekt data.frame z podsumowaniem zrealizowanych zasileń wszystkich banków
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i sześcioma kolumnami,
#' zawierającymi miary łącznej liczby zasileń wszystkich banków.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_3.
#' @return Obiekt ggplot z tabelą opisującą podsumowanie uruchomień zasileń wszystkich banków.
#'
#' @examples
#' INFOGRAPHIC_RAP006BDS_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP006BDS_prepared_data_3
#' @export

INFOGRAPHIC_RAP006BDS_3 <- function(json_content) {
  df_main_2 <- getRAP006BDS_prepared_data_3(json_content)
  
  
  PODSUMOWANIE_ZASILEN_3 <- data.frame(ROW_0=c(df_main_2$Z1, 'łączna liczba\n przekazanych rekordów zasileń'))
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(4, 6), "mm")
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
  
  table_grob_4 <- tableGrob(PODSUMOWANIE_ZASILEN_3, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_4, 1, 1, "core-fg")
  table_grob_4$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  # title <- textGrob("PODSUMOWANIE ZASILEŃ WSZYSTKICH BANKÓW W SYSTEMIE DFG",gp=gpar(fontsize=16, fontface="bold", "URW DIN-DEMI"))
   footnote <- textGrob("w tym:", x=0.5, hjust=0.5,
                        gp=gpar(fontsize=12, fontface="plain", "URW DIN-DEMI"))
   
   padding <- unit(3.5,"line")
  # table_grob_4 <- gtable_add_rows(table_grob_4, 
  #                                 heights = grobHeight(title) + padding,
  #                                 pos = 0)
  # 
   table_grob_4 <- gtable_add_rows(table_grob_4, 
                                   heights = grobHeight(footnote)+ padding,
                                   pos = 3)
  # 
   table_grob_4 <- gtable_add_grob(table_grob_4, list(#title, 
                                                      footnote),
                                   #t=c(1, nrow(table_grob_4)), l=c(1,2), 
                                   t=nrow(table_grob_4), l=1,
                                   r=ncol(table_grob_4))
  
  PODSUMOWANIE_ZASILEN_4 <- data.frame(ROW_1=c(df_main_2$P2, 'udział\n zasileń\n przyjętych '),
                                       ROW_2=c(df_main_2$P3, 'udział\n zasileń przyjętych\n z ostrzeżeniem '),
                                       ROW_3=c(df_main_2$P4, 'udział\n zasileń\n odrzuconych '),
                                       ROW_4=c(df_main_2$P5, 'udział\n zasileń\n w weryfikacji '),
                                       ROW_5=c(df_main_2$P6, 'udział\n zasileń\n roboczych '))
  custom_theme_2 <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(4, 6), "mm")
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
  
  table_grob_5 <- tableGrob(PODSUMOWANIE_ZASILEN_4, theme = custom_theme_2, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_5, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_5, 1, 2, "core-fg")
  ind3 <- find_cell(table_grob_5, 1, 3, "core-fg")
  ind4 <- find_cell(table_grob_5, 1, 4, "core-fg")
  ind5 <- find_cell(table_grob_5, 1, 5, "core-fg")
  
  table_grob_5$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_5$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_5$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_5$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_5$grobs[ind5][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
#  grid.newpage()
  
  separators <- replicate(ncol(table_grob_5) -1,
                          segmentsGrob(x1 = unit(0, "npc"), gp=gpar(col = "#B43C23",lty=2)),
                          simplify=FALSE)
  ## add vertical lines on the left side of columns (after 2nd)
  table_grob_5 <- gtable::gtable_add_grob(table_grob_5, grobs = separators,
                                          t = 1, b = nrow(table_grob_5), l = seq_len(ncol(table_grob_5) -1)+1)
  
  plot <- plot_grid(table_grob_4, table_grob_5, ncol = 1, scale=1)

return(plot)
}