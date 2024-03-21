#' Tworzy obiekt data.frame z miarami liczb i kwot składek rozliczeń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dziesięcioma kolumnami,
#' zawierającymi miary liczb i kwot składek rozliczeń.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_2.
#' @return Obiekt data.frame z miarami liczb i kwot składek rozliczeń.
#'
#' @examples
#' DATA_RAP004B_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP004B_prepared_data_2
#' @export
DATA_RAP004B_2 <- function(json_content) {
  df_main_2 <- getRAP004B_prepared_data_2(json_content)
  
  df <- data.frame(C1 = c('liczba wpłat',
                          'liczba wypłat z MRP',
                          'kwota wpłat',
                          'kwota wypłat z MRP',
                          'liczba składek o statusie Rozliczona',
                          'liczba składek o statusie Nierozliczona',
                          'liczba składek o statusie Zaległa',
                          'kwota składek o statusie Rozliczona',
                          'kwota składek o statusie Nierozliczona',
                          'kwota składek o statusie Zaległa'),
                   C2 = c(df_main_2$L_WPLAT,
                          df_main_2$L_WYPLAT_Z,
                          df_main_2$KWOTA_WPLAT,
                          df_main_2$KWOTA_Z,
                          df_main_2$L_ROZLICZONYCH,
                          df_main_2$L_NIEROZLICZONYCH,
                          df_main_2$L_ZALEGLYCH,
                          df_main_2$KWOTA_ROZLICZONA,
                          df_main_2$KWOTA_NIEROZLICZONA,
                          df_main_2$KWOTA_ZALEGLA))
  
  return(df)
}

#' Tworzy obiekt data.frame z miarami liczb i kwot składek rozliczeń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dziesięcioma kolumnami,
#' zawierającymi miary liczb i kwot składek rozliczeń.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_2.
#' @return Obiekt grob_table z trzema tabelami opisującymi miary liczb i kwot składek rozliczeń.
#'
#' @examples
#' INFOGRAPHIC_RAP004B_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP004B_prepared_data_2
#' @export

INFOGRAPHIC_RAP004B_2 <- function(json_content) {
  df_main_2 <-  getRAP004B_prepared_data_2(json_content)

  MIARY_ROZLICZEN_1 <- data.frame(ROW_1 = c(df_main_2$L_WPLAT, 'liczba\nwpłat',
                                            df_main_2$L_WYPLAT_Z, 'liczba\nwypłat z MRP'),
                                  ROW_2 = c(df_main_2$KWOTA_WPLAT, 'kwota\nwpłat',
                                            df_main_2$KWOTA_Z, 'kwota\nwypłat z MRP'))

  MIARY_ROZLICZEN_2 <- data.frame(ROW_1 = c(df_main_2$L_ROZLICZONYCH, 'liczba składek\no statusie Rozliczona',
                                            df_main_2$L_NIEROZLICZONYCH, 'liczba składek\no statusie Nierozliczona',
                                            df_main_2$L_ZALEGLYCH, 'liczba składek\no statusie Zaległa'),
                                  ROW_2 = c(df_main_2$KWOTA_ROZLICZONA, 'kwota składek\no statusie Rozliczona',
                                            df_main_2$KWOTA_NIEROZLICZONA, 'kwota składek\no statusie Nierozliczona',
                                            df_main_2$KWOTA_ZALEGLA, 'kwota składek\no statusie Zaległa'))

  df1 <- data.frame(var1=c(df_main_2$L_WPLAT, 'liczba\nwpłat'))
  df2 <- data.frame(var1=c(df_main_2$L_WYPLAT_Z, 'liczba\nwypłat z MRP'))

  df3 <- data.frame(var1=c(df_main_2$KWOTA_WPLAT, 'kwota
             wpłat           '))
  df4 <- data.frame(var1=c(df_main_2$KWOTA_Z, 'kwota
             wypłat z MRP           '))


  df5 <- data.frame(var1=c(df_main_2$L_ROZLICZONYCH, 'liczba składek\no statusie Rozliczona'))
  df6 <- data.frame(var1=c(df_main_2$L_NIEROZLICZONYCH, 'liczba składek\no statusie Nierozliczona'))
  df7 <- data.frame(var1=c( df_main_2$L_ZALEGLYCH, 'liczba składek\no statusie Zaległa'))

  df8 <- data.frame(var1=c(df_main_2$KWOTA_ROZLICZONA, 'kwota składek
             o statusie Rozliczona           '))
  df9 <- data.frame(var1=c(df_main_2$KWOTA_NIEROZLICZONA, 'kwota składek
             o statusie Nierozliczona           '))
  df10 <- data.frame(var1=c(df_main_2$KWOTA_ZALEGLA, 'kwota składek
             o statusie Zaległa           '))


  df_list <- list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)

  
  # custom_theme <- ttheme_minimal(
  #   core = list(
  #     bg_params = list(fill = "white", col = NA),
  #     fg_params = list(fontface = "bold", fontsize = 15, "URWDIN-Regular"),
  #     padding = unit(c(18, 10), "mm")
  #   ),
  #   colhead = list(
  #     bg_params = list(fill = "white", col = NA),
  #     fg_params = list(fontface = "bold", fontsize = 15, "URWDIN-Demi")
  #   ),
  #   rowhead = list(
  #     fg_params = list(fontface = "plain", fontsize = 15, "URWDIN-Demi"),
  #     padding = unit(c(18, 8), "mm")
  #   ),
  #   rowsep = list(col = "red", lty = 2, lwd = 2),
  #   colsep = list(col = NA)
  # )

  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(0,6),"mm")
    ),
    colhead = list(
      bg_params = list(fill = "dodgerblue3", col = NA),
      fg_params = list(fontsize = 15, col = "white", just="left")
    )

  )
  

  j <-0

  for (i in df_list){
    j<- j+1
    assign(paste0("table_grob_",j), tableGrob(i, theme = custom_theme, rows=NULL,cols=NULL))
  }

  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }

  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_2, 1, 1, "core-fg")
  table_grob_2$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_3, 1, 1, "core-fg")
  table_grob_3$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_4, 1, 1, "core-fg")
  table_grob_4$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_5, 1, 1, "core-fg")
  table_grob_5$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_6, 1, 1, "core-fg")
  table_grob_6$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_7, 1, 1, "core-fg")
  table_grob_7$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_8, 1, 1, "core-fg")
  table_grob_8$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_9, 1, 1, "core-fg")
  table_grob_9$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_10, 1, 1, "core-fg")
  table_grob_10$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")

    grob_list <- list(table_grob_1,
                    table_grob_2,
                    table_grob_3,
                    table_grob_4,
                    table_grob_5,
                    table_grob_6,
                    table_grob_7,
                    table_grob_8,
                    table_grob_9,
                    table_grob_10)

    k <- 0

  for (i in grob_list){
    k <- k + 1

    assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
  }

    grid_1 <- plot_grid(plot_1, plot_2, plot_3, plot_4 ,ncol = 2, nrow = 2)
    grid_2 <- plot_grid(plot_5, plot_6, plot_7, plot_8, plot_9, plot_10 ,ncol = 3, nrow = 2)

  separators <- replicate(ncol(table_grob_1) -1,
                          segmentsGrob(x1 = unit(0, "npc"), gp=gpar(col = "#B43C23", fill = "#B43C23",lty=2, lwd = 2,alpha = 1)),
                          simplify=FALSE)
  
  plot <- plot_grid(grid_1, grid_2, ncol=1, scale=1)
  
  return(plot)
}