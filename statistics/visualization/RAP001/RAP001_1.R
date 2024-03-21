#' Tworzy obiekt data.frame z miarami deweloperskich teczek rozliczeń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dwunastoma kolumnami,
#' zawierającymi miary deweloperskich teczek rozliczeń.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP001_prepared_data_1.
#' @return Obiekt data.frame z miarami deweloperskich teczek rozliczeń.
#'
#' @examples
#' DATA_RAP001_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP001_prepared_data_1
#' @export

DATA_RAP001_1 <- function(json_content) {
  df_main <- getRAP001_prepared_data_1(json_content) 
 
 df <- data.frame(C1 = c("Liczba Deweloperskich Teczek Roszczeń", "Suma należności wymaganych",
                         "Suma należności aktualnych", "Liczba należności głównych - z wypłat",
                         "Liczba należności przedawnionych", "Liczba należności umorzonych",
                         "Liczba należności anulowanych", "Liczba nalezności rozliczonych", "Czas obsługi sprawy regresowej (od założenia sprawy do rozliczenia/umorzenia)",
                         "Liczba spraw reg. Dobrowolnego", "Liczba spraw reg. Sądowego", "Liczba spraw reg. Egzekucyjnego"),
                  C2 = c(df_main$NR_DTR, df_main$SUMA_WYMAGANYCH, df_main$SUMA_AKTUALNYCH, df_main$L_GLOWNYCH, df_main$L_PRZEDAWNIONYCH, df_main$L_UMORZONYCH,
                         df_main$L_ANULOWANYCH, df_main$L_ROZLICZONYCH, df_main$DNI, df_main$DOBROWOLNE, df_main$SADOWE, df_main$EGZEKUCYJNE))
 
  return(df)
}

#' Tworzy obiekt data.frame z miarami deweloperskich teczek rozliczeń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dwunastoma kolumnami,
#' zawierającymi miary deweloperskich teczek rozliczeń.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP001_prepared_data_1.
#' @return Obiekt ggplot z ikoną oraz pięcioma tabelami opisującymi miary deweloperskich teczek rozliczeń.
#'
#' @examples
#' INFOGRAPHIC_RAP001_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP001_prepared_data_1
#' @export

INFOGRAPHIC_RAP001_1 <- function(json_content) {
  df_main_2 <-  getRAP001_prepared_data_1(json_content)

  
  MIARY_DTR_1 <- data.frame(#ROW_01 = c("                       ", "                       "),
                              ROW_1 = c(df_main_2$NR_DTR, "Liczba\nDeweloperskich\nTeczek Roszczeń"))
                              #ROW_02 = c("                       ", "                       "))
  
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
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
  
  table_grob_1 <- tableGrob(MIARY_DTR_1, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")

  MIARY_DTR_2 <- data.frame(ROW_1=c(df_main_2$SUMA_WYMAGANYCH, 'suma\nnależności\nwymaganych'),
                              ROW_2=c(df_main_2$SUMA_AKTUALNYCH, 'suma\nnależności\naktualnych'))
  
  custom_theme_2 <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
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
  
  table_grob_2 <- tableGrob(MIARY_DTR_2, theme = custom_theme_2, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_2, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_2, 1, 2, "core-fg")
  # ind3 <- find_cell(table_grob_2, 1, 3, "core-fg")
  # ind4 <- find_cell(table_grob_2, 3, 1, "core-fg")
  # ind5 <- find_cell(table_grob_2, 3, 2, "core-fg")
  # ind6 <- find_cell(table_grob_2, 3, 3, "core-fg")
  table_grob_2$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  # table_grob_2$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 16, fontface="bold", col = "#B43C23")
  # table_grob_2$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 16, fontface="bold", col = "#B43C23")
  # table_grob_2$grobs[ind5][[1]][["gp"]] <- gpar(fontsize = 16, fontface="bold", col = "#B43C23")
  # table_grob_2$grobs[ind6][[1]][["gp"]] <- gpar(fontsize = 16, fontface="bold", col = "#B43C23")
  
  MIARY_DTR_3 <- data.frame(ROW_1 = c(df_main_2$L_GLOWNYCH, 'liczba\nnależności głównych\n-z wypłat'),
                            ROW_2 = c(df_main_2$L_PRZEDAWNIONYCH, 'liczba\nnależności\nprzedawnionych'),
                            ROW_3 = c(df_main_2$L_UMORZONYCH, 'liczba\nnależności\numorzonych'),
                            ROW_4 = c(df_main_2$L_ANULOWANYCH, 'liczba\nnależności\nanulowanych'),
                            ROW_5 = c(df_main_2$L_ROZLICZONYCH, 'liczba\nnależności\nrozliczonych'))
  
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
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
  
  table_grob_3 <- tableGrob(MIARY_DTR_3, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_3, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_3, 1, 2, "core-fg")
  ind3 <- find_cell(table_grob_3, 1, 3, "core-fg")
  ind4 <- find_cell(table_grob_3, 1, 4, "core-fg")
  ind5 <- find_cell(table_grob_3, 1, 5, "core-fg")
  table_grob_3$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_3$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")  
  table_grob_3$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_3$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_3$grobs[ind5][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  MIARY_DTR_4 <- data.frame(#ROW_01 = c("                       ", "                       "),
                            ROW_1 = c(df_main_2$DNI, "czas obsługi sprawy regresowej\n(od założenia sprawy do rozliczenia/umorzenia)"))
                           # ROW_02 = c("                       ", "                       "))
  
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
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
  
  table_grob_4 <- tableGrob(MIARY_DTR_4, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_4, 1, 1, "core-fg")
  table_grob_4$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  MIARY_DTR_5 <- data.frame(ROW_1=c(df_main_2$DOBROWOLNE, 'liczba\nspraw\nreg. Dobrowolnego'),
                            ROW_2=c(df_main_2$SADOWE, 'liczba\nspraw\nreg. Sądowego'),
                            ROW_3=c(df_main_2$EGZEKUCYJNE, 'liczba\nspraw\nreg. Egzekucyjnego'))
  
  custom_theme_2 <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
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
  
  table_grob_5 <- tableGrob(MIARY_DTR_5, theme = custom_theme_2, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob_5, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_5, 1, 2, "core-fg")
  ind3 <- find_cell(table_grob_5, 1, 3, "core-fg")
  table_grob_5$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_5$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_5$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")

  
  plot <- plot_grid(table_grob_1, table_grob_2, table_grob_3, table_grob_4, table_grob_5, ncol = 1, scale=1)
  
  return(plot)
}