#' Tworzy obiekt data.frame z liczbą uruchomień reguł walidacyjnych
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dwiema kolumnami,
#' zawierającymi liczbą uruchomień reguł walidacyjnych i średnią liczbą uruchomień reguł walidacyjnych.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP002DS_prepared_data_1.
#' @return Obiekt data.frame z liczbą uruchomień reguł walidacyjnych.
#'
#' @examples
#' DATA_RAP002DS_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP002DS_prepared_data_1
#' @export

DATA_RAP002DS_1 <- function(json_content) {
  df_main_2 <- getRAP002DS_prepared_data_1(json_content)

    if (all(df_main_2 == 0)) {
    df_main_2$SR_URUCHOMIEN <- 0
  } else {
    df_main_2$SR_URUCHOMIEN <- df_main_2$L_URUCHOMIEN / df_main_2$L_ZASILEN
    df_main_2$SR_URUCHOMIEN <- format(round((df_main_2$SR_URUCHOMIEN), 1), nsmall = 2) 
  }
  
  df <- data.frame(C1 = c('łączna liczba uruchomień reguł walidacyjnych', 'średnia liczba uruchomień reguł walidacyjnych na jedno zasilenie'),
                   C2 = c(df_main_2$L_URUCHOMIEN, df_main_2$SR_URUCHOMIEN))
  
  return(df)
}

#' Tworzy obiekt data.frame z liczbą uruchomień reguł walidacyjnych
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dwiema kolumnami,
#' zawierającymi liczbą uruchomień reguł walidacyjnych i średnią liczbą uruchomień reguł walidacyjnych.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP002DS_prepared_data_1.
#' @return Obiekt ggplot z ikoną oraz pięcioma tabelami opisującymi liczbę prowadzonych MRP.
#'
#' @examples
#' INFOGRAPHIC_RAP002DS_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP002DS_prepared_data_1
#' @export
INFOGRAPHIC_RAP002DS_1 <- function(json_content) {
  df_main_2 <-  getRAP002DS_prepared_data_1(json_content)
  
  if (all(df_main_2 == 0)) {
    df_main_2$SR_URUCHOMIEN <- 0
  } else {
    df_main_2$SR_URUCHOMIEN <- df_main_2$L_URUCHOMIEN / df_main_2$L_ZASILEN
    df_main_2$SR_URUCHOMIEN <- format(round((df_main_2$SR_URUCHOMIEN), 1), nsmall = 2) 
  }
  
  df_main_2$L_URUCHOMIEN <- format(as.numeric(df_main_2$L_URUCHOMIEN), big.mark=" ")
  df_main_2$SR_URUCHOMIEN <- format(as.numeric(df_main_2$SR_URUCHOMIEN), big.mark=" ", decimal.mark = ",")
  
  URUCHOMIENIA_REGUL <- data.frame(ROW_1 = c(df_main_2$L_URUCHOMIEN, 'łączna liczba\nuruchomień\nreguł walidacyjnych'),
                                   ROW_2 = c(df_main_2$SR_URUCHOMIEN, 'średnia liczba\nuruchomień reguł walidacyjnych\nna jedno zasilenie'))
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
      padding = unit(c(18, 6), "mm")
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
  
  table_grob_1 <- tableGrob(URUCHOMIENIA_REGUL, theme = custom_theme, rows = NULL, cols=NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_1, 1, 2, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  table_grob_1$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", col = "#B43C23")
  
  plot <- plot_grid(table_grob_1, ncol = 1, scale=1)
  
  return(plot)
}