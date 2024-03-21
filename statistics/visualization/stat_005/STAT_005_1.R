#' getStat_005_prepared_data
#'
#' @description
#' Funkcja `getStat_005_prepared_data` przygotowuje dane o liczbie nabywców chronionych przez DFG
#' w postaci obiektu `data.frame`.
#'
#' @usage
#' getStat_005_prepared_data(json_content)
#'
#' @param json_content Obiekt JSON zawierający dane o liczbie nabywców chronionych przez DFG.
#'
#' @return
#' Funkcja zwraca obiekt `data.frame`, w którym nazwy kolumn odpowiadają nazwom kolumn w pliku źródłowym.
#' Wszystkie kolumny liczbowe zostają skonwertowane do formatu numerycznego.
#'
#' @examples
#' data <- '{"RODZAJ_MRP": "POS", "MIESIAC": "2021-01", "WOJEWODZTWO": "MAZOWIECKIE", "OCHRONA_W_OKRESIE": "2900",
#' '"OCHRONA_ROZPOCZETA": "20700", "OCHRONA_ZAKONCZONA": "72"}'
#' json_content <- jsonlite::fromJSON(data)
#' getStat_005_prepared_data(json_content)
#'
#' @details
#' Funkcja nadaje kolumnom w obiekcie JSON nowe nazwy, odpowiadające nazwom kolumn w obiekcie `data.frame`,
#' który zostanie utworzony. Konwertuje obiekt JSON na `data.frame` przy użyciu funkcji `as.data.frame`.
#' Konwertuje trzy kolumny `LICZBA_NABYWCOW`, `OCHRONA_W_OKRESIE`,`OCHRONA_ROZPOCZETA` oraz `OCHRONA_ZAKONCZONA` do formatu numerycznego przy użyciu
#' funkcji `as.numeric`.

getStat_005_prepared_data_1 <- function(json_content) {
  colnames(json_content) <- c("LICZBA_NABYWCOW","OCHRONA_W_OKRESIE","OCHRONA_ROZPOCZETA","OCHRONA_ZAKONCZONA")
  df_main <- as.data.frame(json_content)
  df_main$LICZBA_NABYWCOW <- format(as.numeric(df_main$LICZBA_NABYWCOW), big.mark=" ")
  df_main$OCHRONA_W_OKRESIE <- format(as.numeric(df_main$OCHRONA_W_OKRESIE), big.mark=" ")
  df_main$OCHRONA_ROZPOCZETA <- format(as.numeric(df_main$OCHRONA_ROZPOCZETA), big.mark=" ")
  df_main$OCHRONA_ZAKONCZONA <- format(as.numeric(df_main$OCHRONA_ZAKONCZONA), big.mark=" ")
  return(df_main)
}

#' Tworzy obiekt ggplot z tabelami opisującymi liczbę nabywców chronionych przez DFG
#'
#' Funkcja tworzy i zwraca obiekt ggplot z ikoną oraz czterema tabelami
#' opisującymi liczbę nabywców chronionych przez DFG, umieszczonymi obok siebie.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_005_prepared_data_1.
#' @return Obiekt ggplot z ikoną oraz pięcioma tabelami opisującymi liczbę założonych MRP.
#'
#' @examples
#' INFOGRAPHIC_STAT_005_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getStat_005_prepared_data_1
#' @export
DATA_STAT_005_1 <- function(json_content) {
  df_main_2 <- getStat_005_prepared_data_1(json_content)
  df <- data.frame(
    C1 = c("liczba nabywców chronionych na koniec wybranego okresu", "liczba nabywców chronionych w wybranym okresie",
           "liczba nabywców, dla których rozpoczęto ochronę","liczba nabywców, dla których zakończono ochronę"),
    C2 = c(df_main_2$LICZBA_NABYWCOW, df_main_2$OCHRONA_W_OKRESIE, df_main_2$OCHRONA_ROZPOCZETA,df_main_2$OCHRONA_ZAKONCZONA)
  )
  
  return(df)
}



INFOGRAPHIC_STAT_005_1 <- function(json_content) {
  df_main_2 <- getStat_005_prepared_data_1(json_content)
  
  df1 <- data.frame(var1=c(df_main_2$LICZBA_NABYWCOW,"liczba nabywców
chronionych na koniec
  wybranego okresu"))
  df2 <- data.frame(var1=c(df_main_2$OCHRONA_W_OKRESIE,"liczba nabywców
chronionych w wybranym
okresie"))
  df3 <- data.frame(var1=c(df_main_2$OCHRONA_ROZPOCZETA,"liczba nabywców, 
dla których
rozpoczęto ochronę"))
  df4 <- data.frame(var1=c(df_main_2$OCHRONA_ZAKONCZONA,"liczba nabywców,
dla których
zakończono ochronę"))
  
  
  df_list <- list(df1,df2)
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 12, just="left"),
      padding = unit(c(0,6),"mm")
    ),
    colhead = list(
      bg_params = list(fill = "dodgerblue3", col = NA),
      fg_params = list(fontsize = 12, col = "white", just="left")
    )
    
  )
  custom_theme_2 <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 12, just="left"),
      padding = unit(c(0,6),"mm")
    ),
    colhead = list(
      bg_params = list(fill = "dodgerblue3", col = NA),
      fg_params = list(fontsize = 12, col = "white", just="left")
    )
    
    
  )
  j <-0
  
  for (i in df_list){
    
    j<- j+1
    assign(paste0("table_grob_",j), tableGrob(i, theme = custom_theme, rows=NULL,cols=NULL))
    
    
  }
  
  
  df_list <- list(df3,df4)
  
  j <-2
  
  for (i in df_list){
    
    j<- j+1
    assign(paste0("table_grob_",j), tableGrob(i, theme = custom_theme_2, rows=NULL,cols=NULL))
    
    
  }
  
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  ind2 <- find_cell(table_grob_2, 1, 1, "core-fg")
  ind3 <- find_cell(table_grob_3, 1, 1, "core-fg")
  ind4 <- find_cell(table_grob_4, 1, 1, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_2$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_3$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  table_grob_4$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")

  
  grob_list <- list(table_grob_1,table_grob_2,table_grob_3,table_grob_4)
  
  
  k <-0
  
  for (i in grob_list){
    
    k<- k+1
    assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
    
  }
  
  icon_path_1 <- paste(getwd(), '/icons/grupa_ludzi.svg', sep="")
  icon_1 <- ggdraw() + draw_image(icon_path_1, scale = 1.8)
  
  icon_path_2 <- paste(getwd(), '/icons/gwarancja-dfg.svg', sep="")#plus
  icon_2 <- ggdraw() + draw_image(icon_path_2, scale = 0.8)
  
  icon_path_3 <- paste(getwd(), '/icons/klucze-do-mieszkania.svg', sep="")#minus
  icon_3 <- ggdraw() + draw_image(icon_path_3, scale = 0.8)
  
  
  
  
  # grid_1 <- plot_grid(plot_1, icon_1, plot_2, ncol = 3, scale=1, rel_widths = c(1.3,0.4,1.2))
  # grid_2 <- plot_grid(icon_2, plot_3, ncol = 2, rel_widths = c(1,2))
  # grid_3 <- plot_grid(icon_3, plot_4, ncol = 2, rel_widths = c(1,2))
  # grid_4 <- plot_grid(grid_2,grid_3, ncol=1, scale=1)
  #
  # plot <- plot_grid(grid_1,grid_4,ncol=2,rel_widths = c(2.3,1))

  grid_1 <- plot_grid(plot_1, icon_1, plot_2, ncol = 3, scale=1, rel_widths = c(1.3,0.4,1.3))
  grid_2 <- plot_grid(plot_3, icon_2, plot_4, ncol = 3, scale = 1, rel_widths = c(1.3,0.4,1.3))
  # grid_3 <- plot_grid(icon_3, plot_4, ncol = 2, rel_widths = c(1,2))
  # grid_4 <- plot_grid(grid_1,grid_2, ncol=1, nrow=2, scale=1)

  plot <- plot_grid(grid_1,grid_2,nrow=2,rel_widths = c(1,1))

  
  
  return(plot)
}