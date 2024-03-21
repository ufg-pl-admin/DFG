#' Tworzy obiekt ggplot z tabelami opisującymi liczbę inwestycji
#'
#' Funkcja tworzy i zwraca obiekt ggplot z ikoną oraz trzema tabelami
#' opisującymi liczbę inwestycji, umieszczonymi obok siebie.
#'
#' @return Obiekt ggplot z ikoną oraz trzema tabelami opisującymi liczbę założonych MRP.
#'
#' @examples
#' INFOGRAPHIC_STAT_006_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @export
DATA_STAT_006_2 <- function(json_content) {
  
  colnames(json_content) <- c("L_INWESTYCJI_CHR","L_INWESTYCJI_CHR_ROZ","L_INWESTYCJI_CHR_ZAM","L_UMOW_CHR",
                              "SR_L_UMOW_NA_INWESTYCJE", "MEDIANA_L_UMOW", "MIN_CZAS_INWESTYCJI", "SR_CZAS_INWESTYCJI","MAX_CZAS_INWESTYCJI",
                              "SR_WARTOSC_UMOWY","MEDIANA_WARTOSC_UMOWY","SR_L_NABYWCOW")
  
  


  
  df_main <- as.data.frame(json_content)
  
  df_main$L_UMOW_CHR <- as.numeric(df_main$L_UMOW_CHR)
  df_main$SR_L_UMOW_NA_INWESTYCJE <- as.numeric(df_main$SR_L_UMOW_NA_INWESTYCJE)
  df_main$MEDIANA_L_UMOW <- as.numeric(df_main$MEDIANA_L_UMOW)
  
  
  
  df_main$L_UMOW_CHR <- formatC(df_main$L_UMOW_CHR, digits = 0, format = "f", big.mark=" ")
  df_main$SR_L_UMOW_NA_INWESTYCJE <- formatC(df_main$SR_L_UMOW_NA_INWESTYCJE, digits = 0, format = "f", big.mark=" ")
  df_main$MEDIANA_L_UMOW <- formatC(df_main$MEDIANA_L_UMOW, digits = 0, format = "f", big.mark=" ")
  df <- data.frame(
    C1 = c("liczba umów deweloperskich objętych ochroną DFG", "średnia liczba umów deweloperskich na inwestycję",
           "mediana liczby umów deweloperskich na inwestycję"),
    C2 = c(df_main$L_UMOW_CHR, df_main$SR_L_UMOW_NA_INWESTYCJE, df_main$MEDIANA_L_UMOW)
  )
  
  return(df)
}



INFOGRAPHIC_STAT_006_2 <- function(json_content) {
  
  colnames(json_content) <- c("L_INWESTYCJI_CHR","L_INWESTYCJI_CHR_ROZ","L_INWESTYCJI_CHR_ZAM","L_UMOW_CHR",
                              "SR_L_UMOW_NA_INWESTYCJE", "MEDIANA_L_UMOW", "MIN_CZAS_INWESTYCJI", "SR_CZAS_INWESTYCJI","MAX_CZAS_INWESTYCJI",
                              "SR_WARTOSC_UMOWY","MEDIANA_WARTOSC_UMOWY","SR_L_NABYWCOW")
  df_main <- as.data.frame(json_content)
  
  
  
  df_main$L_UMOW_CHR <- as.numeric(df_main$L_UMOW_CHR)
  df_main$SR_L_UMOW_NA_INWESTYCJE <- as.numeric(df_main$SR_L_UMOW_NA_INWESTYCJE)
  df_main$MEDIANA_L_UMOW <- as.numeric(df_main$MEDIANA_L_UMOW)
  
  
  
  df_main$L_UMOW_CHR <- formatC(df_main$L_UMOW_CHR, digits = 0, format = "f", big.mark=" ")
  df_main$SR_L_UMOW_NA_INWESTYCJE <- formatC(df_main$SR_L_UMOW_NA_INWESTYCJE, digits = 0, format = "f", big.mark=" ")
  df_main$MEDIANA_L_UMOW <- formatC(df_main$MEDIANA_L_UMOW, digits = 0, format = "f", big.mark=" ")
  
  
  df1 <- data.frame(var1=c(df_main$L_UMOW_CHR,"liczba\n umów deweloperskich\n objętych ochroną DFG"))
  df2 <- data.frame(var1=c(df_main$SR_L_UMOW_NA_INWESTYCJE,"średnia liczba\n umów deweloperskich\n na inwestycję"))
  df3 <- data.frame(var1=c(df_main$MEDIANA_L_UMOW,"mediana liczby\n umów deweloperskich\n na inwestycję"))
  
  
  df_list <- list(df1,df2,df3)
  
  
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 14.4, just="left"),
      padding = unit(c(0,6),"mm")
    ),
    colhead = list(
      bg_params = list(fill = "dodgerblue3", col = NA),
      fg_params = list(fontface = "bold", fontsize = 14.4, col = "white", just="left")
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
  
  # 
  ind <- find_cell(table_grob_1, 1, 1, "core-fg")
  table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_2, 1, 1, "core-fg")
  table_grob_2$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  ind <- find_cell(table_grob_3, 1, 1, "core-fg")
  table_grob_3$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
  
  
  
  grob_list <- list(table_grob_1,table_grob_2,table_grob_3)
  
  
  k <-0
  
  for (i in grob_list){
    
    k<- k+1
    
    
    assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
    
  }
  # 
  # icon_path <- paste(getwd(), '/icons/umowy.svg', sep="")
  # icon <- ggdraw() + draw_image(icon_path, scale = 0.7)
  
  
  
  
  icon_list<-list("paragraf.svg","srednia.svg","mediana.svg","kalendarz.svg","kalendarz_2.svg","umowy.svg",
                  "odchylenie_standardowe.svg","rozklad_normalny.svg")
  
  y<-0
  for (i in icon_list)
  {
    y<- y+1
    assign(paste0("icon_",y), ggdraw() + draw_image(paste(getwd(), paste('/icons/',i, sep=""), sep=""), scale = 0.9))
  }
  
  
  plot <- plot_grid(plot_1, icon_6, plot_2, icon_3, plot_3, ncol = 5, scale = 1)
  
  
  return(plot)
}






