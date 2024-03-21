#' Tworzy obiekt ggplot z tabelami opisującymi liczbę inwestycji
#'
#' Funkcja tworzy i zwraca obiekt ggplot z ikoną oraz trzema tabelami
#' opisującymi liczbę inwestycji, umieszczonymi obok siebie.
#'
#' @return Obiekt ggplot z ikoną oraz trzema tabelami opisującymi liczbę założonych MRP.
#'
#' @examples
#' INFOGRAPHIC_STAT_006_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @export
DATA_STAT_006_4 <- function(json_content) {
  
  colnames(json_content) <- c("L_INWESTYCJI_CHR","L_INWESTYCJI_CHR_ROZ","L_INWESTYCJI_CHR_ZAM","L_UMOW_CHR",
                              "SR_L_UMOW_NA_INWESTYCJE", "MEDIANA_L_UMOW", "MIN_CZAS_INWESTYCJI", "SR_CZAS_INWESTYCJI","MAX_CZAS_INWESTYCJI",
                              "SR_WARTOSC_UMOWY","MEDIANA_WARTOSC_UMOWY","SR_L_NABYWCOW")
  
  
  
  df_main <- as.data.frame(json_content)
  
  df_main$SR_WARTOSC_UMOWY <- as.numeric(df_main$SR_WARTOSC_UMOWY)
  df_main$MEDIANA_WARTOSC_UMOWY <- as.numeric(df_main$MEDIANA_WARTOSC_UMOWY)
  df_main$SR_L_NABYWCOW <- as.numeric(df_main$SR_L_NABYWCOW)
  
  convert_format <- function(value) {
    if (value >= 1000000000) {
      amount <- formatC(value / 1000000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " mld")
    } else if (value >= 1000000) {
      amount <- formatC(value / 1000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " mln")
    } else if (value >= 1000) {
      amount <- formatC(value / 1000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " tys.")
    } else {
      amount <- formatC(value, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, "")
    }
  }
  
  df_main$SR_WARTOSC_UMOWY <- sapply(df_main$SR_WARTOSC_UMOWY, convert_format)
  df_main$MEDIANA_WARTOSC_UMOWY <- sapply(df_main$MEDIANA_WARTOSC_UMOWY, convert_format)
  df_main$SR_L_NABYWCOW <- formatC(df_main$SR_L_NABYWCOW, digits = 0, format = "f", big.mark=" ")
  

  
  df <- data.frame(
    C1 = c("średnia wartość umowy deweloperskiej objętej ochroną DFG", "mediana wartości umowy deweloperskiej objętej ochroną DFG",
           "średnia liczba nabywców objętych ochroną w ramach jednej inwestycji"),
    C2 = c(df_main$SR_WARTOSC_UMOWY, df_main$MEDIANA_WARTOSC_UMOWY, df_main$SR_L_NABYWCOW)
  )
  
  return(df)
}



INFOGRAPHIC_STAT_006_4 <- function(json_content) {
  
  colnames(json_content) <- c("L_INWESTYCJI_CHR","L_INWESTYCJI_CHR_ROZ","L_INWESTYCJI_CHR_ZAM","L_UMOW_CHR",
                              "SR_L_UMOW_NA_INWESTYCJE", "MEDIANA_L_UMOW", "MIN_CZAS_INWESTYCJI", "SR_CZAS_INWESTYCJI","MAX_CZAS_INWESTYCJI",
                              "SR_WARTOSC_UMOWY","MEDIANA_WARTOSC_UMOWY","SR_L_NABYWCOW")
  df_main <- as.data.frame(json_content)
  
  
  
  df_main$SR_WARTOSC_UMOWY <- as.numeric(df_main$SR_WARTOSC_UMOWY)
  df_main$MEDIANA_WARTOSC_UMOWY <- as.numeric(df_main$MEDIANA_WARTOSC_UMOWY)
  df_main$SR_L_NABYWCOW <- as.numeric(df_main$SR_L_NABYWCOW)
  
  convert_format <- function(value) {
    if (value >= 1000000000) {
      amount <- formatC(value / 1000000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " mld")
    } else if (value >= 1000000) {
      amount <- formatC(value / 1000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " mln")
    } else if (value >= 1000) {
      amount <- formatC(value / 1000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " tys.")
    } else {
      amount <- formatC(value, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, "")
    }
  }
  
  df_main$SR_WARTOSC_UMOWY <- sapply(df_main$SR_WARTOSC_UMOWY, convert_format)
  df_main$MEDIANA_WARTOSC_UMOWY <- sapply(df_main$MEDIANA_WARTOSC_UMOWY, convert_format)
  df_main$SR_L_NABYWCOW <- formatC(df_main$SR_L_NABYWCOW, digits = 0, format = "f", big.mark=" ")
  
  df1 <- data.frame(var1=c(df_main$SR_WARTOSC_UMOWY,"średnia wartość\n umowy deweloperskiej\n objętej ochroną DFG"))
  df2 <- data.frame(var1=c(df_main$MEDIANA_WARTOSC_UMOWY,"mediana wartości\n umowy deweloperskiej\n objętej ochroną DFG"))
  df3 <- data.frame(var1=c(df_main$SR_L_NABYWCOW,"średnia liczba nabywców\n objętych ochroną\n w ramach jednej inwestycji"))
  
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
                  "monety_czerwone.svg","para_ludzi.svg")
  
  y<-0
  for (i in icon_list)
  {
    y<- y+1
    assign(paste0("icon_",y), ggdraw() + draw_image(paste(getwd(), paste('/icons/',i, sep=""), sep=""), scale = 0.9))
  }
  
  
  plot <- plot_grid(plot_1, icon_7, plot_2, icon_8, plot_3, ncol = 5, scale = 1)
  
  
  return(plot)
}






