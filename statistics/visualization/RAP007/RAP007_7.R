#' Tworzy obiekt data.frame z miarami spraw niezgodności w podziale miesięcznym
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dwoma kolumnami,
#' zawierającymi miary spraw niezgodności w podziale miesięcznym.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP007_prepared_data_6.
#' @return Obiekt data.frame z miarami spraw niezgodności w podziale miesięcznym.
#'
#' @examples
#' DATA_RAP007_7(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP007_prepared_data_6
#' @export
source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))
source(paste(getwd(), '/utils/labelUtil.R', sep=""))  

DATA_RAP007_7 <- function(json_content) {
  df_main_2 <- getRAP007_prepared_data_6(json_content)
  colnames(df_main_2) <- c("MIESIAC", "L_NIEZGODNOSCI")
  df <- data.frame(C1 = c("Liczba spraw w ujęciu miesięcznym"),
                   C2 = c(df_main_2$L_NIEZGODNOSCI))
  
  return(df_main_2)
}

#' Tworzy obiekt data.frame z miarami spraw niezgodności w podziale miesięcznym
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dwoma kolumnami,
#' zawierającymi miary spraw niezgodności w podziale miesięcznym.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP007_prepared_data_6.
#' @return Obiekt ggplot prezentujący miary spraw niezgodności w podziale miesięcznym.
#'
#' @examples
#' INFOGRAPHIC_RAP007_7(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP007_prepared_data_6
#' @export
INFOGRAPHIC_RAP007_7 <- function(json_content) {
  df_main_2 <-  getRAP007_prepared_data_6(json_content) 

  #wyznaczenie punktu środkowego
  date_values <- as.Date(paste0(df_main_2$MIESIAC, "-01"), format = "%m-%Y-%d")

  middle_date <- as.character(format(date_values[length(date_values) %/% 2 + 1], "%m-%Y"))

  plot <- if (all(df_main_2$L_NIEZGODNOSCI == 0)) { 
    
    ggplot(df_main_2, aes(x=L_NIEZGODNOSCI, y=MIESIAC)) +
      geom_bar(stat='identity', position='dodge', alpha=1, width=.8, fill= "#B43C23") +
      xlab("") + 
      ylab("") +
      theme_bw() +
      theme(axis.text.y=element_text(color = "black", family = "URWDIN-Regular"),
            axis.text.x=element_text(color = "black", family = "URWDIN-Regular"),
            axis.line = element_line(colour = "black"),
            panel.border = element_blank(),
            legend.title = element_blank(),
            plot.title = element_blank()) +
      scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(df_main_2$L_NIEZGODNOSCI)+0.1*max(df_main_2$L_NIEZGODNOSCI))) +
      theme(plot.title = element_text(family = "URW DIN-DEMI", size = 16),
            axis.text = element_text(family = "URW DIN-REGULAR", size = 12),
            legend.position="none") +
      geom_text(data = df_main_2, aes(x = 0, y=middle_date, label = "Brak danych dla wybranych parametrów filtrowania."),
                vjust = 0.5, hjust = 0.5, color = "black", family = "URW DIN-REGULAR", size = 6)
    
  } else {
    
  ggplot(df_main_2, aes(x=L_NIEZGODNOSCI, y=MIESIAC)) +
    geom_bar(stat='identity', position='dodge', alpha=1, width=.8, fill= "#B43C23") +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(axis.text.y=element_text(color = "black", family = "URWDIN-Regular"),
          axis.text.x=element_text(color = "black", family = "URWDIN-Regular"),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          legend.title = element_blank(),
          plot.title = element_blank()) +
    scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(df_main_2$L_NIEZGODNOSCI)+0.1*max(df_main_2$L_NIEZGODNOSCI))) +
    theme(plot.title = element_text(family = "URW DIN-DEMI", size = 16),
          axis.text = element_text(family = "URW DIN-REGULAR", size = 12),
          legend.position="none") +
    geom_text(aes(label = sapply(L_NIEZGODNOSCI, LABEL)),  color = "black",  family ="URWDIN-Demi", size = 5, hjust =-0.5)
  }
  
  return(plot)
}