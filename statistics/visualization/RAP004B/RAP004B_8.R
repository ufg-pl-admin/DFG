#' Tworzy obiekt data.frame z miarami inwestycji w podziale na statusy DFG
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i czterema kolumnami,
#' zawierającymi miary inwestycji w podziale na statusy DFG.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_8.
#' @return Obiekt data.frame z miarami inwestycji w podziale na statusy DFG.
#'
#' @examples
#' DATA_RAP004B_8(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP004B_prepared_data_8
#' @export
source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))
source(paste(getwd(), '/utils/labelUtil.R', sep=""))  

DATA_RAP004B_8 <- function(json_content) {
  df_main_2 <- getRAP004B_prepared_data_5(json_content)
  
  df <- data.frame(C1 = c('Brak zasilenia',
                          'Brak zwrotu środków',
                          'Wstrzymanie wypłat'),
                   C2 = c(df_main_2$L_BRAKZASILENIA,
                          df_main_2$L_BRAKZWROTU,
                          df_main_2$L_WSTRZBANK))
  
  
  return(df)
}

#' Tworzy obiekt data.frame z miarami inwestycji w podziale na statusy DFG
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i czterema kolumnami,
#' zawierającymi miary inwestycji w podziale na statusy DFG.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_8.
#' @return Obiekt ggplot prezentujący liczbę inwestycji w podziale na statusy DFG.
#'
#' @examples
#' INFOGRAPHIC_RAP004B_8(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP004B_prepared_data_8
#' @export
INFOGRAPHIC_RAP004B_8 <- function(json_content) {
  df_main_2 <-  getRAP004B_prepared_data_5(json_content)

  df <- setNames(data.frame(t(as.matrix(df_main_2))), as.character(df_main_2[[1]]))
  df <- tibble::rownames_to_column(df, "STATUS")
  
  colnames(df) <- c("STATUS", "LICZBA")
  
  plot <- if(length(json_content) == 0) { 
    
    ggplot(df, aes(x=STATUS, y=LICZBA)) +
      geom_bar(stat='identity', position='dodge', show.legend = FALSE, width = 0.3) +
      theme_bw() +
      theme(panel.border = element_blank(),
            legend.title = element_blank(),
            axis.line = element_line(colour = "grey"),
            plot.title = element_blank()) +
      ylab(" ") +
      xlab(" ") +
      scale_x_discrete(labels= c('Brak zasilenia', 'Brak zwrotu środków', 'Wstrzymanie wypłat'))+#, expand = c(0, 0)) +
      theme(plot.title = element_blank()) +
      scale_y_continuous(expand = c(0, 0),
                         labels = function(x) format(x, big.mark = " ")) +
      theme(axis.text.y=element_text(color = "black", family = "URWDIN-Regular"),
            axis.text.x=element_text(color = "black", family = "URWDIN-Regular"),
            axis.title.y=element_text(angle = 0, face="bold", size=15),
            axis.title.x=element_text(hjust=1, face="bold", size=15)) +
      geom_text(aes(x=2.5, y=0, label = "Brak danych dla wybranych parametrów filtrowania."),
                vjust = 0.5, hjust = 0.5, color = "black", family = "URW DIN-REGULAR", size = 5)
    
  } else {
    
    ggplot(df, aes(x=STATUS, y=LICZBA, fill=STATUS)) +
      geom_bar(stat='identity', position='dodge', show.legend = FALSE, width = 0.3) +
      theme_bw() +
      theme(panel.border = element_blank(),
            legend.title = element_blank(),
            axis.line = element_line(colour = "grey"),
            plot.title = element_blank()) +
      ylab(" ") +
      xlab(" ") +
      scale_fill_manual(values=c('#B43C23','#B43C23','#B43C23')) +
      scale_x_discrete(labels= c('Brak zasilenia', 'Brak zwrotu środków', 'Wstrzymanie wypłat'))+
      theme(plot.title = element_blank()) +
      scale_y_continuous(expand = c(0, 0),
                         breaks = int_breaks,
                         labels = function(x) format(x, big.mark = " "), 
                         limits=c(0,max(df$LICZBA)+0.1*max(df$LICZBA))) +
      theme(axis.text.y=element_text(color = "black", family = "URWDIN-Regular"),
            axis.text.x=element_text(color = "black", family = "URWDIN-Regular"),
            axis.title.y=element_text(angle = 0, face="bold", size=15),
            axis.title.x=element_text(hjust=1, face="bold", size=15)) +
      geom_text(aes(label = sapply(df$LICZBA, LABEL)),  color = "black",  family ="URWDIN-Demi", vjust =-0.5)
  }
  
  return(plot)
}