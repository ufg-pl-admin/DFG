#' Tworzy obiekt data.frame z sumą kwot należności w podziale na województwa.
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwunastoma kolumnami,
#' zawierającymi sumę kwot należności w podziale na województwa.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP001_prepared_data_2.
#' @return Obiekt data.frame z sumą kwot należności w podziale na województwa.
#'
#' @examples
#' DATA_RAP001_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP001_prepared_data_2
#' @export
DATA_RAP001_2 <- function(json_content) {
  df_main_2 <- getRAP001_prepared_data_2(json_content)
  # df <- data.frame(C1 = c("Województwo", "Kwota należności"),
  #                  C2 = c(df_main_2$WOJEWODZTWO, df_main_2$SUMA_OGOLEM))
  
  return(df_main_2)
}

#' Tworzy obiekt data.frame z sumą kwot należności w podziale na województwa.
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwunastoma kolumnami,
#' zawierającymi sumę kwot należności w podziale na województwa.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP001_prepared_data_2.
#' @return Obiekt ggplot z wykresem słupkowym.
#'
#' @examples
#' INFOGRAPHIC_RAP001_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP001_prepared_data_2
#' @export

source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))
source(paste(getwd(), '/utils/labelUtil.R', sep=""))

INFOGRAPHIC_RAP001_2 <- function(json_content) {
  df_main_2 <-  getRAP001_prepared_data_2(json_content) 

  plot <- if(length(json_content) == 0) { 
 
    ggplot(df_main_2, aes(x=0, y=0)) +
      xlab("") +
      ylab("") +
      theme_bw() +
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(color = "black", family = "URWDIN-Regular"),
            axis.line = element_line(colour = "black"),
            axis.title.y=element_text(angle = 0, face="bold", size=9),
            axis.title.x=element_text(hjust=1, face="bold", size=9),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            plot.title = element_blank()) +
      labs(fill=NULL) +
      scale_x_continuous(
        breaks = int_breaks,
        expand = c(0, 0),
        labels = function(x) paste0(format(x, decimal.mark = ",", big.mark = " "), " PLN")
      ) +
    geom_text(aes(label = "Brak danych dla wybranych parametrów filtrowania."),
              vjust = 0.5, hjust = 0.5, color = "black", family = "URW DIN-REGULAR", size = 5)
    
  } else { 
    ggplot(df_main_2, aes(x=SUMA_OGOLEM, y=WOJEWODZTWO)) +
    geom_bar(stat='identity', position='dodge', alpha=1, width=.8, fill= "#B43C23") +
    xlab("") + 
    ylab("") +
    theme_bw() +
    theme(axis.text.y=element_text(color = "black", family = "URWDIN-Regular"),
          axis.text.x=element_text(color = "black", family = "URWDIN-Regular"),
          axis.line = element_line(colour = "black"),
          axis.title.y=element_text(angle = 0, face="bold", size=9),
          axis.title.x=element_text(hjust=1, face="bold", size=9),
          panel.border = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          plot.title = element_blank())+#element_text(hjust = 0.5, size = 14, margin=margin(0,0,0,0))) +
    labs(fill=NULL) +
    scale_x_continuous(
      breaks = int_breaks,
      expand = c(0, 0),
      labels = function(x) paste0(format(x, decimal.mark = ",", big.mark = " "), " PLN"), 
      limits=c(0,max(df_main_2$SUMA_OGOLEM)+0.15*max(df_main_2$SUMA_OGOLEM))
    ) +
    theme(#plot.title = element_text(family = "URW DIN-DEMI", size = 16),
          axis.text = element_text(family = "URW DIN-REGULAR", size = 12),
          legend.position="none") +
    geom_text(aes(label = paste0(sapply(SUMA_OGOLEM, LABEL), " PLN")),  color = "black",  family ="URWDIN-Demi", size=5, hjust =-0.5)
  }
  
  return(plot)
}