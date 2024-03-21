#' Tworzy obiekt data.frame z liczbą wystąpień anomalii
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami,
#' zawierającymi liczby wystapień anomalii.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP009B_prepared_data_4.
#' @return Obiekt data.frame z liczbą wystapień anomalii.
#'
#' @examples
#' DATA_RAP009B_4(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP009B_prepared_data_4
#' @export
DATA_RAP009B_4 <- function(json_content) {
  
  df <- getRap_009B_4_prepared_data(json_content)
  
  
  return(df)
}

#' Tworzy obiekt data.frame z liczbą wystąpień anomalii
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami,
#' zawierającymi liczby wystapień anomalii.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP009B_prepared_data_4.
#' @return Obiekt ggplot z wykresem słupkowym opisującymi liczbę wystąpień anomalii.
#'
#' @examples
#' INFOGRAPHIC_RAP009B_4(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP009B_prepared_data_4
#' @export
INFOGRAPHIC_RAP009B_4<- function(json_content) {
  df <- DATA_RAP009B_4(json_content)

  df <- df[with(df,order(LICZBA_ANOMALII, KOD_ANOMALII, decreasing = TRUE)),]

  if (nrow(df) >= 25) {
    df <- df[1:25, ]
  } else if (nrow(df) >= 20) {
    df <- df[1:20, ]
  }else if (nrow(df) >= 15) {
    df <- df[1:15, ]
  } else if (nrow(df) >= 10) {
    df <- df[1:10, ]
  } else if (nrow(df) >= 5) {
    df <- df[1:5, ]
  }
  
  int_breaks <- function(x, n = 5) {
    l <- pretty(x, n)
    l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
  }
  
  LABEL <- function(value) {
    if (value >= 1000) {
      amount <- format(value, format = "f", big.mark=" ")
    } else {
      amount <- format(value, format = "f", big.mark="")
    }
  }
  
  plot <- if(length(json_content) == 0) { 
    
    ggplot(df, aes(x=0, y=0)) +
      xlab("") +
      ylab("") +
      theme_bw() +
      theme(axis.text.y=element_blank(),
            axis.text.x=element_blank(),
            axis.line = element_line(colour = "black"),
            axis.title.y=element_text(angle = 0, face="bold", size=9),
            axis.title.x=element_text(hjust=1, face="bold", size=9),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            plot.title = element_blank()) +
      geom_text(aes(label = "Brak danych dla wybranych parametrów filtrowania."),
                vjust = 0.5, hjust = 0.5, color = "black", family = "URW DIN-REGULAR", size = 5)
    
  } else {
    
    ggplot(df, aes(x=LICZBA_ANOMALII, y=KOD_ANOMALII)) +
    #ggtitle("WYSTĄPIENIA ANOMALII") +
    geom_bar(stat="identity", fill="#D29082", width=.8) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(axis.text.y=element_text(family = "URWDIN-Regular", color="black"),
          axis.text.x=element_text(family = "URWDIN-Regular", color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(df$LICZBA_ANOMALII)+0.1*max(df$LICZBA_ANOMALII))) +
    geom_text(aes(label = sapply(LICZBA_ANOMALII, LABEL)),  color = "black",  family ="URWDIN-Demi", hjust =-0.5, size=4.5)
    
}
    
  return(plot)
}




