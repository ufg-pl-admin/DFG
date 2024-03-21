#' Tworzy obiekt data.frame z liczbą uruchomień anomalii w podziale na statusy
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami,
#' zawierającymi liczbę uruchomień i liczby uruchomień w podziale na statusy anomalii.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP009DS_prepared_data_3.
#' @return Obiekt data.frame z liczbą uruchomień anomalii.
#'
#' @examples
#' DATA_RAP009DS_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP009DS_prepared_data_3
#' @export
DATA_RAP009DS_3 <- function(json_content) {
  
  df <- getRap_009DS_3_prepared_data(json_content)
  
  
  return(df)
}

#' Tworzy obiekt data.frame z liczbą uruchomień anomalii w podziale na statusy
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami,
#' zawierającymi liczbę uruchomień i liczby uruchomień w podziale na statusy anomalii.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP009DS_prepared_data_3.
#' @return Obiekt ggplot z wykresem słupkowym liczby uruchomień anomalii per status.
#'
#' @examples
#' INFOGRAPHIC_RAP009DS_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP009DS_prepared_data_3
#' @export
source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))
source(paste(getwd(), '/utils/labelUtil.R', sep="")) 

INFOGRAPHIC_RAP009DS_3<- function(json_content) {
  
  df1<- DATA_RAP009DS_3(json_content)
  
  
  
  df1$STATUS_UDOSTEPNIENIA = str_wrap(df1$STATUS_UDOSTEPNIENIA, width = 11)
  
  
  plot <- if(length(json_content) == 0) { 
    
    ggplot(df1, aes(x=0, y=0)) +
      theme_minimal() +
      ylab(" ") +
      xlab(" ") +
      labs(fill=NULL) +
      theme(axis.text.y=element_blank(),
            axis.text.x=element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank()) +
      geom_text(aes(label = "Brak danych dla wybranych parametrów filtrowania."),
                vjust = 0.5, hjust = 0.5, color = "black", family = "URW DIN-REGULAR", size = 6)
    
  } else {
    
    ggplot(df1, aes(x=STATUS_UDOSTEPNIENIA, y=LICZBA_ANOMALII, fill=STATUS_UDOSTEPNIENIA)) +
      geom_bar(stat='identity', position='dodge', show.legend = FALSE, width=0.2) +
      theme_minimal() +
      ylab(" ") +
      xlab(" ") +
      scale_fill_manual(values=c('#B43C23','#B43C23','#B43C23','#B43C23')) +
      scale_y_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(df1$LICZBA_ANOMALII)+0.1*max(df1$LICZBA_ANOMALII))) +
      theme(axis.text.y=element_text(family = "URWDIN-Regular", color="black"),
            axis.text.x=element_text(family = "URWDIN-Regular", color="black"),
            panel.grid.minor = element_blank()) +
      labs(fill=NULL) +
      geom_text(aes(label = sapply(LICZBA_ANOMALII, LABEL)),  color = "black",  family ="URWDIN-Demi", vjust =-0.5)
  
  }
  
  return(plot)
  
}