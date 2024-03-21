#' Tworzy obiekt data.frame z podsumowaniem wątków w podziale na status
#'
#' Funkcja tworzy i zwraca obiekt data.frame z czterema kolumnami,
#' zawierającymi liczbę wątków w podziale na status.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP005BDS_prepared_data_2.
#' @return Obiekt data.frame z liczbą wątków w podziale na status.
#'
#' @examples
#' DATA_RAP005BDS_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP005BDS_prepared_data_2
#' @export
DATA_RAP005BDS_3 <- function(json_content) {
  
  df_main <- getRap_005_2_prepared_data(json_content)

  if (nrow(df_main) == 0) {
    # Obsługa przypadku pustej ramki danych
    df <- data.frame(STATUS_WIADOMOSCI = character(),
                            LICZBA_WATKOW = numeric(),
                            stringsAsFactors = FALSE)
  } else {
    # Agregacja danych
    df <- setNames(aggregate(df_main$LICZBA_WATKOW,
                                    by = list(df_main$STATUS_WIADOMOSCI),
                                    FUN = sum),
                          c("STATUS_WIADOMOSCI", "LICZBA_WATKOW"))
  }
  
  return(df)
}

#' Tworzy obiekt data.frame z podsumowaniem wątków w podziale na status
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami,
#' zawierającymi liczbę wątków w podziale na status.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP005BDS_prepared_data_2.
#' @return Obiekt ggplot z wybresem słupkowym podsumowanie miar wątków w podziale na status.
#'
#' @examples
#' INFOGRAPHIC_RAP005BDS_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP005BDS_prepared_data_2
#' @export
source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))
source(paste(getwd(), '/utils/labelUtil.R', sep=""))  

INFOGRAPHIC_RAP005BDS_3<- function(json_content) {
  
  df1<- DATA_RAP005BDS_3(json_content)
  
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
  df1$STATUS_WIADOMOSCI = str_wrap(df1$STATUS_WIADOMOSCI, width = 11)
    print(df1)
  
  ggplot(df1, aes(x=STATUS_WIADOMOSCI, y=LICZBA_WATKOW, fill=STATUS_WIADOMOSCI)) +
    geom_bar(stat='identity', position='dodge', show.legend = FALSE, width=0.2) +
    theme_minimal() +
    ylab(" ") +
    xlab(" ") +
    scale_fill_manual(values=c('#B43C23','#B43C23','#B43C23','#B43C23','#B43C23')) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = int_breaks,
                       labels = function(x) format(x, big.mark = " "), 
                       limits=c(0,max(df1$LICZBA_WATKOW)+0.1*max(df1$LICZBA_WATKOW))) +
    theme(axis.text.y=element_text(family = "URWDIN-Regular", color="black"),
          axis.text.x=element_text(family = "URWDIN-Regular", color="black"),
          panel.grid.minor = element_blank()) +
    labs(fill=NULL) +
    geom_text(aes(label = sapply(df1$LICZBA_WATKOW, LABEL)),  color = "black",  family ="URWDIN-Demi", vjust =-0.5)
}
  
  return(plot)
  
}