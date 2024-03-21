#' Tworzy obiekt data.frame z podsumowaniem wątków w podziale na kategorię
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami,
#' zawierającymi liczbę wątków w podziale na kategorię.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP005BDS_prepared_data_4.
#' @return Obiekt data.frame z liczbą wątków w podziale na status.
#'
#' @examples
#' DATA_RAP005BDS_4(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP005BDS_prepared_data_4
#' @export
DATA_RAP005BDS_4 <- function(json_content) {
  
  df_main <- getRap_005_4_prepared_data(json_content)
  
  
  return(df_main)
}

#' Tworzy obiekt data.frame z podsumowaniem wątków w podziale na kategorię
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami,
#' zawierającymi liczbę wątków w podziale na kategorię.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP005BDS_prepared_data_4.
#' @return Obiekt ggplot z wybresem słupkowym podsumowanie miar wątków w podziale na kategorię.
#'
#' @examples
#' INFOGRAPHIC_RAP005BDS_4(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP005BDS_prepared_data_4
#' @export
source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))
source(paste(getwd(), '/utils/labelUtil.R', sep=""))  

INFOGRAPHIC_RAP005BDS_4<- function(json_content) {
  
  df<- DATA_RAP005BDS_4(json_content)
  
  colnames(df) <- c("KATEGORIA","Liczba wątków","Liczba odpowiedzi")
  
  df2 <- tidyr::pivot_longer(df, cols=c('Liczba wątków', 'Liczba odpowiedzi'), names_to='variable',
                             values_to="value")


  df2$KATEGORIA <- factor(df2$KATEGORIA, levels = rev(unique(df2$KATEGORIA)))
  df2$variable <- factor(df2$variable, levels = c('Liczba wątków', 'Liczba odpowiedzi'))


  df2$KATEGORIA = str_wrap(df2$KATEGORIA, width = 11)
  
  LABEL2 <- function(value) {
    ifelse(value >= 1000, format(value, big.mark = " "), as.character(value))
  }
  
  plot <- if(length(json_content) == 0) { 
    
    ggplot(df2, aes(x=0, y=0)) +
      xlab("") +
      ylab("") +
      theme_minimal() +
      theme(axis.text.y=element_blank(),
            axis.text.x=element_blank(),
            axis.line = element_line(colour = "black"),
            axis.title.y=element_text(angle = 0, face="bold", size=9),
            axis.title.x=element_text(hjust=1, face="bold", size=9),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            plot.title = element_blank()) +
      labs(fill=NULL) +
      guides(fill = guide_legend()) +
      geom_text(aes(label = "Brak danych dla wybranych parametrów filtrowania."),
                vjust = 0.5, hjust = 0.5, color = "black", family = "URWDIN-Regular", size = 6)
    
  } else {
    num_bars = length(df2$KATEGORIA)
    fixed_bar_width <- ifelse(num_bars < 2, 0.1, 0.2)
    position = position_dodge(width = fixed_bar_width)

    ggplot(df2, aes(x=KATEGORIA, y=value, fill=variable)) +
    geom_bar(
     aes(x = fct(KATEGORIA), y = value, group = variable, fill = variable),
     stat = 'identity', position = position, alpha = 1, width = fixed_bar_width) +
    geom_text(
        aes(x = fct(KATEGORIA), y = value, label = LABEL2(value)),
        vjust = -1, color = 'black', family = 'URWDIN-Demi',
        position = position,
        inherit.aes = TRUE) +
    theme_minimal() +
    ylab(" ") +
    xlab(" ") +
    scale_fill_manual(values=c('#B43C23', '#A7A8A9')) +
    scale_y_continuous(expand = c(0, 0),
                         breaks = int_breaks,
                         labels = function(x) format(x, big.mark = " "), 
                         limits=c(0,max(df2$value)+0.1*max(df2$value))) +
    theme(legend.position = "bottom",
          axis.text.y=element_text(family = "URWDIN-Regular", color="black"),
          axis.text.x=element_text(family = "URWDIN-Regular", color="black"),
          panel.grid.minor = element_blank()) +
    guides(fill = guide_legend()) +
    labs(fill=NULL)
}
  
  return(plot)
  
}