#' Tworzy obiekt data.frame z podsumowaniem wątków i odpowiedzi w zestawieniu miesięcznym
#'
#' Funkcja tworzy i zwraca obiekt data.frame z trzema kolumnami,
#' zawierającymi liczbę wątków i odpowiedzi w podziale na miesiące.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP005BDS_prepared_data_1.
#' @return Obiekt data.frame z liczbą wątków w ujęciu miesięcznym.
#'
#' @examples
#' DATA_RAP005BDS_5(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP005BDS_prepared_data_1
#' @export
DATA_RAP005BDS_5 <- function(json_content) {
  return(getRap_005_1_prepared_data(json_content))
}

#' Tworzy obiekt data.frame z podsumowaniem wątków i odpowiedzi w zestawieniu miesięcznym
#'
#' Funkcja tworzy i zwraca obiekt data.frame z trzema kolumnami,
#' zawierającymi liczbę wątków i odpowiedzi w podziale na miesiące.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP005BDS_prepared_data_1.
#' @return Obiekt ggplot z wykresem słupkowym podsumowanie liczby wątków i odpowiedizw podziale miesięcznym.
#'
#' @examples
#' INFOGRAPHIC_RAP005BDS_5(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP005BDS_prepared_data_1
#' @export
INFOGRAPHIC_RAP005BDS_5<- function(json_content) {
  
  df<- DATA_RAP005BDS_5(json_content)
  
  
  colnames(df) <- c("DATA_WATKU","Liczba wątków","Liczba odpowiedzi")
  
  df2 <- tidyr::pivot_longer(df, cols=c('Liczba wątków', 'Liczba odpowiedzi'), names_to='variable', 
                             values_to="value")
  

  #df2$DATA_WATKU <- factor(df2$DATA_WATKU, levels = rev(unique(df2$DATA_WATKU)))
  
  LABEL2 <- function(value) {
    ifelse(value >= 1000, 
           format(value, format = "f", big.mark = " "),
           paste0(as.character(value), " "))
  }
  
  plot <- if(length(json_content) == 0) { 
    
    ggplot(df2, aes(x=0, y=0)) +
      xlab("") +
      ylab("") +
      theme_minimal() +
      theme(axis.text.y=element_blank(),
            axis.text.x=element_blank(),
            axis.line = element_line(colour = "black"),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            plot.title = element_blank()) +
      labs(fill=NULL) +
      guides(fill = guide_legend(reverse = TRUE)) +
      #coord_flip() +
      geom_text(aes(label = "Brak danych dla wybranych parametrów filtrowania."),
                vjust = 0.5, hjust = 0.5, color = "black", family = "URWDIN-Regular", size = 6)
    
  } else {
    num_bars = ceiling(length(df2$DATA_WATKU)/2)
    fixed_bar_width <- ifelse(num_bars < 2, 0.4, 0.8)
    font_size <- if (num_bars > 14) {
      3
    } else if (num_bars > 6) {
      4
    } else {
      5
    }
    position = position_dodge(width = fixed_bar_width)

    ggplot(df2, aes(x=DATA_WATKU, y=value, fill=variable)) +
    geom_bar(
     aes(x = fct(DATA_WATKU), y = value, group = variable, fill = variable),
     stat = 'identity', position = position, alpha = 1, width = fixed_bar_width) +
    geom_text(
        aes(x = fct(DATA_WATKU), y = value, label = LABEL2(value)),
        hjust = -1, color = 'black', family = 'URWDIN-Demi', size = font_size,
        position = position,
        inherit.aes = TRUE) +
    # geom_bar(stat='identity', position = 'dodge', width=0.4) +
    theme_minimal() +
    ylab(" ") +
    xlab(" ") +
    scale_fill_manual(values=c('#A7A8A9','#B43C23')) +
    scale_y_continuous(expand = c(0, 0),
                   breaks = int_breaks,
                   labels = function(x) format(x, big.mark = " "),
                   limits=c(0,max(df2$value)+0.1*max(df2$value))) +
    theme(legend.position = "bottom",
          axis.text.y=element_text(family = "URWDIN-Regular", color="black"),
          axis.text.x=element_text(family = "URWDIN-Regular", color="black"),
          panel.grid.minor = element_blank())  +
    guides(fill = guide_legend()) +
    labs(fill=NULL) +
    coord_flip()
      # geom_text(data = df2[df2$variable == "Liczba wątków", ],
      #           aes(label = LABEL2(value)),
      #           color = "black",  family ="URWDIN-Demi", vjust = -0.4, hjust = -0.2) +
      # geom_text(data = df2[df2$variable == "Liczba odpowiedzi", ],
      #           aes(label = LABEL2(value)),
      #           color = "black",  family ="URWDIN-Demi", vjust = 1.2, hjust = -0.2)
  }
  
  return(plot)
  
}