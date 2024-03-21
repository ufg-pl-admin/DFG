#' Tworzy obiekt data.frame z sumą kwot należności w podziale na lata.
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwunastoma kolumnami,
#' zawierającymi sumę kwot należności w podziale na lata.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP001_prepared_data_2.
#' @return Obiekt data.frame z sumą kwot należności w podziale na lata.
#'
#' @examples
#' DATA_RAP001_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP001_prepared_data_3
#' @export

DATA_RAP001_3 <- function(json_content) {
  df_main_2 <- getRAP001_prepared_data_3(json_content)

  return(df_main_2)
}

#' Tworzy obiekt data.frame z sumą kwot należności w podziale na lata.
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwunastoma kolumnami,
#' zawierającymi sumę kwot należności w podziale na lata.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP001_prepared_data_3.
#' @return Obiekt ggplot z wykresem słupkowym.
#'
#' @examples
#' INFOGRAPHIC_RAP001_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP001_prepared_data_3
#' @export
source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))

INFOGRAPHIC_RAP001_3 <- function(json_content) {
  df_main_2 <-  getRAP001_prepared_data_3(json_content)
  
  plot <- ggplot(df_main_2, aes(x=ROK, y=SUMA_OGOLEM)) +
    geom_bar(stat='identity', width = 0.2, position='dodge', show.legend = FALSE, fill = "#B43C23") +
    theme_minimal() +
    ylab(" ") +
    xlab(" ") +
    theme(axis.text.y=element_text(size = 12, color = "black", family = "URWDIN-Regular"),
          axis.text.x=element_text(size = 12, color = "black", family = "URWDIN-Regular"),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),
          legend.position = "bottom",
          plot.title = element_blank())+#element_text(hjust = 0.5, size = 14, margin=margin(0,0,0,0))) +
    labs(fill=NULL) +
    scale_y_continuous(
      breaks = int_breaks,
      expand = c(0, 0),
      labels = function(x) paste0(format(x, decimal.mark = ",", big.mark = " "), " PLN"), 
      limits=c(0,max(df_main_2$SUMA_OGOLEM)+0.1*max(df_main_2$SUMA_OGOLEM))
    ) +
    theme(#plot.title = element_text(family = "URW DIN-DEMI", size = 16),
          axis.text = element_text(family = "URW DIN-REGULAR", size = 12),
          legend.position="none") +
    geom_text(aes(label = sapply(SUMA_OGOLEM, LABEL)),  color = "black",  family ="URWDIN-Demi", size=5, vjust =-0.5)
  
  return(plot)
}