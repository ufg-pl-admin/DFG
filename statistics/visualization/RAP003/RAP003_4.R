#' Tworzy obiekt data.frame z miar rozliczonych zasileń WPLMRP, INSKLA i DANEAN.
#'
#' Funkcja tworzy i zwraca obiekt data.frame z trzema kolumnami,
#' zawierającymi miary rozliczonych zasileń WPLMRP, INSKLA i DANEAN.
#'
#' @examples
#' DATA_RAP003_4(NULL)
#'
#' @export
options(scipen=999)
source(paste(getwd(), '/utils/convert_toUtil.R', sep=""))
source(paste(getwd(), '/utils/labelUtil.R', sep=""))

DATA_RAP003_4 <- function(json_content) {
  
  
  colnames(json_content) <- c("RODZAJ_ZASILENIA", "LICZBA_ZASILEN", "KWOTA_WPLAT")
  df <- as.data.frame(json_content)
  df$LICZBA_ZASILEN <- as.numeric(df$LICZBA_ZASILEN)
  df$KWOTA_WPLAT <- as.numeric(df$KWOTA_WPLAT)
  
  
  return(df)
}

#' Tworzy obiekt data.frame z miar rozliczonych zasileń WPLMRP, INSKLA i DANEAN.
#'
#' Funkcja tworzy i zwraca obiekt data.frame z trzema kolumnami,
#' zawierającymi miary poprawnie rozliczonych zasileń WPLMRP, INSKLA i DANEAN.
#'
#' @return Obiekt ggplot z liczbą rozliczonych zasileń WPLMRP, INSKLA i DANEAN.
#'
#' @examples
#' INFOGRAPHIC_RAP003_4(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @export

INFOGRAPHIC_RAP003_4<- function(json_content) {
  df <- DATA_RAP003_4(json_content)
  
  
  left_max_limit <- round(max(df$LICZBA_ZASILEN) + 0.4 * max(df$LICZBA_ZASILEN))
  
  plot1 <- ggplot(df, aes(x=LICZBA_ZASILEN, y=RODZAJ_ZASILENIA)) +
    geom_bar(stat="identity", fill="#B43C23", width=.5) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(axis.text.y=element_blank(),
          axis.text.x=element_text(family = "URWDIN-Regular", color="black"), 
          plot.margin = unit(c(0, 0.3, 0, 0.8), "cm"),
          panel.border = element_blank()) +
    scale_x_continuous(breaks = seq(0, max(df$LICZBA_ZASILEN), length.out = 5),
                       expand = c(0,0), 
                       labels = function(x) sapply(x, convert),
                       limits = c(left_max_limit, 0),
                       trans = "reverse"
    ) +
    scale_y_discrete(position = "right") +
    geom_text(aes(label = sapply(LICZBA_ZASILEN, convert)),
              color = "black",  family ="URWDIN-Demi", hjust = 1.2) 
  
  right_max_limit <- round(max(df$KWOTA_WPLAT) + 0.4 * max(df$KWOTA_WPLAT))
  
  plot2 <- ggplot(df, aes(x=KWOTA_WPLAT, y=RODZAJ_ZASILENIA)) +
    geom_bar(stat="identity", fill="#B43C23", width=.5) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(axis.text.y=element_text(family = "URWDIN-Regular", color="black",hjust =0.5,vjust = 0.5),
          axis.text.x=element_text(family = "URWDIN-Regular", color="black"), 
          plot.margin = unit(c(0, 1.2, 0, -1.1), "cm"),
          panel.border = element_blank()) +
    scale_x_continuous(
      breaks = seq(0, max(df$KWOTA_WPLAT), length.out = 5),
      expand = c(0, 0),
      labels = function(x) sapply(x,convert_to_millions_or_billions_or_thousands),
      limits = range(c(0, right_max_limit))
    ) +
    geom_text(aes(label = sapply(KWOTA_WPLAT, convert_to_millions_or_billions_or_thousands)),  color = "black",  family ="URWDIN-Demi", hjust =-0.2) 
  
  plot <- plot_grid(plot1, plot2)

  return(plot)
}