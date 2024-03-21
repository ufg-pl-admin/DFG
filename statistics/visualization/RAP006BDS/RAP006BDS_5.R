#' Tworzy obiekt data.frame z podsumowaniem zrealizowanych zasileń podzielonych na statusy
#'
#' Funkcja tworzy i zwraca obiekt data.frame z siedmioma kolumnami,
#' zawierającymi miary łącznej liczby zasileń podzielonych na statusy w danym okresie.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_5.
#' @return Obiekt data.frame z liczbą zasileń podzielonych na statusy.
#'
#' @examples
#' DATA_RAP006BDS_5(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP006BDS_prepared_data_5
#' @export
DATA_RAP006BDS_5 <- function(json_content) {
  df_main_2 <- getRAP006BDS_prepared_data_5(json_content)
  colnames(df_main_2) <- c("OKRES", "PRZYJETE", "PRZYJETE_Z_OSTRZEZENIEM", "ODRZUCONE", "W_WERYFIKACJI", "ROBOCZE", "LACZNIE")
  
  return(df_main_2)
}

#' Tworzy obiekt data.frame z podsumowaniem zrealizowanych zasileń w zestawieniu z liczbą błędów/ostrzeżeń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z czterema kolumnami,
#' zawierającymi miary łącznej liczby zasileń w zestawieniu z liczbą błędów i wystapień ostrzeżeń w danym okresie.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_5.
#' @return Obiekt ggplot z wykresem słupkowym podsumowanie zasileń w podziale na statusy.
#'
#' @examples
#' INFOGRAPHIC_RAP006BDS_5(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP006BDS_prepared_data_5
#' @export
source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))
source(paste(getwd(), '/utils/label2Util.R', sep=""))

INFOGRAPHIC_RAP006BDS_5 <- function(json_content) {
  df_main_2 <- getRAP006BDS_prepared_data_5(json_content)
  
  liczba_zasilen <- df_main_2 %>% select(DATA_ZASILENIA, 
                                         PRZYJETE, 
                                         PRZYJETE_Z_OSTRZEZENIEM, 
                                         ODRZUCONE, 
                                         W_WERYFIKACJI, 
                                         ROBOCZE) 
  
  liczba_zasilen <- as.data.frame(liczba_zasilen)
  liczba_zasilen <- melt(liczba_zasilen, id.vars = 'DATA_ZASILENIA')

  #calculation of the midpoint
  date_values <- as.Date(paste0(liczba_zasilen$DATA_ZASILENIA, "-01"), format = "%m-%Y-%d")
  
  middle_date <- as.character(format(date_values[length(date_values) %/% 2 + 1], "%m-%Y"))

  plot <- if (all(liczba_zasilen$value == 0)) { 
    
    ggplot(liczba_zasilen, aes(DATA_ZASILENIA, value)) +
      geom_bar(stat="identity", position = 'stack', alpha=1, width=.8) +
      xlab("") + 
      ylab("") +
      theme_bw() +
      theme(axis.text.y=element_text(color = "black", family = "URWDIN-Regular"),
            axis.text.x=element_text(color = "black", family = "URWDIN-Regular"), 
            axis.line = element_line(colour = "black"), 
            panel.border = element_blank(),
            legend.position="bottom",
            legend.title = element_blank()
      ) +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(breaks = int_breaks, labels = LABEL2) +
      geom_text(data = df_main_2, aes(x = middle_date, y=0, label = "Brak danych dla wybranych parametrów filtrowania."),
                vjust = 0.5, hjust = 0.5, color = "black", family = "URW DIN-REGULAR", size = 6)
  } else {
    text_theme = element_text(color = "black", family = "URWDIN-Regular")
    if (nrow(liczba_zasilen)/5 > 20) {
      text_theme = element_text(angle = 25, vjust=0.7, color = "black", family = "URWDIN-Regular")
    }

    ggplot(liczba_zasilen, aes(DATA_ZASILENIA, value, fill = variable)) +
      geom_bar(stat="identity", position = 'stack', alpha=1, width=.8) +
      xlab("") + 
      ylab("") +
      theme_bw() +
      theme(axis.text.y=element_text(color = "black", family = "URWDIN-Regular"),
            axis.text.x=text_theme,
            axis.line = element_line(colour = "black"), 
            panel.border = element_blank(),
            legend.position="bottom",
            legend.title = element_blank()
      ) +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(breaks = int_breaks, expand = c(0, 0), labels = LABEL2) +
      scale_fill_manual(labels = c("PRZYJETE" = 'Przyjęte',
                                   "PRZYJETE_Z_OSTRZEZENIEM" = 'Przyjęte z ostrzeżeniem',
                                   "ODRZUCONE" = 'Odrzucone',
                                   "W_WERYFIKACJI" = 'W weryfikacji',
                                   "ROBOCZE" = 'Robocze'),
                        values=c("#09599B", "#199CB5", "#A7A8A9", "#D29082", "#B43C23")) 
    
  }
  return(plot)
}