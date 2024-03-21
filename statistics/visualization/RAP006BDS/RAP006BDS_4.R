#' Tworzy obiekt data.frame z podsumowaniem zrealizowanych zasileń w zestawieniu z liczbą błędów/ostrzeżeń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z czterema kolumnami,
#' zawierającymi miary łącznej liczby zasileń w zestawieniu z liczbą błędów i wystapień ostrzeżeń w danym okresie.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_4.
#' @return Obiekt data.frame z liczbą uruchomień zasileń.
#'
#' @examples
#' DATA_RAP006BDS_4(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP006BDS_prepared_data_4
#' @export
DATA_RAP006BDS_4 <- function(json_content) {
  df_main_2 <- getRAP006BDS_prepared_data_4(json_content)

  return(df_main_2)
}

#' Tworzy obiekt data.frame z podsumowaniem zrealizowanych zasileń w zestawieniu z liczbą błędów/ostrzeżeń
#'
#' Funkcja tworzy i zwraca obiekt data.frame z czterema kolumnami,
#' zawierającymi miary łącznej liczby zasileń w zestawieniu z liczbą błędów i wystapień ostrzeżeń w danym okresie.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_4.
#' @return Obiekt ggplot z wykresem liniowym podsumowanie zrealizowanych zasileń.
#'
#' @examples
#' INFOGRAPHIC_RAP006BDS_4(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP006BDS_prepared_data_4
#' @export

INFOGRAPHIC_RAP006BDS_4 <- function(json_content) {
  df_main_2 <- getRAP006BDS_prepared_data_4(json_content)

  int_breaks <- function(x, n = 5) {
    l <- pretty(x, n)
    l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
  }
  
  LABEL <- function(value) {
    amount <- ifelse(value >= 1000, format(value, format = "f", big.mark = " "), 
                     format(value, format = "f", big.mark = ""))
    return(amount)
  }
  
  
  
  plot <- if (all(df_main_2$LICZBA_ZASILEN == 0) && all(df_main_2$LICZBA_BLEDOW == 0) && all(df_main_2$LICZBA_OSTRZEZEN == 0)) {
    
    ggplot(data=df_main_2, aes(x=0, y=0)) +
      xlab("")+
      ylab("") +
      theme(
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0)
      ) +
      geom_text(aes(label = "Brak danych dla wybranych parametrów filtrowania."),
                vjust = 0.5, hjust = 0.5, color = "black", family = "URW DIN-REGULAR", size = 5)
  } else {
    zero_entry <- data.frame(OKRES = '', LICZBA_ZASILEN = 0, LICZBA_BLEDOW = 0, LICZBA_OSTRZEZEN = 0)
    df_main_2 <- rbind(zero_entry, df_main_2)
    df_main_2$OKRES <- factor(df_main_2$OKRES, levels = unique(df_main_2$OKRES), ordered = TRUE)

    text_theme = element_text(color = "black", family = "URWDIN-Regular")
    print(nrow(df_main_2))
    if (nrow(df_main_2) > 20) {
      text_theme = element_text(angle = 25, vjust=0.7, color = "black", family = "URWDIN-Regular")
    }

  ggplot(data = df_main_2, mapping = aes(x = OKRES)) +
    geom_line(mapping = aes(y = LICZBA_ZASILEN, group = 1, color = "Liczba Zasileń")) +
    geom_line(mapping = aes(y = LICZBA_BLEDOW, group = 1, color = "Liczba Błędów")) +
    geom_line(mapping = aes(y = LICZBA_OSTRZEZEN, group = 1, color = "Liczba Ostrzeżeń")) +
    xlab("") +
    ylab("") +
    ylim(0, max(df_main_2$LICZBA_ZASILEN,
            df_main_2$LICZBA_BLEDOW,
            df_main_2$LICZBA_OSTRZEZEN,
            3)) +
    theme_bw() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(breaks = int_breaks, labels = LABEL) +
    scale_color_manual(values = c("Liczba Zasileń" = "#000000", "Liczba Błędów" = "#B43C23", "Liczba Ostrzeżeń" = "#D29082")) +
    theme(axis.text.y=element_text(color = "black", family = "URWDIN-Regular"),
          axis.text.x=text_theme,
          axis.line = element_line(colour = "azure4"), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          text = element_text(family = "URWDIN-Regular"),
          plot.margin = unit(c(0.2,1,0.2,0.2),"cm"))
}
    
return(plot)
}