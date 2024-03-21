#' Tworzy obiekt data.frame z miarami kompletności danych w podziale na statusy dla umów deweloperskich
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i pięcioma kolumnami,
#' zawierającymi miary kompletności danych w podziale na statusy dla umów deweloperskich.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_6.
#' @return Obiekt data.frame z miarami kompletności danych w podziale na statusy dla umów deweloperskich.
#'
#' @examples
#' DATA_RAP004B_6(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP004B_prepared_data_6
#' @export
DATA_RAP004B_6 <- function(json_content) {
  df_main_2 <- getRAP004B_prepared_data_4(json_content)
  
  df <- data.frame(C1 = c('liczba umów deweloperskich o statusie Kompletne',
                          'liczba umów deweloperskich o statusie Brak danych od Dewelopera', 
                          'liczba umów deweloperskich o statusie Brak danych od Banku',
                          'liczba umów deweloperskich o statusie Oczekuje na dane od Dewelopera',
                          'liczba umów deweloperskich o statusie Oczekuje na dane od Banku'),
                   C2 = c(df_main_2$L_KOMPLETNE, df_main_2$L_BRAKDEWELOPER, 
                          df_main_2$L_BRAKBANK, df_main_2$L_OCZEKUJEDEWELOPER, df_main_2$L_OCZEKUJEBANK))
  
  return(df)
}

#' Tworzy obiekt data.frame z miarami kompletności danych w podziale na statusy dla umów deweloperskich
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i pięcioma kolumnami,
#' zawierającymi miary kompletności danych w podziale na statusy dla umów deweloperskich.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_6.
#' @return Obiekt grob_table z pięcioma tabelami kompletność danych dla umów deweloperskich.
#'
#' @examples
#' INFOGRAPHIC_RAP004B_6(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP004B_prepared_data_6
#' @export

INFOGRAPHIC_RAP004B_6 <- function(json_content) {
  df_main_2 <-  getRAP004B_prepared_data_4(json_content)

  if(length(json_content) == 0 || all(df_main_2 == 0)) {
    plot <- ggplot(data=df_main_2, aes(x=0, y=0)) +
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
                vjust = 0.5, hjust = 0.5, color = "black", family = "URWDIN-Regular", size = 6)
    return(plot)
  } else {
    df <- setNames(data.frame(t(as.matrix(df_main_2))), as.character(df_main_2[[1]]))
    df <- tibble::rownames_to_column(df, "STATUS")

    colnames(df) <- c("STATUS", "LICZBA")

    # sprawdzenie czy wartości nie są zerowe
    if (all(df$LICZBA == 0)) {
      # Zastąpienie wartości zerowych 1
      df$LICZBA <- 1
    }

    df <- df %>%
      mutate(Percent=LICZBA/sum(df$LICZBA)) %>%
      arrange(desc(LICZBA))

    df$colors <-  LETTERS[seq_len(nrow(df))]

    colors = c('A'='#D9553B','B'='#E17A65','C'='#EBA799','D'='#F2C8C0', 'E'='#F9E3DF')

    df <- df %>%
      arrange(desc(colors)) %>%
      mutate(
        Percent_cum = cumsum(Percent),
        y = .5 * (Percent_cum + lag(Percent_cum, default = 0))
      )

    max_value <- max(df$Percent, na.rm = TRUE)
    second_largest <- df$Percent[2]
    df$STATUS <- ifelse(df$STATUS == "L_KOMPLETNE", "Kompletne",
                        ifelse(df$STATUS == "L_BRAKDEWELOPER", "Brak danych od Dewelopera",
                               ifelse(df$STATUS == "L_OCZEKUJEDEWELOPER", "Oczekuje na dane od Dewelopera",
                                      ifelse(df$STATUS == "L_OCZEKUJEBANK", "Oczekuje na dane od Banku",
                                             ifelse(df$STATUS == "L_BRAKBANK", "Brak danych od Banku",df$STATUS)))))

    df$STATUS <- str_wrap(df$STATUS, width = 15)

    plt1 <- ggplot(data = df, fill = colors) +
      geom_hline(
        aes(yintercept = y),
        data = data.frame(y = c((max_value*1/4), (max_value*2/4), (max_value*3/4), max_value)),
        color = "lightgrey"
      ) +
      geom_col(
        aes(
          x = reorder(STATUS, -Percent),
          y = Percent,
          fill = colors
        ),
        position = "dodge2",
        show.legend = TRUE,
        alpha = .9
      ) +
      geom_point(
        aes(
          x = reorder(STATUS, -Percent),
          y = min(Percent)
        ),
        size = 3,
        color = "#B43C23"
      ) +
      geom_segment(
        aes(
          x = reorder(STATUS, -Percent),
          y = min(Percent),
          xend = reorder(STATUS, -Percent),
          yend = max(Percent),
          group = colors
        ),
        linetype = "dashed",
        color = "gray12"
      ) +
      geom_label(
        aes(
          x = reorder(STATUS, -Percent),
          y = max(Percent),
          label = paste0(round((Percent * 100),2), "%")
        ),
        position = position_stack(vjust = 0.5),
        fill = alpha("white", 0),
        label.size = 0,
        hjust = 0,
        vjust = ifelse(df$Percent == max(df$Percent), 0.55, 0.75),
        angle = 0,
        label.padding = unit(1.2, "lines"),
        color = "black",
        fontface = "bold"
      ) +
      coord_polar(start = -pi/2, clip="off") +
      theme(
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black", family = "URWDIN-Regular", face = "plain", size = 15),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_blank()
      ) +
      theme(legend.position = "none") +
      scale_fill_manual(values = colors, name = "STATUS", labels = df$STATUS)

    plot <- plt1 +
      scale_y_continuous(
        limits = c(-0.2, ifelse(df$Percent == second_largest, max_value + 0.12, max_value + 0.2)),#limits = c(-0.2, max_value + 0.09),
        expand = c(0, 0),
        breaks = c((max_value*1/4), (max_value*2/4), (max_value*3/4), max_value)
      )

    return(plot)
  }

}