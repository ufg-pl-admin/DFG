#' Tworzy obiekt data.frame z miarami kompletności zasileń umów MRP w podziale na statusy
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dziesięcioma kolumnami,
#' zawierającymi miary kompletności zasileń umów MRP w podziale na statusy.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_4.
#' @return Obiekt data.frame z miarami kompletności zasileń umów MRP w podziale na statusy.
#'
#' @examples
#' DATA_RAP004B_4(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP004B_prepared_data_4
#' @export
DATA_RAP004B_4 <- function(json_content) {
  df_main_2 <- getRAP004B_prepared_data_3(json_content)
  
  df <- data.frame(C1 = c('liczba umów MRP o statusie Kompletne',
                          'liczba umów MRP o statusie Brak danych od Dewelopera', 
                          'liczba umów MRP o statusie Oczekuje na dane od Dewelopera'),
                   C2 = c(df_main_2$L_KOMPLETNE, df_main_2$L_BRAKDEWELOPER, 
                          df_main_2$L_OCZEKUJEDEWELOPER))
  return(df)
}

#' Tworzy obiekt data.frame z miarami kompletności zasileń umów MRP w podziale na statusy
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i dziesięcioma kolumnami,
#' zawierającymi miary kompletności zasileń umów MRP w podziale na statusy.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP004B_prepared_data_4.
#' @return Obiekt ggplot opisujący miary kompletności zasileń umów MRP w podziale na statusy.
#'
#' @examples
#' INFOGRAPHIC_RAP004B_4(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP004B_prepared_data_4
#' @export

INFOGRAPHIC_RAP004B_4 <- function(json_content) {
  df_main_2 <-  getRAP004B_prepared_data_3(json_content)

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
    df$LICZBA<- as.numeric(df$LICZBA)

    # sprawdzenie czy wartości nie są zerowe
    if (all(df$LICZBA == 0)) {
      # Zastąpienie wartości zerowych 1
      df$LICZBA <- 1
    }

    df <- df %>%
      mutate(Percent=LICZBA/sum(df$LICZBA)) %>%
      arrange(desc(LICZBA))

    #df <- cbind(df, colors=c('A', 'B', 'C'))

    df$colors <-  LETTERS[seq_len(nrow(df))]

    colors = c('A'='#D9553B','B'='#E17A65','C'='#EBA799')

    # INFOGRAFIKA_4 <- ggplot(data = df[which(df$LICZBA>0),], aes(x = 2, y = Percent, fill = colors)) +
    #   geom_col(color = "white") +
    #   coord_polar("y", start = 1) +
    #   ggtitle("STATUSY KOMPLETNOSCI DANYCH DLA UMÓW MRP") +
    #   geom_text(aes(label = paste0(round(Percent*100), "%")),
    #             position = position_stack(vjust = 0.5)) +
    #   theme(panel.background = element_blank(),
    #         axis.line = element_blank(),
    #         axis.text = element_blank(),
    #         axis.ticks = element_blank(),
    #         axis.title = element_blank(),
    #         plot.title = element_text(hjust = 0.5, size = 18)) +
    #   theme(legend.position="right") +
    #   scale_fill_manual(values = colors, name = "Status Kompletności", labels = c(df$STATUS)) +
    #   xlim(0.5, 2.5) +
    #   theme(text = element_text(family = "URWDIN-Regular"))

    df <- df %>%
      arrange(desc(colors)) %>%
      mutate(
        Percent_cum = cumsum(Percent),
        y = .5 * (Percent_cum + lag(Percent_cum, default = 0))
      )

    max_value <- max(df$Percent, na.rm = TRUE)

    df$STATUS <- ifelse(df$STATUS == "L_KOMPLETNE", "Kompletne",
                        ifelse(df$STATUS == "L_BRAKDEWELOPER", "Brak danych od Dewelopera",
                               ifelse(df$STATUS == "L_OCZEKUJEDEWELOPER", "Oczekuje na dane od Dewelopera", df$STATUS)))

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
        vjust = 0.75,
        angle = 0,
        label.padding = unit(1.2, "lines"),
        color = "black",
        fontface = "bold"
      ) +
      # geom_text(
      #   aes(
      #     x = reorder(STATUS, -Percent),
      #     y = max(Percent),  # Adjust the y position for label placement above the bars
      #     label = STATUS
      #   ),
      #   hjust = 0.75,
      #   vjust = -0.5,  # Move labels slightly above the bars
      #   color = "black",
      #   family = "URWDIN-Regular", fontface = "plain", size = 8
      # ) +
    # geom_text(
    #   aes(
    #     x = reorder(STATUS, -Percent),
    #     y = max_value,  # Adjust the y position for label placement
    #     label = STATUS
    #   ),
    #   position = position_dodge(width = 1),
    #   hjust = 0.5,
    #   vjust = -0.5,  # Move labels away from the chart
    #   color = "black",
    #   fontface = "bold"
    # ) +
    theme(
      panel.background = element_blank(),
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(color = "black", family = "URWDIN-Regular", face = "plain", size = 15),
      axis.ticks = element_blank(),
      #axis.title.x = element_text(vjust=-0.5),
      axis.title = element_blank(),
      plot.title = element_blank()#,
      # plot.margin = unit(rep(-1,9), "cm")
    ) +
      coord_polar(start = -pi/2, clip="off") +
      theme(legend.position = "none") +
      #scale_fill_manual(values = colors, name = "STATUS", guide = "none")
      scale_fill_manual(values = colors, name = "STATUS", labels = df$STATUS) #+
    # scale_x_discrete(expand = expansion(mult = 0.1, add = .4))
    # expand_limits(y = c(0, new_max_y))

    plot <- plt1 +
      scale_y_continuous(
        limits = c(-0.2, max_value + 0.05),
        #expand = c(0, 0),
        breaks = c((max_value*1/4), (max_value*2/4), (max_value*3/4), max_value)
      ) #+ expand_limits(y = c(0, new_max_y))
      return(plot)
  }
  
}