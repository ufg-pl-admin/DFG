#' Tworzy obiekt data.frame z liczbą uruchomień reguł walidacyjnych
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwiema kolumnami i wierszami,
#' zawierającymi liczbę wystąpień błędów per typ zasilenia.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP002B_prepared_data_4.
#' @return Obiekt data.frame z liczbą uruchomień reguł walidacyjnych.
#'
#' @examples
#' DATA_RAP002B_4(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP002B_prepared_data_4
#' @export

DATA_RAP002B_4 <- function(json_content) {
  df_main_2 <- getRAP002B_prepared_data_4(json_content)
  colnames(df_main_2) <- c("TYP_ZASILENIA", "LICZBA_BLADOW")
  
  return(df_main_2)
}

#' Tworzy obiekt data.frame z liczbą uruchomień reguł walidacyjnych
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwiema kolumnami i wierszami,
#' zawierającymi liczbę wystąpień błędów per typ zasilenia.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP002B_prepared_data_4.
#' @return Obiekt ggplot z wykresem radarowym opisującym liczbę błędów.
#'
#' @examples
#' INFOGRAPHIC_RAP002B_4(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP002B_prepared_data_4
#' @export
INFOGRAPHIC_RAP002B_4 <- function(json_content) {
  df_main_2 <-  getRAP002B_prepared_data_4(json_content)
  
  colnames(df_main_2) <- c("TYP_ZASILENIA", "LICZBA_BLADOW")
  df <- df_main_2 %>%
    mutate(Percent=LICZBA_BLADOW/sum(LICZBA_BLADOW)) %>%
    arrange(desc(LICZBA_BLADOW))
  
  df$colors <-  LETTERS[seq_len(nrow(df))]
  
  colors = c('A'='#921B02','B'='#B43C23', 'C'='#D9553B','D'='#E27C68','E'='#D29082', 
             'F'='#EBA799','G'='#F2C8C0', 'H'='#F5E5E3','I'='#F9F0EC','J'='#FFFFFF', 
             'K'='#F6F6F6','L'='#EBEBEB', 'M'='#DDDDDD','N'='#A7A8A9','O'='#757575',
             'P'='#4F4F4F','Q'='#000000', 'R'='#199CB5','S'='#09599B')
  
  df <- df %>%
    arrange(desc(colors)) %>%
    mutate(
      Percent_cum = cumsum(Percent),
      y = .5 * (Percent_cum + lag(Percent_cum, default = 0))
    )
  
  max_value <- max(df$Percent, na.rm = TRUE)
  
  PIEDONUT_TYP_ZASILENIA <- if (length(unique(df$TYP_ZASILENIA)) == 1) {
    ggplot(data=df,aes(x=TYP_ZASILENIA,y=Percent,fill=colors))+
      geom_bar(stat="identity",width=1,colour="#921B02")+
      coord_polar(clip="off")+
      geom_segment(
        aes(
          x = reorder(TYP_ZASILENIA, -Percent),
          y = 0,
          xend = reorder(TYP_ZASILENIA, -Percent),
          yend = ifelse(length(unique(TYP_ZASILENIA)) != 1, max(Percent), 1),#max(Percent)
          group = colors
        ),
        linetype = "dashed",
        color = "gray12"
      ) +
      geom_point(
        aes(
          x = reorder(TYP_ZASILENIA, -Percent),
          y = 0
        ),
        size = 3,
        color = "#B43C23"
      ) +
      geom_label(
        aes(
          x = reorder(TYP_ZASILENIA, -Percent),
          y = ifelse(length(unique(TYP_ZASILENIA)) != 1, max(Percent), 1),#max(Percent)
          label = paste0(round((Percent * 100),2), "%")
        ),
        position = position_stack(vjust = 0.5),
        fill = alpha("white", 0),
        label.size = 0,
        hjust = 0,
        angle = 0,
        color = "black",
        fontface = "bold"
      ) +
      geom_text(aes(label = TYP_ZASILENIA), position = position_stack(vjust = 1), vjust = 1.6) +
      xlab("")+
      ylab("") +
      theme(
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black", family = "URW DIN-REGULAR", face = "plain", size = 13),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0)
      ) +
      theme(legend.position = "none") +
      scale_fill_manual(values = colors, name = "TYP_ZASILENIA", labels = df$TYP_ZASILENIA) +
      scale_y_continuous(
        limits = c(-0.2, max_value),
        expand = c(0, 0),
        breaks = c((max_value*1/4), (max_value*2/4), (max_value*3/4), max_value)
      )
    
  } else if (length(json_content) == 0){  
    
    ggplot(data=df, aes(x=0, y=0)) +
      geom_hline(
        aes(yintercept = y),
        data = data.frame(y = c((100*1/4), (100*2/4), (100*3/4), 100)),
        color = "lightgrey"
      ) +
      coord_polar(start = pi, clip="off") +
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
                vjust = 0.5, hjust = 0.5, color = "black", family = "URW DIN-REGULAR", size = 6) 
    
  } else {
    
    ggplot(data = df[which(df$LICZBA_BLADOW>0),], fill = colors) +
      geom_hline(
        aes(yintercept = y),
        data = data.frame(y = c((max_value*1/4), (max_value*2/4), (max_value*3/4), max_value)),
        color = "lightgrey"
      ) +
      geom_col(
        aes(
          x = reorder(TYP_ZASILENIA, -Percent),
          y = Percent,
          fill = colors
        ),
        position = "identity",#"dodge2",
        show.legend = TRUE,
        alpha = .9
      ) +
      geom_point(
        aes(
          x = reorder(TYP_ZASILENIA, -Percent),
          y = 0
        ),
        size = 3,
        color = "#B43C23"
      ) +
      geom_segment(
        aes(
          x = reorder(TYP_ZASILENIA, -Percent),
          y = 0,
          xend = reorder(TYP_ZASILENIA, -Percent),
          yend = ifelse(length(unique(TYP_ZASILENIA)) != 1, max(Percent), 1),#max(Percent)
          group = colors
        ),
        linetype = "dashed",
        color = "gray12"
      ) +
      geom_label(
        aes(
          x = reorder(TYP_ZASILENIA, -Percent),
          y = ifelse(length(unique(TYP_ZASILENIA)) != 1, max(Percent), 1),#max(Percent)
          label = paste0(round((Percent * 100),2), "%")
        ),
        position = position_stack(vjust = 0.5),
        fill = alpha("white", 0),
        label.size = 0,
        hjust = 0,
        angle = 0,
        color = "black",
        fontface = "bold"
      ) +
      coord_polar(start = ifelse(length(unique(df$TYP_ZASILENIA)) != 1, -pi/2, -pi), clip="off") +
      theme(
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black", family = "URW DIN-REGULAR", face = "plain", size = 13),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0)
      ) +
      theme(legend.position = "none") +
      scale_fill_manual(values = colors, name = "TYP_ZASILENIA", labels = df$TYP_ZASILENIA) +
      scale_y_continuous(
        limits = c(-0.2, max_value),
        expand = c(0, 0),
        breaks = c((max_value*1/4), (max_value*2/4), (max_value*3/4), max_value)
      )
    
  }
  return(PIEDONUT_TYP_ZASILENIA)
}
