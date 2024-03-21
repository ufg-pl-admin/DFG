#' Tworzy obiekt data.frame z liczbą umów deweloperskich w podziale na miesiące
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwiema kolumnami:
#' "MRP" zawierającą tekstowe opisy grup MRP oraz "KAT" zawierającą liczbę umów deweloperskich
#' przypisanych do każdej grupy.
#'
#' @param json_content Obiekt JSON.
#' @return Obiekt data.frame z liczbą umów deweloperskich w podziale na miesiące.
#'
#' @examples
#' DATA_STAT_007_2(json_content)
#'
#' @export
DATA_STAT_007_2 <- function(json_content) {
  df_main <- getStat_007_prepared_data_2(json_content)

  df <- df_main %>% group_by(MIESIAC) %>%
    mutate(KAT = case_when(
      str_detect(MIESIAC,'styczeń') ~ 12,
      str_detect(MIESIAC,'luty') ~ 11,
      str_detect(MIESIAC,'marzec') ~ 10,
      str_detect(MIESIAC,'kwiecień') ~ 9,
      str_detect(MIESIAC,'maj') ~ 8,
      str_detect(MIESIAC,'czerwiec') ~ 7,
      str_detect(MIESIAC,'lipiec') ~ 6,
      str_detect(MIESIAC,'sierpień') ~ 5,
      str_detect(MIESIAC,'wrzesień') ~ 4,
      str_detect(MIESIAC,'październik') ~ 3,
      str_detect(MIESIAC,'listopad') ~ 2,
      str_detect(MIESIAC,'grudzień') ~ 1
    ))
  
  df_main <- df[order(df$KAT,decreasing=TRUE),]
  df$MIESIAC <- factor(df$MIESIAC,levels = df$MIESIAC[order(df$KAT)])

  df_main$Ad4 <- format(as.numeric(df_main$Ad4), big.mark=" ")
  df_main$Ad5 <- format(as.numeric(df_main$Ad5), big.mark=" ")
  df_main$Ad6 <- format(as.numeric(df_main$Ad6), big.mark=" ")
  
  return(df_main)
}

INFOGRAPHIC_STAT_007_2 <- function(json_content) {
  df_main_2 <- getStat_007_prepared_data_2(json_content)
  df_main_2 <- melt(df_main_2, id.vars = 'MIESIAC')
  df_main_2 <- df_main_2 %>%
    mutate(variable = recode(variable, Ad4 ="liczba umów deweloperskich objętych ochroną DFG",#= 'Liczba chronionych umów deweloperskich',
                             Ad5 ="liczba zawartych umów deweloperskich",#= 'Liczba podpisanych umów deweloperskich',
                             Ad6 ="liczba zakończonych umów deweloperskich"), ordered = TRUE)#=  'Liczba ochronionych umów deweloperskich' ))

  df_main_2$MIESIAC = factor(df_main_2$MIESIAC,
                     levels = c("grudzień","listopad","październik","wrzesień","sierpień","lipiec","czerwiec","maj","kwiecień","marzec","luty","styczeń"), ordered = TRUE)

  int_breaks <- function(x, n = 5) {
    l <- pretty(x, n)
    l[abs(l %% 1) < .Machine$double.eps ^ 0.5]
  }
  print(df_main_2)


  LABEL <- function(value) {
    ifelse(value >= 1000, format(value, big.mark = " "), as.character(value))
  }

  num_bars <- length(unique(df_main_2$MIESIAC))
  fixed_bar_width <- ifelse(num_bars < 2, 0.3, 0.8)

  position = position_dodge(width = fixed_bar_width)

 plot <- ggplot(df_main_2) +
   geom_bar(
     aes(x = fct_rev(MIESIAC), y = value, group = variable, fill = variable),
     stat = 'identity', position = position, alpha = 1, width = fixed_bar_width
   ) +
   geom_text(
     aes(x = fct_rev(MIESIAC), y = value, label = LABEL(value), fill = variable),
     hjust = -0.2, size = 2.5, color = 'black', family = 'URWDIN-Regular',
     position = position,
     inherit.aes = TRUE
   ) +
   xlab("") +
   ylab("") +
   coord_flip() +
   theme_bw() +
   theme(
     axis.text.y=element_text(size=10, color = "black", family = "URWDIN-Regular"),
      axis.text.x=element_text(size=10, color = "black", family = "URWDIN-Regular"),
      axis.line = element_line(colour = "black"),
      axis.title.y=element_text(angle = 0, face="bold", size=9),
      axis.title.x=element_text(hjust=1, face="bold", size=9),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position="bottom",
      legend.title = element_blank(),
      legend.text=element_text(size=8),
      plot.title = element_blank()) +
   scale_y_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(df_main_2$value)+0.1*max(df_main_2$value) +20)) +
   scale_x_discrete(expand = expansion(mult = c(0.08, 0))) +
   scale_fill_manual(values=c("#D29082", "#A7A8A9", "#666666"), guide = guide_legend())

  return(plot)
}