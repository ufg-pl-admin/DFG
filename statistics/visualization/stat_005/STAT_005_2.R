#' Tworzy obiekt data.frame z liczbą nabywców w podziale na miesiące
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwiema kolumnami:
#' "MRP" zawierającą tekstowe opisy grup MRP oraz "L_NABYWCOW" zawierającą liczbę nabywców
#' przypisanych do każdej grupy.
#'
#' @param json_content Obiekt JSON.
#' @return Obiekt data.frame z liczbą nabywców w podziale na miesiące.
#'
#' @examples
#' DATA_STAT_005_2(json_content)
#'
#' @export
DATA_STAT_005_2<- function(json_content) {

  
  colnames(json_content) <- c("MIESIAC", "L_NABYWCOW")
  df_main <- as.data.frame(json_content)
  df_main$L_NABYWCOW <- as.numeric(df_main$L_NABYWCOW)
  
  df <- df_main %>% group_by(MIESIAC) %>%
    mutate(KAT = case_when(
      str_detect(MIESIAC,'Styczeń') ~ 1,
      str_detect(MIESIAC,'Luty') ~ 2,
      str_detect(MIESIAC,'Marzec') ~ 3,
      str_detect(MIESIAC,'Kwiecień') ~ 4,
      str_detect(MIESIAC,'Maj') ~ 5,
      str_detect(MIESIAC,'Czerwiec') ~ 6,
      str_detect(MIESIAC,'Lipiec') ~ 7,
      str_detect(MIESIAC,'Sierpień') ~ 8,
      str_detect(MIESIAC,'Wrzesień') ~ 9,
      str_detect(MIESIAC,'Październik') ~ 10,
      str_detect(MIESIAC,'Listopad') ~ 11,
      str_detect(MIESIAC, "Grudzień") ~ 12
    ))
  
  df$MIESIAC <- tolower(df$MIESIAC)
  df <- df[order(df$KAT),]
  df$MIESIAC <- factor(df$MIESIAC,levels = df$MIESIAC[order(df$KAT)])
  return(df)
}


INFOGRAPHIC_STAT_005_2 <- function(json_content) {
  df <- DATA_STAT_005_2(json_content)
  
  df <- df[order(df$KAT,decreasing=TRUE),]

  int_breaks <- function(x, n = 5) {
    l <- pretty(x, n)
    l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
  }
  
  LABEL <- function(value) {
    if (value >= 1000) {
      amount <- format(value, format = "f", big.mark=" ")
    } else {
      amount <- format(value, format = "f", big.mark="")
    }
  }
  
  
  plot <- ggplot(df, aes(x=L_NABYWCOW, y=MIESIAC)) +
    geom_bar(stat="identity", fill="#D29082", width=.8) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(axis.text.y=element_text(size=10, family = "URWDIN-Regular", color="black"),
          axis.text.x=element_text(size=10, family = "URWDIN-Regular", color="black"),
          plot.title = element_text(hjust = 0.5, size = 18,margin=margin(0,0,18,0), family ="URWDIN-Demi"),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(df$L_NABYWCOW)+0.1*max(df$L_NABYWCOW))) + 
    scale_y_discrete(limits = levels(df$MIESIAC)) +
    geom_text(aes(label = sapply(L_NABYWCOW, LABEL)),  color = "black",  family ="URWDIN-Demi", hjust =-0.5, size=4.5)
  

  return(plot)
}