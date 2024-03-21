#' Tworzy obiekt data.frame z liczbą prowadzonych MRP w podziale na miesiące
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwiema kolumnami:
#' "MRP" zawierającą tekstowe opisy grup MRP oraz "NM5" zawierającą liczbę MRP
#' przypisanych do każdej grupy.
#'
#' @param json_content Obiekt JSON.
#' @return Obiekt data.frame z liczbą prowadzonych MRP w podziale na miesiące.
#'
#' @examples
#' DATA_STAT_002_4(json_content)
#'
#' @export
#' 
DATA_STAT_002_4 <- function(json_content) {

  colnames(json_content) <- c("MIESIAC", "NM5")
  df_main <- as.data.frame(json_content)
  df_main$NM5 <- as.numeric(df_main$NM5)
  
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
      str_detect(MIESIAC,'Grudzień') ~ 12
    ))
  
  df$MIESIAC <- tolower(df$MIESIAC)
  df <- df[order(df$KAT),]#,decreasing=TRUE
  df$MIESIAC <- factor(df$MIESIAC,levels = df$MIESIAC[order(df$KAT)])
  df$NM5[is.na(df$NM5)] <- 0
  
    return(df)
}

#' Tworzy wykres słupkowy liczby prowadzonych MRP w podziale na miesiące
#'
#' Funkcja tworzy i zwraca wykres słupkowy z liczbą banków przypisanych do każdej grupy MRP. Wykres przedstawia
#' miesiące na osi Y oraz liczbę MRP na osi X.
#'
#' @param json_content Obiekt JSON.
#' @return Obiekt ggplot reprezentujący wykres słupkowy z liczbą prowadzonych MRP w podziale na miesiące.
#'
#' @examples
#' INFOGRAPHIC_STAT_002_4(json_content)
#'
#' @importFrom ggplot2 ggplot aes xlab ylab geom_bar scale_x_continuous theme_bw
#' @importFrom ggplot2 theme element_text margin
#' @export
INFOGRAPHIC_STAT_002_4 <- function(json_content) {
  L_MRP <- DATA_STAT_002_4(json_content)
  
  L_MRP <- L_MRP[order(L_MRP$KAT,decreasing=TRUE),]
  
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
  
  plot <- ggplot(L_MRP, aes(x = NM5, y = MIESIAC)) +
    #ggtitle("LICZBA PROWADZONYCH MRP W PODZIALE NA MIESIĄCE") +
    geom_bar(stat="identity", position = 'dodge', alpha=1, width=.8, fill = "#D29082") +
    xlab("") + 
    ylab("") +
    theme_bw() +
    theme(axis.text.y=element_text(size=10, color = "black", family = "URWDIN-Regular"),
          axis.text.x=element_text(size=10, color = "black", family = "URWDIN-Regular"),
          axis.line = element_line(colour = "black"),
          axis.title.y=element_text(angle = 0, face="bold", size=9),
          axis.title.x=element_text(hjust=1, face="bold", size=9),
          panel.border = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          #legend.position="bottom",
          legend.title = element_blank(),
          plot.title = element_blank()) +#element_text(face="bold", hjust = 0.5, size = 18, margin=margin(0,0,18,0), family ="URWDIN-Demi")) +
    scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(L_MRP$NM5)+0.1*max(L_MRP$NM5))) +
    scale_y_discrete(limits = levels(L_MRP$NM5))  +
    geom_text(aes(label = sapply(NM5, LABEL)),  color = "black",  family ="URWDIN-Demi", hjust =-0.5)
    # scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(L_MRP$NM5)+0.1*max(L_MRP$NM5))) +
    # geom_text(aes(label = sapply(NM5, LABEL)),
    #           color = ifelse(L_MRP$NM5 / max(L_MRP$NM5) < 0.045, "black", "white"),
    #           family = "URWDIN-Demi",
    #           hjust = ifelse(L_MRP$NM5 / max(L_MRP$NM5) < 0.045, 0, 0.5),
    #           nudge_x = ifelse((L_MRP$NM5 / max(L_MRP$NM5) < 0.045), 0.01 * max(L_MRP$NM5), -0.03 * max(L_MRP$NM5))
    # )
     return(plot)
}