#' Tworzy obiekt data.frame z liczbą inwestycji i umów deweloperskich w podziale na województwa
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwiema kolumnami:
#' "L_UMOW" zawierającą grupy standaryzowanych nazw województw oraz "L_UMOW" zawierającą liczbę inwestycji
#' przypisanych do każdej grupy.
#'
#' @param json_content Obiekt JSON.
#' @return Obiekt data.frame z liczbą inwestycji i umów deweloperskich w podziale na województwa.
#'
#' @examples
#' DATA_STAT_009_3(json_content)
#'
#' @export
DATA_STAT_009_3 <- function(json_content) {
  df_main <- getStat_009_prepared_data_3(json_content)
  df_main <- df_main %>% arrange(desc(L_UMOW))
  
  return(df_main)
}

INFOGRAPHIC_STAT_009_3 <- function(json_content) {
  df <- DATA_STAT_009_3(json_content)
  
  
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
  
  
  plot <- ggplot(df, aes(x=L_UMOW, y=reorder(WOJEWODZTWO, L_UMOW))) +
    #ggtitle("LICZBA BANKÓW WG LICZBY PROWADZONYCH MRP") +
    geom_bar(stat="identity", position = 'dodge', alpha=1, width=.8, fill="#D29082") +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(axis.text.y=element_text(family = "URWDIN-Regular", color="black", size = 12),
          axis.text.x=element_text(family = "URWDIN-Regular", color="black", size = 10),
          axis.line = element_line(colour = "black"),
          # axis.title.y=element_text(angle = 0, face="bold", size=9, margin = margin(r = -40)),
          # axis.title.x=element_text(hjust=1, face="bold", size=9),
          #plot.title = element_text(hjust = 0.5, size = 18,margin=margin(0,0,18,0), family ="URWDIN-Demi"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = int_breaks, expand = c(0, 0), labels = number_format(), limits = c(0, max(df$L_UMOW) + 0.1 * max(df$L_UMOW))) +
    scale_y_discrete(limits = levels(df$L_UMOW))  +
    geom_text(aes(label = sapply(L_UMOW, LABEL)), color = "black", family = "URWDIN-Demi", hjust = -0.5)
  
  
  
  # scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format()) +
  #   scale_y_discrete(limits = levels(df$MRP)) +
  
  # hjust = ifelse(df$WOJEWODZTWO<=0,0,1),  nudge_x = ifelse((max(df$WOJEWODZTWO)-df$WOJEWODZTWO>100 & df$WOJEWODZTWO<=9)
  #                                            | (max(df$WOJEWODZTWO)-df$WOJEWODZTWO>1000 & df$WOJEWODZTWO<=99),
  #                                            0,-1*0.01*max(df$WOJEWODZTWO))
  return(plot)
}