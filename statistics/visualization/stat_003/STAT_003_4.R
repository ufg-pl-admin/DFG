#' Tworzy obiekt data.frame z liczbą założonych MRP w podziale na miesiące
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwiema kolumnami:
#' "MRP" zawierającą tekstowe opisy grup MRP oraz "L_MRP" zawierającą liczbę MRP
#' przypisanych do każdej grupy.
#'
#' @param json_content Obiekt JSON.
#' @return Obiekt data.frame z liczbą założonych MRP w podziale na miesiące.
#'
#' @examples
#' DATA_STAT_003_4(json_content)
#'
#' @export
DATA_STAT_003_4 <- function(json_content) {
  df_main <- getStat_003_prepared_data_3(json_content)
  df_main <- df_main[order(df_main$KAT),]
  return(df_main)
}

INFOGRAPHIC_STAT_003_4 <- function(json_content) {
  df_main_2 <- getStat_003_prepared_data_3(json_content)
  
  df_main_2 <- df_main_2[order(df_main_2$KAT),]
  df_main_2$MIESIAC <- factor(df_main_2$MIESIAC, levels = df_main_2$MIESIAC[order(df_main_2$KAT)])
  
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
  
  plot <- ggplot(df_main_2, aes(x = L_MRP, y = MIESIAC)) +
    geom_bar(stat="identity", position = 'dodge', alpha=1, width=.8, fill = "#D29082") +
    xlab("") + 
    ylab("") +
    theme_bw() +
    theme(axis.text.y=element_text(color = "black", family = "URWDIN-Regular", size = 12),
          axis.text.x=element_text(color = "black", family = "URWDIN-Regular", size = 10),
          axis.line = element_line(colour = "black"),
          axis.title.y=element_text(angle = 0, face="bold", size=9),
          axis.title.x=element_text(hjust=1, face="bold", size=9),
          panel.border = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          plot.title = element_blank()) +
    scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(df_main_2$L_MRP)+0.1*max(df_main_2$L_MRP))) +
    scale_y_discrete(limits = levels(df_main_2$L_MRP))  +
    geom_text(aes(label = sapply(L_MRP, LABEL)),  color = "black",  family ="URWDIN-Demi", hjust =-0.5)

  return(plot)
}