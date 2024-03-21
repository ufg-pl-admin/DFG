#' Tworzy obiekt data.frame z liczbą inwestycji wg daty otwarcia w podziale na miesiące
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwiema kolumnami:
#' "MRP" zawierającą tekstowe opisy grup MRP oraz "NM3" zawierającą liczbę inwestycji wg daty otwarcia
#' przypisanych do każdej grupy.
#'
#' @param json_content Obiekt JSON.
#' @return Obiekt data.frame z liczbą inwestycji wg daty otwarcia w podziale na miesiące.
#'
#' @examples
#' DATA_STAT_008_2(json_content)
#'
#' @export
source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))
source(paste(getwd(), '/utils/labelUtil.R', sep=""))

DATA_STAT_008_2 <- function(json_content) {
  df_main <- getStat_008_prepared_data_2(json_content)
  df_main <- df_main[order(df_main$KAT),]
  return(df_main)
}

INFOGRAPHIC_STAT_008_2 <- function(json_content) {
  df_main_2 <- getStat_008_prepared_data_2(json_content)

  df_main_2 <- df_main_2[order(df_main_2$KAT),]
  df_main_2$MIESIAC <- factor(df_main_2$MIESIAC, levels = df_main_2$MIESIAC[order(df_main_2$KAT)])

  middle_date <- as.character(df_main_2$MIESIAC[ceiling(length(df_main_2$MIESIAC) / 2)])

  
  plot <- if (all(df_main_2$NM3 == 0)) { 
    
    ggplot(df_main_2, aes(x = NM3, y = MIESIAC)) +
      geom_bar(stat="identity", position = 'dodge', alpha=1, width=.8, fill = "#D29082") +
      xlab("") + 
      ylab("") +
      theme_bw() +
      theme(axis.text.y=element_text(size=10, color = "black", family = "URWDIN-Regular"),
            axis.text.x=element_text(size=10, color = "black",family = "URWDIN-Regular"),
            axis.line = element_line(colour = "black"),
            axis.title.y=element_text(angle = 0, face="bold", size=9),
            axis.title.x=element_text(hjust=1, face="bold", size=9),
            panel.border = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            plot.title = element_blank()) +#element_text(face="bold", hjust = 0.5, size = 18, margin=margin(0,0,18,0), family ="URWDIN-Demi")) +
      scale_x_continuous(breaks = int_breaks, expand = c(0,0), limits=c(0,max(df_main_2$NM3)+0.1*max(df_main_2$NM3))) +
      scale_y_discrete(limits = levels(df_main_2$NM3)) +
      geom_text(data = df_main_2, aes(x = 0, y=middle_date, label = "Brak danych dla wybranych parametrów filtrowania."),
                vjust = 0.5, hjust = 0.5, color = "black", family = "URWDIN-Regular", size = 6)
    
  } else {

    ggplot(df_main_2, aes(x = NM3, y = MIESIAC)) +
    geom_bar(stat="identity", position = 'dodge', alpha=1, width=.8, fill = "#D29082") +
    xlab("") + 
    ylab("") +
    theme_bw() +
    theme(axis.text.y=element_text(size=10, color = "black", family = "URWDIN-Regular"),
          axis.text.x=element_text(size=10, color = "black",family = "URWDIN-Regular"),
          axis.line = element_line(colour = "black"),
          axis.title.y=element_text(angle = 0, face="bold", size=9),
          axis.title.x=element_text(hjust=1, face="bold", size=9),
          panel.border = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          plot.title = element_blank()) +#element_text(face="bold", hjust = 0.5, size = 18, margin=margin(0,0,18,0), family ="URWDIN-Demi")) +
    scale_x_continuous(breaks = int_breaks, expand = c(0,0), limits=c(0,max(df_main_2$NM3)+0.1*max(df_main_2$NM3))) +
    scale_y_discrete(limits = levels(df_main_2$NM3))  +
    geom_text(aes(label = sapply(NM3, LABEL)),  color = "black",  family ="URWDIN-Demi", hjust =-0.5)
  }
  
  return(plot)
}