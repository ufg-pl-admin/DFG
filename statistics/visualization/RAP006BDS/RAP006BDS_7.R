#' Tworzy obiekt data.frame z podsumowaniem liczby rekordów zasileń, w których wystąpiło ostrzeżenie per reguła
#'
#' Funkcja tworzy i zwraca obiekt data.frame z trzema kolumnami,
#' zawierającymi miary łącznej liczby rekordów zasileń, w których wystąpiło ostrzeżenie zgrupowanych do reguł walidacyjnych.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_7.
#' @return Obiekt data.frame z liczbą rekordów zasileń, w których wystąpiło ostrzeżenie per reguła.
#'
#' @examples
#' DATA_RAP006BDS_7(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP006BDS_prepared_data_7
#' @export
DATA_RAP006BDS_7 <- function(json_content) {
  df_main_2 <- getRAP006BDS_prepared_data_7(json_content)

  return(df_main_2)
}

#' Tworzy obiekt data.frame z podsumowaniem liczby rekordów zasileń, w których wystąpiło ostrzeżenie per reguła
#'
#' Funkcja tworzy i zwraca obiekt data.frame z trzema kolumnami,
#' zawierającymi miary łącznej liczby rekordów zasileń, w których wystąpiło ostrzeżenie zgrupowanych do reguł walidacyjnych.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP006BDS_prepared_data_7.
#' @return Obiekt ggplot z wykresem liniowym podsumowanie zrealizowanych zasileń.
#'
#' @examples
#' INFOGRAPHIC_RAP006BDS_7(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP006BDS_prepared_data_7
#' @export
source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))
source(paste(getwd(), '/utils/label2Util.R', sep=""))

INFOGRAPHIC_RAP006BDS_7 <- function(json_content) {
  df_main_2 <- getRAP006BDS_prepared_data_7(json_content)
  
  df_main_2 <- df_main_2[with(df_main_2,order(LICZBA_ZASILEN, LICZBA_OSTRZEZEN, decreasing = TRUE)),]

     if (nrow(df_main_2) >= 25) {
    df_main_2 <- df_main_2[1:25, ]
  } else if (nrow(df_main_2) >= 20) {
    df_main_2 <- df_main_2[1:20, ]
  }else if (nrow(df_main_2) >= 15) {
    df_main_2 <- df_main_2[1:15, ]
  } else if (nrow(df_main_2) >= 10) {
    df_main_2 <- df_main_2[1:10, ]
  } else if (nrow(df_main_2) >= 5) {
    df_main_2 <- df_main_2[1:5, ]
  }

  wystapienia_ostrzezen <- melt(df_main_2, id.vars = 'REGULA')
  wystapienia_ostrzezen <- wystapienia_ostrzezen %>%
    mutate(variable = recode(variable, LICZBA_ZASILEN = 'Liczba rekordów zasileń', 
                             LICZBA_OSTRZEZEN = 'Liczba wystąpień ostrzeżeń'))
  
   vjust_L_regul <- ifelse(nrow(df_main_2) == 25, -0.25, ifelse(nrow(df_main_2) == 20, -0.3,
                                                               ifelse(nrow(df_main_2) == 15, -0.5, ifelse(nrow(df_main_2) == 10, -0.8,
                                                                                                          ifelse(nrow(df_main_2) <= 5, -2.2, NA)))))
  vjust_L_korekt <- ifelse(nrow(df_main_2) == 25, 1, ifelse(nrow(df_main_2) == 20, 1.2,
                                                             ifelse(nrow(df_main_2) == 15, 1.5, ifelse(nrow(df_main_2) == 10, 1.8,
                                                                                                       ifelse(nrow(df_main_2) <= 5, 3.2, NA)))))
  
  plot <- if(length(json_content) == 0) { 
    
    ggplot(wystapienia_ostrzezen, aes(x=0, y=0)) +
      xlab("") +
      ylab("") +
      theme_bw() +
      theme(axis.text.y=element_blank(),
            axis.text.x=element_blank(),
            axis.line = element_line(colour = "black"),
            axis.title.y=element_text(angle = 0, face="bold", size=9),
            axis.title.x=element_text(hjust=1, face="bold", size=9),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            plot.title = element_blank()) +
      labs(fill=NULL) +
      geom_text(aes(label = "Brak danych dla wybranych parametrów filtrowania."),
                vjust = 0.5, hjust = 0.5, color = "black", family = "URW DIN-REGULAR", size = 5)
    
  } else { 
    
    ggplot(wystapienia_ostrzezen, aes(x = value, y = REGULA, fill = variable)) +
    geom_bar(stat="identity", position = 'dodge', alpha=1, width=.8) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(axis.text.y=element_text(face="bold", color = "black", size=9),
          axis.text.x = element_text(color = "black", size=11),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 16)
    ) +
    scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(wystapienia_ostrzezen$value)+0.1*max(wystapienia_ostrzezen$value))) +
    scale_fill_manual(values=c("#B43C23", "#757575")) +
    theme(axis.text = element_text(family = "URWDIN-Regular")) +
      geom_text(data = wystapienia_ostrzezen[wystapienia_ostrzezen$variable == "Liczba rekordów zasileń", ],
                aes(label = LABEL2(value)),
                color = "black",  family ="URWDIN-Demi", vjust = vjust_L_korekt, hjust = -0.5) +
      geom_text(data = wystapienia_ostrzezen[wystapienia_ostrzezen$variable == "Liczba wystąpień ostrzeżeń", ],
                aes(label = LABEL2(value)),
                color = "black",  family ="URWDIN-Demi", vjust = vjust_L_regul, hjust = -0.5)
  }
  
return(plot)
}