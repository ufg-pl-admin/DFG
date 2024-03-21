#' Tworzy obiekt data.frame z liczbą uruchomień reguł walidacyjnych i liczbę korekt
#'
#' Funkcja tworzy i zwraca obiekt data.frame z trzema kolumnami i wierszami,
#' zawierającymi liczbę uruchomień reguł walidacyjnych per nazwę reguły.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP002_prepared_data_5.
#' @return Obiekt data.frame z liczbą uruchomień reguł walidacyjnych.
#'
#' @examples
#' DATA_RAP002_5(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP002_prepared_data_5
#' @export
DATA_RAP002_5 <- function(json_content) {
  df_main_2 <- getRAP002_prepared_data_5(json_content)

  colnames(df_main_2) <- c("REGULA", "LICZBA_URUCHOMIEN", "LICZBA_KOREKT") 

  return(df_main_2)
}

#' Tworzy obiekt data.frame z liczbą uruchomień reguł walidacyjnych
#'
#' Funkcja tworzy i zwraca obiekt data.frame z trzema kolumnami i wierszami,
#' zawierającymi liczbę korekt i uruchomień reguł.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP002_prepared_data_5.
#' @return Obiekt ggplot z wykresem liniowym opisującym 25 reguł walidacyjnych pod względem liczbę korekt i uruchomień reguły.
#'
#' @examples
#' INFOGRAPHIC_RAP002_5(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP002_prepared_data_5
#' @export
source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))
source(paste(getwd(), '/utils/label2Util.R', sep=""))

INFOGRAPHIC_RAP002_5 <- function(json_content) {
  df_main_2 <- getRAP002_prepared_data_5(json_content)

  df_main_2 <- df_main_2[with(df_main_2,order(L_URUCHOMIEN, L_SKORYGOWANYCH, decreasing = TRUE)),]
  
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

  uruchomien_ostrzezen <- melt(df_main_2, id.vars = 'NAZWA_REGULY')

  uruchomien_ostrzezen <- uruchomien_ostrzezen %>%
    mutate(variable = recode(variable, L_URUCHOMIEN = 'Liczba uruchomień reguł walidacyjnych', 
                             L_SKORYGOWANYCH = 'Liczba korekt')) 
  
 vjust_L_regul <- ifelse(nrow(df_main_2) == 25, -0.2, ifelse(nrow(df_main_2) == 20, -0.2, 
                                                               ifelse(nrow(df_main_2) == 15, -0.5, ifelse(nrow(df_main_2) == 10, -0.8, 
                                                                                                          ifelse(nrow(df_main_2) <= 5, -2.2, NA)))))
 vjust_L_korekt <- ifelse(nrow(df_main_2) == 25, 1.2, ifelse(nrow(df_main_2) == 20, 1.2, 
                                                             ifelse(nrow(df_main_2) == 15, 1.5, ifelse(nrow(df_main_2) == 10, 1.8, 
                                                                                                       ifelse(nrow(df_main_2) <= 5, 3.2, NA)))))
  plot <- if(length(json_content) == 0) { 
  
    ggplot(uruchomien_ostrzezen, aes(x=0, y=0)) +
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
    
    ggplot(uruchomien_ostrzezen, aes(x = value, y = NAZWA_REGULY, fill = variable)) +
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
    scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(uruchomien_ostrzezen$value)+0.1*max(uruchomien_ostrzezen$value))) +
    scale_fill_manual(values=c("#B43C23", "#757575")) +
    theme(axis.text = element_text(family = "URW DIN-REGULAR")) +
      geom_text(data = uruchomien_ostrzezen[uruchomien_ostrzezen$variable == "Liczba korekt", ],
                aes(label = LABEL2(value)),
                color = "black",  family ="URWDIN-Demi", vjust = vjust_L_korekt, hjust = -0.5) +
      geom_text(data = uruchomien_ostrzezen[uruchomien_ostrzezen$variable == "Liczba uruchomień reguł walidacyjnych", ],
                aes(label = LABEL2(value)),
                color = "black",  family ="URWDIN-Demi", vjust = vjust_L_regul, hjust = -0.5)
  }
    
  return(plot)
}