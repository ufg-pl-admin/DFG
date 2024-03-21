#' Tworzy obiekt data.frame z miarami spraw niezgodności w podziale na statusy
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i siedmioma kolumnami,
#' zawierającymi miary spraw niezgodności w podziale na statusy.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP007_prepared_data_3.
#' @return Obiekt data.frame z miarami spraw niezgodności w podziale na statusy.
#'
#' @examples
#' DATA_RAP007_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP007_prepared_data_3
#' @export
source(paste(getwd(), '/utils/int_breaksUtil.R', sep=""))
source(paste(getwd(), '/utils/labelUtil.R', sep=""))  

DATA_RAP007_3 <- function(json_content) {
  df_main_2 <- getRAP007_prepared_data_2(json_content)
  colnames(df_main_2) <- c("L_WWER",
                           "L_ROZW",
                           "L_ODRZ",
                           "L_DOPR",
                           "L_DODOPR",
                           "L_REKL",
                           "L_ZAMK",
                           "L_AUTZAMK")
  df <- data.frame(C1 = c("Liczba spraw niezgodności o statusie W weryfikacji",
                          "Liczba spraw niezgodności o statusie Rozwiązana",
                          "Liczba spraw niezgodności o statusie Odrzucona",
                          "Liczba spraw niezgodności o statusie Doprecyzowana",
                          "Liczba spraw niezgodności o statusie Do doprecyzowania",
                          "Liczba spraw niezgodności o statusie Reklamacja",
                          "Liczba spraw niezgodności o statusie Zamknięta",
                          "Liczba spraw niezgodności o statusie Zamknięta automatycznie"),
                   C2 = c(df_main_2$L_WWER,
                          df_main_2$L_ROZW,
                          df_main_2$L_ODRZ,
                          df_main_2$L_DOPR,
                          df_main_2$L_DODOPR,
                          df_main_2$L_REKL,
                          df_main_2$L_ZAMK,
                          df_main_2$L_AUTZAMK))
  
  return(df_main_2)
}

#' Tworzy obiekt data.frame z miarami spraw niezgodności w podziale na statusy
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i siedmioma kolumnami,
#' zawierającymi miary spraw niezgodności w podziale na statusy.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP007_prepared_data_3.
#' @return Obiekt ggplot prezentujący miary spraw niezgodności w podziale na statusy.
#'
#' @examples
#' INFOGRAPHIC_RAP007_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP007_prepared_data_3
#' @export
INFOGRAPHIC_RAP007_3 <- function(json_content) {
  df_main_2 <-  getRAP007_prepared_data_2(json_content)

  df <- setNames(data.frame(t(as.matrix(df_main_2))), as.character(df_main_2[[1]]))
  df <- tibble::rownames_to_column(df, "STATUS")
  
  colnames(df) <- c("STATUS", "LICZBA")

  df$STATUS <- factor(df$STATUS, levels = df$STATUS)

  plot <- if (all(df$LICZBA == 0)) {
    
    ggplot(df, aes(x=STATUS, y=LICZBA, fill=STATUS)) +
      geom_bar(stat='identity', position='dodge', show.legend = FALSE) +
      theme_bw() +
      theme(panel.border = element_blank(),
            legend.title = element_blank(),
            axis.line = element_line(colour = "grey"),
            plot.title = element_blank()) +
      ylab(" ") +
      xlab(" ") +
      scale_fill_manual(values=c('#B43C23','#B43C23','#B43C23','#B43C23','#B43C23','#B43C23','#B43C23','#B43C23')) +
      scale_x_discrete(labels= c('W weryfikacji', 'Rozwiązana', 'Odrzucona', 'Doprecyzowana', 'Do\ndoprecyzowania', 'Reklamacja', 'Zamknięta', 'Zamknięta\nautomatycznie'), expand = c(0, 0)) +
      theme(plot.title = element_blank()) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(axis.text.y=element_text(color = "black", family = "URWDIN-Regular"),
            axis.text.x=element_text(color = "black", family = "URWDIN-Regular"),
            axis.title.y=element_text(angle = 0, face="bold", size=9),
            axis.title.x=element_text(hjust=1, face="bold", size=9)) +
      geom_text(x = 4, y = 0, label = "Brak danych dla wybranych parametrów filtrowania.", 
                color = "black", family = "URW DIN-REGULAR", size = 4)
    
  } else {
    
    ggplot(df, aes(x=STATUS, y=LICZBA, fill=STATUS)) +
    geom_bar(stat='identity', position='dodge', show.legend = FALSE) +
    theme_bw() +
    theme(panel.border = element_blank(),
          legend.title = element_blank(),
          axis.line = element_line(colour = "grey"),
          plot.title = element_blank()) +
    ylab(" ") +
    xlab(" ") +
    scale_fill_manual(values=c('#B43C23','#B43C23','#B43C23','#B43C23','#B43C23','#B43C23','#B43C23','#B43C23')) +
      scale_x_discrete(labels= c('W weryfikacji', 'Rozwiązana', 'Odrzucona', 'Doprecyzowana', 'Do\ndoprecyzowania', 'Reklamacja', 'Zamknięta', 'Zamknięta\nautomatycznie'), expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0),
                         breaks = int_breaks,
                         limits=c(0,max(df$LICZBA)+0.1*max(df$LICZBA))) +
    theme(axis.text.y=element_text(color = "black", family = "URWDIN-Regular"),
          axis.text.x=element_text(color = "black", family = "URWDIN-Regular"),
          axis.title.y=element_text(angle = 0, face="bold", size=9),
          axis.title.x=element_text(hjust=1, face="bold", size=9)) +
      geom_text(aes(label = LICZBA),
                color = "black",  family ="URWDIN-Demi", vjust =-0.5)
  
  }
  
  return(plot)
}