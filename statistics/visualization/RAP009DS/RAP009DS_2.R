#' Tworzy obiekt data.frame z liczbą uruchomień anomalii w podziale na usługi
#'
#' Funkcja tworzy i zwraca obiekt data.frame z szesnastoma kolumnami,
#' zawierającymi liczbę uruchomień anomalii w podziale na usługi.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP009DS_prepared_data_2.
#' @return Obiekt data.frame z liczbą uruchomień anomalii.
#'
#' @examples
#' DATA_RAP009DS_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP009DS_prepared_data_2
#' @export

DATA_RAP009DS_2 <- function(json_content) {
  
  df_main <- getRap_009DS_2_prepared_data(json_content)
  
  return(df_main)
}

#' Tworzy obiekt data.frame z liczbą uruchomień anomalii w podziale na usługi
#'
#' Funkcja tworzy i zwraca obiekt data.frame z szesnastoma kolumnami,
#' zawierającymi liczbę uruchomień anomalii w podziale na usługi.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP009DS_prepared_data_2.
#' @return Obiekt ggplot z liniowym wykresem opisującym liczbę uruchomień anomalii w podziale na usługi.
#'
#' @examples
#' INFOGRAPHIC_RAP009DS_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP009DS_prepared_data_2
#' @export
INFOGRAPHIC_RAP009DS_2 <- function(json_content) {
  df_main <- DATA_RAP009DS_2(json_content)
  
  
  
  plot <- if (length(unique(df_main$DATA_ANOMALII)) == 1) { 
    ggplot(data = df_main, mapping = aes(x = DATA_ANOMALII)) +
      #  geom_line(data = df_main, mapping = aes(y = ZAWMRP, group = 1, color = "ZAWMRP"), lwd = 0.9) +
      geom_point(data = df_main, mapping = aes(y = ZAWMRP, color = "ZAWMRP"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = ZAUMDE, color = "ZAUMDE"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = ZMUMDE, color = "ZMUMDE"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = WPLMRP, color = "WPLMRP"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = DANEAN, color = "DANEAN"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = INSKLA, color = "INSKLA"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = WYTMRP, color = "WYTMRP"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = WSTRZD, color = "WSTRZD"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = PRZEKS, color = "PRZEKS"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = ZAMMRP, color = "ZAMMRP"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = ROUMDE, color = "ROUMDE"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = UPAPOS, color = "UPAPOS"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = WYPMRP, color = "WYPMRP"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = ZRWNAB, color = "ZRWNAB"), size = 3) +
      geom_point(data = df_main, mapping = aes(y = UPADEW, color = "UPADEW"), size = 3) +
      # ggtitle("LICZBA ANOMALII W PODZIALE NA USŁUGI") +
      xlab("") + 
      ylab("") +
      ylim(0, max(df_main$ZAWMRP,
            df_main$ZAUMDE,
            df_main$ZMUMDE,
            df_main$WPLMRP,
            df_main$DANEAN,
            df_main$INSKLA,
            df_main$WYTMRP,
            df_main$WSTRZD,
            df_main$PRZEKS,
            df_main$ZAMMRP,
            df_main$ROUMDE,
            df_main$UPAPOS,
            df_main$WYPMRP,
            df_main$ZRWNAB,
            df_main$UPADEW,
            3)) +
      theme_bw() +
      scale_x_discrete(expand = c(0, 0)) +
      scale_color_manual(values = c("ZAWMRP" = "#09599B", "ZAUMDE" = "#199CB5", "ZMUMDE" = "#B43C23", "WPLMRP" = "#EFD7D2", "DANEAN" = "#D9553B", "INSKLA" = "#000000",
                                    "WYTMRP" = "#57CC00", "WSTRZD" = "#B40D93", "PRZEKS" = "#93EADE", "ZAMMRP" = "#A7A8A9", "ROUMDE" = "#921B02", "UPAPOS" = "#D29082",
                                    "WYPMRP" = "#757575", "ZRWNAB" = "#EC3038", "UPADEW" = "#F09BFC")) +
      theme(
        legend.position="bottom",
        legend.title = element_blank(),
        axis.text.y=element_text(family = "URWDIN-Regular", color="black"),
        axis.text.x=element_text(family = "URWDIN-Regular", color="black"),
        legend.text = element_text(family = "URWDIN-Regular", color="black"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0, 25, 0, 0))
    
  } else if(length(json_content) == 0) { 
    
    ggplot(data = df_main, aes(x=0, y=0)) +
      xlab("") +
      ylab("") +
      theme_bw() +
      theme(
        legend.position="bottom",
        legend.title = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(family = "URWDIN-Regular", color="black"),
        legend.text = element_text(family = "URWDIN-Regular", color="black"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0, 25, 0, 0)) +
      scale_x_discrete(expand = c(0, 0)) +
      labs(fill=NULL) +
      geom_text(aes(label = "Brak danych dla wybranych parametrów filtrowania."),
                vjust = 0.5, hjust = 0.5, color = "black", family = "URWDIN-Regular", size = 6)
    
  } else { 
    df_main$DATA_ANOMALII <- factor(df_main$DATA_ANOMALII, levels = unique(df_main$DATA_ANOMALII), ordered = TRUE)
    ggplot(data = df_main, mapping = aes(x = DATA_ANOMALII)) +
      geom_line(data = df_main, mapping = aes(y = ZAWMRP, group = 1, color = "ZAWMRP"), lwd = 0.9) +
      geom_line(data = df_main, mapping = aes(y = ZAUMDE, group = 1, color="ZAUMDE"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = ZMUMDE, group = 1, color="ZMUMDE"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = WPLMRP, group = 1, color="WPLMRP"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = DANEAN, group = 1, color="DANEAN"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = INSKLA, group = 1, color="INSKLA"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = WYTMRP, group = 1, color="WYTMRP"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = WSTRZD, group = 1, color="WSTRZD"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = PRZEKS, group = 1, color="PRZEKS"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = ZAMMRP, group = 1, color="ZAMMRP"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = ROUMDE, group = 1, color="ROUMDE"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = UPAPOS, group = 1, color="UPAPOS"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = WYPMRP, group = 1, color="WYPMRP"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = ZRWNAB, group = 1, color="ZRWNAB"), lwd=0.9) +
      geom_line(data = df_main, mapping = aes(y = UPADEW, group = 1, color="UPADEW"), lwd=0.9) +
      
      # ggtitle("LICZBA ANOMALII W PODZIALE NA USŁUGI") +
      xlab("") + 
      ylab("") +
      ylim(0, max(df_main$ZAWMRP,
                    df_main$ZAUMDE,
                    df_main$ZMUMDE,
                    df_main$WPLMRP,
                    df_main$DANEAN,
                    df_main$INSKLA,
                    df_main$WYTMRP,
                    df_main$WSTRZD,
                    df_main$PRZEKS,
                    df_main$ZAMMRP,
                    df_main$ROUMDE,
                    df_main$UPAPOS,
                    df_main$WYPMRP,
                    df_main$ZRWNAB,
                    df_main$UPADEW,
                    3)) +
      theme_bw() +
      scale_x_discrete(expand = c(0, 0)) +
      scale_color_manual(values = c("ZAWMRP" = "#09599B", "ZAUMDE" = "#199CB5", "ZMUMDE" = "#B43C23", "WPLMRP" = "#EFD7D2", "DANEAN" = "#D9553B", "INSKLA" = "#000000",
                                    "WYTMRP" = "#57CC00", "WSTRZD" = "#B40D93", "PRZEKS" = "#93EADE", "ZAMMRP" = "#A7A8A9", "ROUMDE" = "#921B02", "UPAPOS" = "#D29082",
                                    "WYPMRP" = "#757575", "ZRWNAB" = "#EC3038", "UPADEW" = "#F09BFC")) +
      theme(
        legend.position="bottom",
        legend.title = element_blank(),
        axis.text.y=element_text(family = "URWDIN-Regular", color="black"),
        axis.text.x=element_text(family = "URWDIN-Regular", color="black"),
        legend.text = element_text(family = "URWDIN-Regular", color="black"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0, 25, 0, 0))
  }
  
  
  
  return(plot)
}