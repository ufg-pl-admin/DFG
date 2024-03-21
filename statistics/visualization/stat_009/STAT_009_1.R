#' Tworzy obiekt data.frame z liczbą inwestycji i umów deweloperskich w podziale na województwa
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami: "WOJEWODZTWO" i "L_INWESTYCJI",
#' zawierającymi liczbą inwestycji i umów deweloperskich w podziale na województwa.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_009_prepared_data i getCorrectedWords.
#' @return Obiekt data.frame z liczbą inwestycji i umów deweloperskich w podziale na województwa.
#'
#' @examples
#' DATA_STAT_009_1(NULL)
#'
#' @importFrom utils assign
#' @importFrom getCorrectedWords
#' @importFrom getStat_009_prepared_data
#' @export

DATA_STAT_009_1 <- function(json_content) {
  df_main <- getStat_009_prepared_data_1(json_content)

  return(df_main)
}

INFOGRAPHIC_STAT_009_1 <- function(json_content) {
  df_main <- getStat_009_prepared_data_1(json_content)
  

  woj <-  data.frame(
    WOJEWODZTWO = c("dolnośląskie", "kujawsko-pomorskie", "lubelskie", "lubuskie", "łódzkie", "małopolskie", "mazowieckie", "opolskie",
                    "podkarpackie", "podlaskie", "pomorskie", "śląskie", "świętokrzyskie", "warmińsko-mazurskie", "wielkopolskie", "zachodniopomorskie"))
  
  
  
  df <- left_join(woj, df_main, by = "WOJEWODZTWO")
  
  df$L_INWESTYCJI <- as.numeric(df$L_INWESTYCJI)
  df$L_UMOW <- as.numeric(df$L_UMOW)
  df$L_INWESTYCJI[is.na(df$L_INWESTYCJI)] <- 0
  df$L_UMOW[is.na(df$L_UMOW)] <- 0
  
  df <- left_join(df, srodki, by=c("WOJEWODZTWO"="nazwa"))
  lines <- data.frame(
    WOJEWODZTWO = c("pomorskie","warmińsko-mazurskie","podlaskie","kujawsko-pomorskie","mazowieckie","lubelskie","świętokrzyskie","podkarpackie",
                    "zachodniopomorskie","wielkopolskie","lubuskie","łódzkie","dolnośląskie","opolskie","śląskie","małopolskie"),
    line_xend = c(29.5,29.5,29.5,29.5,29.5,29.5,29.5,29.5,10.6,8.7,7.5,7.2,8.6,7.4,7.2,8.3),
    line_yend = c(54.8,53.9,53,52.1,51.3,50.4,49.5,48.6,54.8,53.9,53,52.1,51.3,50.4,49.5,48.6),
    text1_x = c(30,30,30,30,30,30,30,30,4,4,4,4,4,4,4,4),
    text1_y = c(55,54.1,53.2,52.3,51.2,50.3,49.4,48.5,55,54.1,53.2,52.3,51.2,50.3,49.4,48.5),
    text2_x = c(33.2,36.1,33,35.7,33.8,32.8,34.2,33.9,9.7,7.9,6.7,6.3,7.7,6.6,6.2,7.4),
    text2_y = c(55,54.1,53.2,52.3,51.2,50.3,49.4,48.5,55,54.1,53.2,52.3,51.2,50.3,49.4,48.5)
    # ,text3_x = c(34.9,37.6,34.7,37.2,35.5,34.5,35.8,35.6,11.3,9.5,8.4,8,9.4,8.3,8,9.1),
    # text3_y = c(55,54.1,53.2,52.3,51.4,50.5,49.6,48.7,55,54.1,53.2,52.3,51.4,50.5,49.6,48.7)
  )
  
  df <- left_join(df, lines, by=c("WOJEWODZTWO"="WOJEWODZTWO"))
  
  convert_to_millions_or_billions_or_thousands1 <- function(value) {
    if (value >= 1000000000) {
      amount <- formatC(value / 1000000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " mld")
    } else if (value >= 1000000) {
      amount <- formatC(value / 1000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " mln")
    } else if (value >= 1000) {
      amount <- formatC(value / 1000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0(amount, " tys.")
    } else {
      amount <- formatC(value, digits = 0, format = "f", big.mark=" ")
      paste0(amount, "")
    }
  }
  convert_to_millions_or_billions_or_thousands2 <- function(value) {
    if (value >= 1000000000) {
      amount <- formatC(value / 1000000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0("/ ", amount, " mld")
    } else if (value >= 1000000) {
      amount <- formatC(value / 1000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0("/ ", amount, " mln")
    } else if (value >= 1000) {
      amount <- formatC(value / 1000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
      paste0("/ ", amount, " tys.")
    } else {
      amount <- formatC(value, digits = 0, format = "f", big.mark=" ")
      paste0("/ ", amount, "")
    }
  }
  df$L_INWESTYCJI <- sapply(df$L_INWESTYCJI, convert_to_millions_or_billions_or_thousands1)
  df$L_UMOW <- sapply(df$L_UMOW, convert_to_millions_or_billions_or_thousands2)

  plot <- ggplot(wojewodztwa_df) +
    geom_polygon(aes(long, lat, group=group), fill="white", color="black", show.legend = FALSE) +
    geom_text(data=df, aes(text1_x, text1_y, label=WOJEWODZTWO, color="#B43C23",
                           vjust =  "inward",
                           hjust =  ifelse(WOJEWODZTWO %in% c("pomorskie","warmińsko-mazurskie","podlaskie","kujawsko-pomorskie","mazowieckie","lubelskie","świętokrzyskie","podkarpackie")
                                           ,"outward","inward")), size=3.5, show.legend = FALSE, family="URWDIN-Demi") +
    scale_colour_manual(values="#B43C23") +
    geom_text(data=df, aes(text2_x, text2_y, label=paste0(L_INWESTYCJI, " ", L_UMOW),
                           vjust =  "inward",
                           hjust =  ifelse(WOJEWODZTWO %in% c("pomorskie","warmińsko-mazurskie","podlaskie","kujawsko-pomorskie","mazowieckie","lubelskie","świętokrzyskie","podkarpackie")
                                           ,"outward","inward")), size=3.5, family="URWDIN-Demi") +
    geom_point(data=df, aes(long,lat, color="#B43C23"), size=2.1, show.legend = FALSE) +
    geom_segment(aes(x = long, y = lat,
                     xend =ifelse(str_count(paste0(L_INWESTYCJI, " ", L_UMOW))>1 & WOJEWODZTWO %in% c("dolnośląskie", "lubuskie","łódzkie", "małopolskie","opolskie","śląskie","wielkopolskie","zachodniopomorskie")
                                  ,line_xend+str_count(paste0(L_INWESTYCJI, " ", L_UMOW))*0.3-0.3,line_xend), yend = line_yend ), data = df, linetype="dashed", lwd=0.3) +
    xlab(paste0("<b style='color:#B43C23'>Nazwa województwa </b>",
                "<b>Liczba banków prowadzących MRP</b>")) +
    coord_map() +
    theme_bw()+
    theme( #plot.title = element_text(hjust = 0.5, size = 18,margin=margin(0,0,18,0), family ="URWDIN-Demi"),
      panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_blank(),
      axis.text=element_blank(),axis.ticks=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank())+ 
    scale_x_continuous(expand = expansion(mult = 0.1, add = 2.1))
  
  
  legend_path <- paste(getwd(), '/icons/map_legend4.png', sep="")
  map_legend <- ggdraw() + draw_image(legend_path)
  
  
  plot <- plot_grid(plot, map_legend, ncol=1,rel_heights=c(3, 0.25))
  
  return(plot)
}