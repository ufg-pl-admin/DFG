#' Tworzy obiekt data.frame z liczbą założonych MRP w podziale na województwa
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami: "WOJEWODZTWO" i "L_MRP",
#' zawierającymi liczbą założonych MRP w podziale na województwa.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_001_prepared_data i getCorrectedWords.
#' @return Obiekt data.frame z liczbą założonych MRP w podziale na województwa.
#'
#' @examples
#' DATA_STAT_003_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom getCorrectedWords
#' @importFrom getStat_003_prepared_data
#' @export

DATA_STAT_003_3 <- function(json_content) {
  
  source(paste(getwd(), '/utils/provinceUtil.R', sep=""))
 # df_main <- getStat_003_prepared_data_2(json_content)
    json_content <- na.omit(json_content)

  createDataFrame <- function(json_content) {
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("L_MRP", "WOJEWODZTWO"))))
      df_main$WOJEWODZTWO <- as.character(df_main$WOJEWODZTWO)
      print(df_main)
    } else {
      colnames(json_content) <- c("L_MRP", "WOJEWODZTWO")
      df_main <- as.data.frame(json_content)
      df_main$L_MRP <- as.numeric(df_main$L_MRP)
      df_main <- df_main[complete.cases(df_main$WOJEWODZTWO), ]
      
      incorrectedWords <- df_main$WOJEWODZTWO
      correctedWords <- getCorrectedWords(incorrectedWords)
      correctedProvinces <- unlist(correctedWords)
      df_main$WOJEWODZTWO <-  correctedProvinces
      
      df_main <- setNames(aggregate(df_main$L_MRP, by = list(df_main$WOJEWODZTWO), FUN = sum), c("WOJEWODZTWO","L_MRP"))
      print(df_main)
    }
    
    return(df_main)
  }
  df_main <- createDataFrame(json_content)

  if (length(json_content) != 2) {
    woj <-  data.frame(
      WOJEWODZTWO = c("dolnośląskie", "kujawsko-pomorskie", "lubelskie", "lubuskie", "łódzkie", "małopolskie", "mazowieckie", "opolskie",
                      "podkarpackie", "podlaskie", "pomorskie", "śląskie", "świętokrzyskie", "warmińsko-mazurskie", "wielkopolskie", "zachodniopomorskie"))
    
    
    df_main <- left_join(woj, df_main, by=c("WOJEWODZTWO"="WOJEWODZTWO"))
  }
  df_main <- left_join(df_main, srodki, by=c("WOJEWODZTWO"="nazwa"))
   lines <- data.frame(
    WOJEWODZTWO = c("pomorskie","warmińsko-mazurskie","podlaskie","kujawsko-pomorskie","mazowieckie","lubelskie","świętokrzyskie","podkarpackie",
                    "zachodniopomorskie","wielkopolskie","lubuskie","łódzkie","dolnośląskie","opolskie","śląskie","małopolskie"),
    line_xend = c(29.5,29.5,29.5,29.5,29.5,29.5,29.5,29.5,10.6,8.7,7.5,7.2,8.6,7.4,7.2,8.3),
    line_yend = c(54.8,53.9,53,52.1,51.3,50.4,49.5,48.6,54.8,53.9,53,52.1,51.3,50.4,49.5,48.6),
    text1_x = c(30,30,30,30,30,30,30,30,4,4,4,4,4,4,4,4),
    text1_y = c(55,54.1,53.2,52.3,51.2,50.3,49.4,48.5,55,54.1,53.2,52.3,51.2,50.3,49.4,48.5),
    text2_x = c(33.2,36.4,33,36,33.8,32.8,34.3,33.9,10.1,8,6.7,6.3,7.7,6.6,6.2,7.5),
    text2_y = c(55,54.1,53.2,52.3,51.2,50.3,49.4,48.5,55,54.1,53.2,52.3,51.2,50.3,49.4,48.5)
  )
  df <- left_join(df_main, lines, by=c("WOJEWODZTWO"="WOJEWODZTWO"))
  df$L_MRP[is.na(df$L_MRP)] <- 0
  # df_main <- setNames(aggregate(df_main$L_MRP, by = list(df_main$WOJEWODZTWO), FUN = sum), c("WOJEWODZTWO","L_MRP"))
  # 
  # woj <-  data.frame(
  #   WOJEWODZTWO = c("dolnośląskie", "kujawsko-pomorskie", "lubelskie", "lubuskie", "łódzkie", "małopolskie", "mazowieckie", "opolskie",
  #                   "podkarpackie", "podlaskie", "pomorskie", "śląskie", "świętokrzyskie", "warmińsko-mazurskie", "wielkopolskie", "zachodniopomorskie"))
  # 
  # 
  # 
  # df <- left_join(woj, df_main, by = "WOJEWODZTWO")
  # 
  # df$L_MRP <- as.numeric(df$L_MRP)
  # df$L_MRP[is.na(df$L_MRP)] <- 0
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  return(df)
}

INFOGRAPHIC_STAT_003_3 <- function(json_content) {
  df <- DATA_STAT_003_3(json_content)
  #df$L_MRP <- format(as.numeric(df$L_MRP), big.mark=" ")
  LABEL <- function(value) {
    if (value >= 1000) {
      amount <- format(value, format = "f", big.mark=" ")
    } else {
      amount <- format(value, format = "f", big.mark="")
    }
  }
  #sapply(L_MRP, LABEL)
  plot <- ggplot(wojewodztwa_df) +
    #    ggtitle("LICZBA PROWADZONYCH MRP W PODZIALE NA WOJEWÓDZTWA") +
    geom_polygon(aes(long, lat, group=group), fill="white",color="black", show.legend = FALSE) +
    geom_text(data=df, aes(text1_x, text1_y, label=WOJEWODZTWO, color="#B43C23",
                           vjust =  "inward",
                           hjust =  ifelse(WOJEWODZTWO %in% c("pomorskie","warmińsko-mazurskie","podlaskie","kujawsko-pomorskie","mazowieckie","lubelskie","świętokrzyskie","podkarpackie")
                                           ,"outward","inward")), size=3.5, show.legend = FALSE, family="URWDIN-Demi") +
    scale_colour_manual(values="#B43C23") +
    geom_text(data=df, aes(text2_x, text2_y, label=sapply(L_MRP, LABEL),
                           vjust = "inward", 
                           hjust =  ifelse(WOJEWODZTWO %in% c("pomorskie","warmińsko-mazurskie","podlaskie","kujawsko-pomorskie","mazowieckie","lubelskie","świętokrzyskie","podkarpackie")
                                           ,"outward","inward")), size= 3.5, family="URWDIN-Demi", fontface="bold") +
    geom_point(data=df, aes(long,lat, color="#B43C23"), size=2.1, show.legend = FALSE) +
    geom_segment(aes(x = long, y = lat,
                     xend =ifelse(str_count(L_MRP)>1 & WOJEWODZTWO %in% c("dolnośląskie", "lubuskie","łódzkie", "małopolskie","opolskie","śląskie","wielkopolskie","zachodniopomorskie")
                                  ,line_xend+str_count(L_MRP)*0.3-0.3,line_xend), yend = line_yend ), data = df, linetype="dashed", lwd=0.3) +
    xlab(paste0("<b style='color:#B43C23'>Nazwa województwa </b>",
                "<b> Liczba prowadzących MRP</b>")) +
    coord_map() +
    theme_bw() +
    theme(plot.title = element_blank(),#element_text(hjust = 0.5, size = 18,margin=margin(0,0,18,0), family="URWDIN-Demi", face = "bold"),
          panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_blank(),
          axis.text=element_blank(),axis.ticks=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank())+ 
    scale_x_continuous(expand = expansion(mult = 0.1, add = .6))
  
  legend_path <- paste(getwd(), '/icons/map_legend3.png', sep="")
  map_legend <- ggdraw() + draw_image(legend_path)
  
  
  plot <- plot_grid(plot, map_legend, ncol=1,rel_heights=c(3, 0.25))
  
  return(plot)
}