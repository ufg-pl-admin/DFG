#' Tworzy obiekt data.frame z liczbą banków dla każdej grupy MRP
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwiema kolumnami:
#' "MRP" zawierającą tekstowe opisy grup MRP oraz "BANKI" zawierającą liczbę banków
#' przypisanych do każdej grupy.
#'
#' @param json_content Obiekt JSON.
#' @return Obiekt data.frame z liczbą banków dla każdej grupy MRP.
#'
#' @examples
#' DATA_STAT_001_4(json_content)
#'
#' @export
DATA_STAT_001_4 <- function(json_content) {
    source(paste(getwd(), '/utils/rangeUtil.R', sep=""))
    df_main <- getStat_001_prepared_data_1(json_content)
    x_min <- min(df_main$LICZBA_MRP)
    x_max <- max(df_main$LICZBA_MRP)
    
    breaks <- get_braeks(df_main, x_min, x_max)

    
    labels <- paste0(breaks[-length(breaks)] + 1, "-", breaks[-1])
    labels[1] <- paste0("do ", breaks[2])
    df <- df_main %>%
      mutate(MRP = cut(LICZBA_MRP, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)) %>% 
      group_by(MRP) %>% summarise(BANKI = sum(LICZBA_BANKOW)) 
    
    df <- as.data.frame(df)
    
    
    df_labels <- data.frame(MRP = labels)
    
    df <- left_join(df_labels, df, by=c("MRP"="MRP"))
    
    df$BANKI[is.na(df$BANKI)] <- 0
    
    df <- df %>%
      mutate(KAT = row_number())
    
    # 
    # 
    # df <- df_main %>% mutate(MRP = case_when(
    #         LICZBA_MRP >= 1 & LICZBA_MRP <= 10 ~ "1-10",
    #         LICZBA_MRP >= 11 & LICZBA_MRP <= 20 ~ "11-20",
    #         LICZBA_MRP >= 21 & LICZBA_MRP <= 30 ~ "21-30",
    #         LICZBA_MRP >= 31 & LICZBA_MRP <= 40 ~ "31-40",
    #         LICZBA_MRP >= 41 & LICZBA_MRP <= 50 ~ "41-50",
    #         LICZBA_MRP >= 51 & LICZBA_MRP <= 60 ~ "51-60",
    #         LICZBA_MRP >= 61 & LICZBA_MRP <= 70 ~ "61-70",
    #         LICZBA_MRP >= 71 & LICZBA_MRP <= 80 ~ "71-80",
    #         LICZBA_MRP >= 81 & LICZBA_MRP <= 90 ~ "81-90",
    #         LICZBA_MRP >= 91 & LICZBA_MRP <= 100 ~ "91-100",
    #         LICZBA_MRP > 100 ~ "101 i więcej"
    #     )
    # ) %>% group_by(MRP) %>% summarize(BANKI = sum(LICZBA_BANKOW), .groups = 'drop')
    # 
    # df_mrp <-  data.frame(
    #   MRP = c("1-10",
    #           "11-20",
    #           "21-30",
    #           "31-40",
    #           "41-50",
    #           "51-60",
    #           "61-70",
    #           "71-80",
    #           "81-90",
    #           "91-100",
    #           "101 i więcej")) 
    # 
    # 
    # df <- left_join(df_mrp, df, by=c("MRP"="MRP"))
    # 
    # df$BANKI[is.na(df$BANKI)] <- 0
    # 
    # df <- df %>% mutate(KAT = case_when(
    #         MRP == "1-10" ~ 11,
    #         MRP == "11-20" ~ 10,
    #         MRP == "21-30" ~ 9,
    #         MRP == "31-40" ~ 8,
    #         MRP == "41-50" ~ 7,
    #         MRP == "51-60" ~ 6,
    #         MRP == "61-70" ~ 5,
    #         MRP == "71-80" ~ 4,
    #         MRP == "81-90" ~ 3,
    #         MRP == "91-100" ~ 2,
    #         MRP == "101 i więcej" ~ 1
    #     )
    # )
    # df <- df[order(df$KAT,decreasing=TRUE),]
    # df$MRP <- factor(df$MRP,levels = df$MRP[order(df$KAT)])
    return(df)
}
#' Tworzy wykres słupkowy liczby banków dla każdej grupy MRP
#'
#' Funkcja tworzy i zwraca wykres słupkowy z liczbą banków przypisanych do każdej grupy MRP. Wykres przedstawia
#' kategorie MRP na osi Y oraz liczbę banków na osi X.
#'
#' @param json_content Obiekt JSON.
#' @return Obiekt ggplot reprezentujący wykres słupkowy liczby banków dla każdej grupy MRP.
#'
#' @examples
#' INFOGRAPHIC_STAT_001_4(json_content)
#'
#' @importFrom ggplot2 ggplot aes xlab ylab geom_bar scale_x_continuous theme_bw
#' @importFrom ggplot2 theme element_text margin
#' @export
INFOGRAPHIC_STAT_001_4 <- function(json_content) {
    df <- DATA_STAT_001_4(json_content)
    
    
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
    
    
    df$MRP <- fct_rev(df$MRP ) 
    
    max_chars <- max(str_length(levels(df$MRP)))
    
    
    # 
    # x <- nrow(df)
    # print(x)
    # 
    # y <- 0.25*x
    
    # df$MRP <- factor(df$MRP, levels = rev(c("1-10",
    #                                     "11-20",
    #                                     "21-30",
    #                                     "31-40",
    #                                     "41-50",
    #                                     "51-60",
    #                                     "61-70",
    #                                     "71-80",
    #                                     "81-90",
    #                                     "91-100",
    #                                     "101 i więcej")))
    # 
    # lim <- c(0, max(df&BANKI) + 6)
    

    plot <- ggplot(df, aes(x=BANKI, y=MRP)) +
        #ggtitle("LICZBA BANKÓW WG LICZBY PROWADZONYCH MRP") +
        geom_bar(aes(y =  reorder(MRP, KAT),  x = BANKI),stat="identity", fill="#D29082", width=.8) +
        xlab("Liczba banków") +
        ylab("Liczba MRP") +
        theme_bw() +
        theme(axis.text.y=element_text(size=10, family = "URWDIN-Regular", color="black"),
              axis.text.x=element_text(size=10, family = "URWDIN-Regular", color="black"),
              axis.title.y=element_text(angle = 0, face="bold", size=9,margin = margin(r=if ( max_chars>=9) {
                -40
              } else if ( max_chars>=7) {
                -30
              } else if ( max_chars>=5) {
                -20
              } else if ( max_chars>=3) {
                -10
              } else {
                -50
              })),
              axis.line = element_line(colour = "black"),
              panel.border = element_blank(),
              axis.title.x=element_text(hjust=1, face="bold", size=9),
              plot.title = element_text(hjust = 0.5, size = 18,margin=margin(0,0,18,0), family ="URWDIN-Demi"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
       
      scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(df$BANKI)+0.1*max(df$BANKI))) +
      #scale_y_discrete(limits = levels(df$MRP))  +
        geom_text(aes(label = sapply(BANKI, LABEL)),  color = "black",  family ="URWDIN-Demi", hjust =-0.5, size=4.5)
    
    
    
    # scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format()) +
    #   scale_y_discrete(limits = levels(df$MRP)) +
    
    # hjust = ifelse(df$BANKI<=0,0,1),  nudge_x = ifelse((max(df$BANKI)-df$BANKI>100 & df$BANKI<=9)
    #                                            | (max(df$BANKI)-df$BANKI>1000 & df$BANKI<=99),
    #                                            0,-1*0.01*max(df$BANKI))
      return(plot)
}


