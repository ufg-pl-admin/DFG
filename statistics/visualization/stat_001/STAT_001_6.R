DATA_STAT_001_6 <- function(json_content) {
    df_main <- getStat_001_prepared_data_3(json_content)

    
# df_miesiace <-  data.frame(
# MIESIAC_OTWARCIA = c('Styczeń',
# 'Luty',
# 'Marzec',
# 'Kwiecień',
# 'Maj',
# 'Czerwiec',
# 'Lipiec',
# 'Sierpień',
# 'Wrzesień',
# 'Październik',
# 'Listopad',
# 'Grudzień')) 
#     
#     
#     df <- left_join(df_miesiace, df_main, by=c("MIESIAC_OTWARCIA"="MIESIAC_OTWARCIA"))
#     
#     df$BANKI[is.na(df$BANKI)] <- 0
    
    

    df <- df_main %>% group_by(MIESIAC_OTWARCIA) %>%
      mutate(KAT = case_when(
        str_detect(MIESIAC_OTWARCIA,'Styczeń') ~ 1,
        str_detect(MIESIAC_OTWARCIA,'Luty') ~ 2,
        str_detect(MIESIAC_OTWARCIA,'Marzec') ~ 3,
        str_detect(MIESIAC_OTWARCIA,'Kwiecień') ~ 4,
        str_detect(MIESIAC_OTWARCIA,'Maj') ~ 5,
        str_detect(MIESIAC_OTWARCIA,'Czerwiec') ~ 6,
        str_detect(MIESIAC_OTWARCIA,'Lipiec') ~ 7,
        str_detect(MIESIAC_OTWARCIA,'Sierpień') ~ 8,
        str_detect(MIESIAC_OTWARCIA,'Wrzesień') ~ 9,
        str_detect(MIESIAC_OTWARCIA,'Październik') ~ 10,
        str_detect(MIESIAC_OTWARCIA,'Listopad') ~ 11,
        str_detect(MIESIAC_OTWARCIA,'Grudzień') ~ 12
      ))
    
    df$MIESIAC_OTWARCIA <- tolower(df$MIESIAC_OTWARCIA)
    df <- df[order(df$KAT),]#,decreasing=TRUE
    df$MIESIAC_OTWARCIA <- factor(df$MIESIAC_OTWARCIA,levels = df$MIESIAC_OTWARCIA[order(df$KAT)])
    return(df)
}

INFOGRAPHIC_STAT_001_6 <- function(json_content) {
    df <- DATA_STAT_001_6(json_content)
    
    df <- df[order(df$KAT,decreasing=TRUE),]
    
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
    
# 
# 
#     df$MIESIAC_OTWARCIA <- factor(df$MIESIAC_OTWARCIA, levels = rev(c(
#       "styczeń",
#       "luty",
#       "marzec",
#       "kwiecień",
#       "maj",
#       "czerwiec",
#       "lipiec",
#       "sierpień",
#       "wrzesień",
#       "październik",
#       "listopad",
#       "grudzień"
#     )))

   
    
    plot <- ggplot(df, aes(x=BANKI, y=MIESIAC_OTWARCIA)) +
    #ggtitle("LICZBA BANKÓW PROWADZĄCYCH MRP W PODZIALE NA MIESIĄCE") +
    geom_bar(stat="identity", fill="#D29082", width=.8) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(
          axis.text.y=element_text(size=10, family = "URWDIN-Regular", color="black"),
          axis.text.x=element_text(size=10, family = "URWDIN-Regular", color="black"),
          plot.title = element_text(hjust = 0.5, size = 18, margin=margin(0,0,18,0), family ="URWDIN-Demi"),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(limits = levels(df$MIESIAC_OTWARCIA)) +
    scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(df$BANKI)+0.1*max(df$BANKI))) +
    scale_y_discrete(limits = levels(df$MIESIAC_OTWARCIA))  +
    geom_text(aes(label = sapply(BANKI, LABEL)),  color = "black",  family ="URWDIN-Demi", hjust =-0.5, size=4.5)
    
    return(plot)
}