DATA_STAT_001_5 <- function(json_content) {
    df_main <- getStat_001_prepared_data_1(json_content)
    source(paste(getwd(), '/utils/rangeUtil.R', sep=""))
    
    x_min <- min(df_main$LICZBA_UMOW_DEW)  
    x_max <- max(df_main$LICZBA_UMOW_DEW)  
    breaks <- get_braeks(df_main, x_min, x_max)
    
    
    labels <- paste0(breaks[-length(breaks)] +1, "-", breaks[-1])
    labels[1] <- paste0("do ", breaks[2])
    
    
    df <- df_main %>%
      mutate(UMOWY = cut(LICZBA_UMOW_DEW, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)) %>% 
      group_by(UMOWY) %>% summarise(BANKI = sum(LICZBA_BANKOW)) 
    
    df <- as.data.frame(df)
    
    df_labels <- data.frame(UMOWY = labels)
    
    df <- left_join(df_labels, df, by=c("UMOWY"="UMOWY"))
    
    df$BANKI[is.na(df$BANKI)] <- 0
    
    df <- df %>%
      mutate(KAT = row_number())

    # df <- df_main %>%
    # mutate(UMOWY = case_when(
    #         LICZBA_UMOW_DEW >= 1 & LICZBA_UMOW_DEW <= 500 ~ '1-500',
    #         LICZBA_UMOW_DEW >= 501 & LICZBA_UMOW_DEW <= 1000 ~ '501-1000',
    #         LICZBA_UMOW_DEW >= 1001 & LICZBA_UMOW_DEW <= 1500 ~ '1001-1500',
    #         LICZBA_UMOW_DEW >= 1501 & LICZBA_UMOW_DEW <= 2000 ~ '1501-2000',
    #         LICZBA_UMOW_DEW >= 2001 & LICZBA_UMOW_DEW <= 2500 ~ '2001-2500',
    #         LICZBA_UMOW_DEW >= 2501 & LICZBA_UMOW_DEW <= 3000 ~ '2501-3000',
    #         LICZBA_UMOW_DEW >= 3001 & LICZBA_UMOW_DEW <= 3500 ~ '3001-3500',
    #         LICZBA_UMOW_DEW >= 3501 & LICZBA_UMOW_DEW <= 4000 ~ '3501-4000',
    #         LICZBA_UMOW_DEW >= 4001 & LICZBA_UMOW_DEW <= 4500 ~ '4001-4500',
    #         LICZBA_UMOW_DEW >= 4501 & LICZBA_UMOW_DEW <= 5000 ~ '4501-5000',
    #         LICZBA_UMOW_DEW > 5001 ~ '5001 i więcej'
    #         #LICZBA_UMOW_DEW == 0 ~ 'brak'
    #     )
    # ) %>% group_by(UMOWY) %>% summarise(BANKI = sum(LICZBA_BANKOW)) 
    # 
    # 
    # df_umowy <-  data.frame(
    #   UMOWY = c('1-500',
    #           '501-1000',
    #           '1001-1500',
    #           '1501-2000',
    #           '2001-2500',
    #           '2501-3000',
    #           '3001-3500',
    #           '3501-4000',
    #           '4001-4500',
    #           '4501-5000',
    #           '5001 i więcej')) 
    # 
    # 
    # df <- left_join(df_umowy, df, by=c("UMOWY"="UMOWY"))
    # 
    # df$BANKI[is.na(df$BANKI)] <- 0
    # 
    # df <- df %>%
    #   mutate(KAT = case_when(
    #     UMOWY == '1-500' ~ 11,
    #     UMOWY == '501-1000' ~ 10,
    #     UMOWY == '1001-1500' ~ 9,
    #     UMOWY == '1501-2000' ~ 8,
    #     UMOWY == '2001-2500' ~ 7,
    #     UMOWY == '2501-3000' ~ 6,
    #     UMOWY == '3001-3500' ~ 5,
    #     UMOWY == '3501-4000' ~ 4,
    #     UMOWY == '4001-4500' ~ 3,
    #     UMOWY == '4501-5000' ~ 2,
    #     UMOWY == '5001 i więcej' ~ 1
    #   )
    #   )
    # 
    # # <- filter(df, UMOWY !="brak")
    # df <- df[order(df$KAT,decreasing=TRUE),]
    # df$UMOWY <- factor(df$UMOWY, levels = df$UMOWY[order(df$KAT)])
    return(df)
}

INFOGRAPHIC_STAT_001_5 <- function(json_content) {
    df <- DATA_STAT_001_5(json_content)
    
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
    
    
    df$UMOWY <- fct_rev(df$UMOWY) 
    
    max_chars <- max(str_length(levels(df$UMOWY)))
    
    # df$UMOWY <- factor(df$UMOWY, levels = rev(c('1-500',
    #                                         '501-1000',
    #                                         '1001-1500',
    #                                         '1501-2000',
    #                                         '2001-2500',
    #                                         '2501-3000',
    #                                         '3001-3500',
    #                                         '3501-4000',
    #                                         '4001-4500',
    #                                         '4501-5000',
    #                                         '5001 i więcej')))

    plot <- ggplot(df, aes(x=BANKI, y=UMOWY)) +
    #ggtitle("LICZBA BANKÓW WG LICZBY UMÓW DEWELOPERSKICH") +
    geom_bar(aes(y =  reorder(UMOWY, KAT),  x = BANKI),stat="identity", fill="#D29082", width=.8) +
    xlab("Liczba banków") +
    ylab("Liczba UD") +
    theme_bw() +
    theme(axis.text.y=element_text(size=10, family = "URWDIN-Regular", color="black"),
          axis.text.x=element_text(size=10, family = "URWDIN-Regular", color="black"),
          axis.title.y=element_text(angle = 0, face="bold", size=10, margin = margin(r=if ( max_chars>=9) {
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
          axis.title.x=element_text(hjust=1, face="bold", size=10),
          plot.title = element_text(hjust = 0.5, size = 18,margin=margin(0,0,18,0), family ="URWDIN-Demi"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
      scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(df$BANKI)+0.1*max(df$BANKI))) +
      #scale_y_discrete(limits = levels(df$UMOWY))  +
      geom_text(aes(label = sapply(BANKI, LABEL)),  color = "black",  family ="URWDIN-Demi", hjust =-0.5, size=4.5)
    return(plot)
}



