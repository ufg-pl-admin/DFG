#' Tworzy obiekt data.frame z liczbą założonych MRP w podziale na grupy
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami: "MRP" i LICZBA_DEWELOPEROW",
#' zawierającymi liczbą założonych MRP w podziale na grupy.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_004_prepared_data i getCorrectedWords.
#' @return Obiekt data.frame z liczbą założonych MRP w podziale na grupy.
#'
#' @examples
#' DATA_STAT_004_3(NULL)
#'
#' @importFrom utils assign
#' @importFrom getCorrectedWords
#' @importFrom getStat_004_prepared_data
#' @export

DATA_STAT_004_3 <- function(json_content) {
  source(paste(getwd(), '/utils/rangeUtil.R', sep=""))

  createDataFrame <- function(json_content) {
    if (length(json_content) == 0) {
      df_main <- data.frame(matrix(ncol = 2, nrow = 2, dimnames = list(NULL, c("MRP", "LICZBA_DEWELOPEROW"))))
      df_main$MRP <- 0
      df_main$LICZBA_DEWELOPEROW <- 0
      
      # for (i in 1:2) {
      #   df_main <- rbind(df_main, c(0, 0))
      # }
      
      df_main$LICZBA_DEWELOPEROW <- as.numeric(df_main$LICZBA_DEWELOPEROW)
      df_main$MRP <- as.numeric(df_main$MRP)
      
      df_main$LICZBA_DEWELOPEROW <- ifelse(is.na(df_main$LICZBA_DEWELOPEROW), 0, df_main$LICZBA_DEWELOPEROW)
      
      N <- nrow(df_main)  
      k <- min(sqrt(N), 1 + 3.222 * log10(N), 5 * log10(N))  # liczba przedziałów
      k <- as.integer(k)  
      
      x_min <- min(df_main$MRP)  
      x_max <- max(df_main$MRP)  
      
      h <- (x_max - x_min) / k  # rozpiętość przedziału
      
      breaks <- seq(x_min, x_max + h, length.out = k + 1) # przedziały
      breaks <- round(breaks)
      
      breaks[1] <- breaks[1] - 1
      breaks <- unique(breaks)
      
      labels <- paste0(breaks[-length(breaks)] + 1, "-", breaks[-1])
      
      df <- df_main %>%
        mutate(MRP = cut(MRP, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)) %>% 
        group_by(MRP) %>% summarise(LICZBA_DEWELOPEROW = sum(LICZBA_DEWELOPEROW)) 
      
      df <- as.data.frame(df)
      
      df_labels <- data.frame(MRP = labels)
      
      df <- left_join(df_labels, df, by=c("MRP" = "MRP"))
      
      # df$LICZBA_DEWELOPEROW[is.na(df$LICZBA_DEWELOPEROW)] <- 0
      
      df <- df %>%
        mutate(KAT = row_number())
      
    } else {
      colnames(json_content) <- c("MRP","LICZBA_DEWELOPEROW")
      df_main <- as.data.frame(json_content)
      df_main$LICZBA_DEWELOPEROW <- as.numeric(df_main$LICZBA_DEWELOPEROW)
      df_main$MRP <- as.numeric(df_main$MRP)
      
      
      N <- nrow(df_main)  
      k <- min(sqrt(N), 1 + 3.222 * log10(N), 5 * log10(N))  # liczba przedziałów
      k <- as.integer(k)  
      
      x_min <- min(df_main$MRP)  
      x_max <- max(df_main$MRP)  
    
      
      
      
      h <- (x_max - x_min) / k  # rozpiętość przedziału
      
      if (length(unique(df_main$MRP)) > 1) {
        breaks <- get_braeks(df_main, x_min, x_max)
        
        labels <- paste0(breaks[-length(breaks)] +1, "-", breaks[-1])
        labels[1] <- paste0("do ", breaks[2])
        
      } else {
        breaks <- seq(x_min, x_max + h, length.out = k + 1) # przedziały
        breaks <- round(breaks)
        
        breaks[1] <- breaks[1] - 1
        breaks <- unique(breaks)
        
        labels <- paste0(breaks[-length(breaks)] +1, "-", breaks[-1])
        labels[1] <- paste0("do ", breaks[2])
      }
      
      df <- df_main %>%
        mutate(MRP = cut(MRP, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)) %>% 
        group_by(MRP) %>% summarise(LICZBA_DEWELOPEROW = sum(LICZBA_DEWELOPEROW)) 
      
      df <- as.data.frame(df)
      
      df_labels <- data.frame(MRP = labels)
      
      df <- left_join(df_labels, df, by=c("MRP"="MRP"))
      
      df$LICZBA_DEWELOPEROW[is.na(df$LICZBA_DEWELOPEROW)] <- 0
      
      df <- df %>%
        mutate(KAT = row_number())
      
      
    }
    
    return(df)
  }
  
  df <- createDataFrame(json_content)
  
  
  return(df)
}

INFOGRAPHIC_STAT_004_3 <- function(json_content) {
  df <- DATA_STAT_004_3(json_content)
  
  
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
  
  
  df$MRP <- fct_rev(df$MRP) 
  
  max_chars <- max(str_length(levels(df$MRP)))
  
  plot <- ggplot(df, aes(x=LICZBA_DEWELOPEROW, y=MRP)) +
    geom_bar(aes(y =  reorder(MRP, KAT),  x = LICZBA_DEWELOPEROW),stat="identity", fill="#D29082", width=.8) +
    xlab("Liczba deweloperów") +
    ylab("Liczba MRP           ") +
    theme_bw() +
    theme(axis.text.y=element_text(size=10, family = "URWDIN-Regular", color="black"),
          axis.text.x=element_text(size=10, family = "URWDIN-Regular", color="black"),
          axis.title.y=element_text(angle = 0, face="bold", size=9, margin = margin(r=-20)),
          axis.title.x=element_text(hjust=1, face="bold", size=9),
          plot.title = element_text(hjust = 0.5, size = 16,margin=margin(0,0,18,0), family ="URWDIN-Demi"),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = int_breaks, expand = c(0,0), labels = number_format(), limits=c(0,max(df$LICZBA_DEWELOPEROW)+0.1*max(df$LICZBA_DEWELOPEROW))) +
    geom_text(aes(label = sapply(LICZBA_DEWELOPEROW, LABEL)),  color = "black",  family ="URWDIN-Demi", hjust =-0.5, size=4.5)
  

  return(plot)
}
