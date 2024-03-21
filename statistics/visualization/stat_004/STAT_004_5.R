#' Tworzy obiekt data.frame z liczbą założonych MRP w podziale na województwa
#'
#' Funkcja tworzy i zwraca obiekt data.frame z czterema kolumnami: "LP","FORMA_PRAWNA","LICZBA_DEWELOPEROW" i"PERCENT",
#' zawierającymi liczbę deweloperów w podziale na status prawny prowadzonej działalności.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_004_prepared_data i getCorrectedWords.
#' @return Obiekt data.frame z liczbą deweloperów w podziale na status prawny prowadzonej działalności.
#'
#' @examples
#' DATA_STAT_004_5(NULL)
#'
#' @importFrom utils assign
#' @importFrom getCorrectedWords
#' @importFrom getStat_004_prepared_data
#' @export
DATA_STAT_004_5 <- function(json_content) {
  colnames(json_content) <- c("LP","FORMA_PRAWNA","LICZBA_DEWELOPEROW","PERCENT")
  df_main <- as.data.frame(json_content)
  nrows <- nrow(df_main)
  df_main$PERCENT <- as.numeric(df_main$PERCENT)


  # adding rows here:
  if(nrows < 5) {
    df_main <- bind_rows(df_main,
                         data.frame(FORMA_PRAWNA = rep("-", 5 - nrows),
                                    LICZBA_DEWELOPEROW = rep("0", 5 - nrows),
                                    PERCENT = rep(0, 5 - nrows)))
  }

  TOP_5  <- df_main[1:5,]
  TOP_5 <- as.data.frame(TOP_5)

  lines <- data.frame(
    LP = c(1, 2, 3, 4, 5),
    FORMA_PRAWNA = c(TOP_5$FORMA_PRAWNA),
    LICZBA_DEWELOPEROW = c(TOP_5$LICZBA_DEWELOPEROW),
    PERCENT = round(TOP_5$PERCENT / sum(df_main$PERCENT) * 100, 1)
  )

  other <- data.frame(LP = '',
                      FORMA_PRAWNA ='pozostałe',
                      LICZBA_DEWELOPEROW = 0,
                      PERCENT = 0.0)

  if (any(df_main$FORMA_PRAWNA == "pozostałe")) {
    other <- data.frame(LP = "",
                      FORMA_PRAWNA ='pozostałe',
                      LICZBA_DEWELOPEROW = df_main$LICZBA_DEWELOPEROW[df_main$FORMA_PRAWNA == "pozostałe"],
                      PERCENT = df_main$PERCENT[df_main$FORMA_PRAWNA == "pozostałe"])

    if (as.numeric(df_main$LP[df_main$FORMA_PRAWNA == "pozostałe"]) < 6) {
      lines[lines$FORMA_PRAWNA == "pozostałe", ] <- data.frame(LP = lines$LP[lines$FORMA_PRAWNA == "pozostałe"],
                        FORMA_PRAWNA ='-',
                        LICZBA_DEWELOPEROW = 0,
                        PERCENT = 0.0)
    }
  }

  lines <- rbind(lines, other)
  lines$PERCENT[length(lines$PERCENT)] <- round(df_main$PERCENT[df_main$FORMA_PRAWNA == "pozostałe"], 1)
  lines$LICZBA_DEWELOPEROW[is.na(lines$LICZBA_DEWELOPEROW)] <- 0
  lines$PERCENT[is.na(lines$PERCENT)] <- 0
  lines$FORMA_PRAWNA[is.na(lines$FORMA_PRAWNA)] <- "-"
  
  lines$PERCENT <- as.numeric(gsub("%", "", lines$PERCENT))
  
  return(lines)
}


getStat_001_prepared_data_5 <- function(json_content) {
  colnames(json_content) <- c("LP","FORMA_PRAWNA","LICZBA_DEWELOPEROW","PERCENT")
  df_main <- as.data.frame(json_content)
  
  df_main$PERCENT <- as.numeric(df_main$PERCENT)
  
  # Round the values proportionally to ensure the sum is 100
  df_main$PERCENT <- round(df_main$PERCENT / sum(df_main$PERCENT) * 100, 1)
  
  # Adjust the last rounded value to ensure the sum is 100
  df_main$PERCENT[length(df_main$PERCENT)] <- round(100 - sum(df_main$PERCENT[-length(df_main$PERCENT)]), 1)
  
  df_main$PERCENT <- as.numeric(gsub("%", "", df_main$PERCENT))
  return(df_main)
}



INFOGRAPHIC_STAT_004_5 <- function(json_content) {
  TOP_FIVE <- DATA_STAT_004_5(json_content)

  TOP_FIVE$PERCENT <- paste0(format(TOP_FIVE$PERCENT, nsmall = 1, decimal.mark = ','), '%')

  TOP_FIVE$LICZBA_DEWELOPEROW <- as.numeric(TOP_FIVE$LICZBA_DEWELOPEROW)
  TOP_FIVE$LP <- as.numeric(TOP_FIVE$LP)
  TOP_FIVE$LP[TOP_FIVE$FORMA_PRAWNA == "pozostałe"] = ""
  
  TOP_FIVE <- rbind(TOP_FIVE, data.frame(LP = "",
                                         FORMA_PRAWNA='Łącznie', 
                                         LICZBA_DEWELOPEROW = sum(TOP_FIVE$LICZBA_DEWELOPEROW),
                                         PERCENT="100%"))
  
  TOP_FIVE$LP[TOP_FIVE$LP == 6] <- ' '
  
  TOP_FIVE$LICZBA_DEWELOPEROW <- format(as.numeric(TOP_FIVE$LICZBA_DEWELOPEROW), big.mark=" ")
  
  colnames(TOP_FIVE) <- c("Lp.", "Forma prawna","Liczba deweloperów","Udział")
  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "plain", fontsize = 12, fontfamily="URWDIN-Regular", hjust = 0, 
                       x = 0.1),
      padding=unit.c(unit(45, "mm"), unit(6, "mm"))
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", fontsize = 14,fontfamily="URWDIN-Demi", col = "#B43C23",  hjust = 0, 
                       x = 0.1)
    ),
    rowhead = list(
      fg_params = list(fontface = "plain", fontsize = 14)
    ),
    rowsep = list(col = "red", lty = 2, lwd = 2), 
    colsep = list(col = NA)
  )
  
  table_grob <- tableGrob(TOP_FIVE, theme = custom_theme, rows = NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  
  nrow <- nrow(TOP_FIVE)

  ind <- find_cell(table_grob, nrow+1, 3, "core-fg")
  table_grob$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 14, fontface="bold", col = "#B43C23", fontfamily="URWDIN-Demi")
  ind_2 <- find_cell(table_grob, nrow+1, 4, "core-fg")
  table_grob$grobs[ind_2][[1]][["gp"]] <- gpar(fontsize = 14, fontface="bold", col = "#B43C23", fontfamily="URWDIN-Demi")
  
  ind_6 <- find_cell(table_grob, nrow+1, 2, "core-fg")
  table_grob$grobs[ind_6][[1]][["gp"]] <- gpar(fontsize = 14, fontface="bold", col = "#B43C23", fontfamily="URWDIN-Demi")
  
  

  for (j in 3:4)
  {
    
    # identify the grobs to change 
    id <- which(grepl("core-fg", table_grob$layout$name ) & table_grob$layout$l == j )
    
    
    # loop through grobs and change relevant parts
    for (i in id) {
      table_grob$grobs[[i]]$x <- unit(1, "npc")
      table_grob$grobs[[i]]$hjust <- 1
      
      
      #change colheads
      id2 <- which(grepl("colhead-fg",table_grob$layout$name) & table_grob$layout$r==j)
      table_grob$grobs[[id2]] <- editGrob(table_grob$grobs[[id2]],hjust=1)
      table_grob$grobs[[id2]] <- editGrob(table_grob$grobs[[id2]],x=unit(1, "npc"))
      
    }
  }
  
  
  
  
  
  # title <- textGrob("TOP 5 FORM PRAWNYCH WG LICZBY DEWELOPERÓW",gp=gpar(fontsize=18, fontfamily="URWDIN-Demi"))
  # 
  # padding <- unit(3.5,"line")
  # table_grob <- gtable_add_rows(table_grob,
  #                                 heights = grobHeight(title) + padding,
  #                                 pos = 0)
  # 
  # 
  # table_grob <- gtable_add_grob(table_grob, list(title),
  #                                 t=1, l=1, 
  #                                 r=ncol(table_grob))
  # 
  
  
  # Create horizontal dashed lines for other rows
  separators <- replicate(nrow(table_grob) - 2,
                          segmentsGrob(y1 = unit(0, "npc"), gp = gpar(lty = 2)),
                          simplify = FALSE)
  
  # Add horizontal lines after the 2nd row
  table_grob <- gtable_add_grob(table_grob, grobs = separators,
                                t = seq_len(nrow(table_grob) - 2) , b = seq_len(nrow(table_grob) - 2), l = 1)
  table_grob <- gtable_add_grob(table_grob, grobs = separators,
                                t = seq_len(nrow(table_grob) - 2) , b = seq_len(nrow(table_grob) - 2) , l = 2)
  table_grob <- gtable_add_grob(table_grob, grobs = separators,
                                t = seq_len(nrow(table_grob) - 2), b = seq_len(nrow(table_grob) - 2) , l = 3)
  table_grob <- gtable_add_grob(table_grob, grobs = separators,
                                t = seq_len(nrow(table_grob) - 2) , b = seq_len(nrow(table_grob) - 2) , l = 4)
  
  table_grob <- gtable_add_grob(table_grob,
                                grobs = segmentsGrob(
                                  x0 = unit(0,"npc"),
                                  y0 = unit(0,"npc"),
                                  x1 = unit(1,"npc"),
                                  y1 = unit(0,"npc"),
                                  gp = gpar(lty = 2)),
                                t = 1, b = 1, l = 1, r = 4)
  
  
  
  plot <- plot_grid(table_grob)
  
  return(plot)
  
}