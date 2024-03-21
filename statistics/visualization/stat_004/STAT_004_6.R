#' Tworzy obiekt data.frame z liczbą założonych MRP w podziale na województwa
#'
#' Funkcja tworzy i zwraca obiekt data.frame z czterema kolumnami: "LP","FORMA_PRAWNA","LICZBA_INWESTYCJI" i"PERCENT",
#' zawierającymi liczbę inwestycji w podziale na status prawny prowadzonej działalności.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_004_prepared_data i getCorrectedWords.
#' @return Obiekt data.frame z liczbę inwestycji w podziale na status prawny prowadzonej działalności.
#'
#' @examples
#' DATA_STAT_004_6(NULL)
#'
#' @importFrom utils assign
#' @importFrom getCorrectedWords
#' @importFrom getStat_004_prepared_data
#' @export
DATA_STAT_004_6 <- function(json_content) {
  
  colnames(json_content) <- c("LP","FORMA_PRAWNA","LICZBA_INWESTYCJI","PERCENT")
  df_main <- as.data.frame(json_content)
  nrows <- nrow(df_main)
  df_main$PERCENT <- as.numeric(df_main$PERCENT)


  # adding rows here:
  if(nrows < 5) {
    df_main <- bind_rows(df_main,
                         data.frame(FORMA_PRAWNA = rep("-", 5 - nrows),
                                    LICZBA_INWESTYCJI = rep("0", 5 - nrows),
                                    PERCENT = rep(0, 5 - nrows)))
  }

  TOP_5  <- df_main[1:5,]
  TOP_5 <- as.data.frame(TOP_5)

  lines <- data.frame(
    LP = c(1, 2, 3, 4, 5),
    FORMA_PRAWNA = c(TOP_5$FORMA_PRAWNA),
    LICZBA_INWESTYCJI = c(TOP_5$LICZBA_INWESTYCJI),
    PERCENT = round(TOP_5$PERCENT / sum(df_main$PERCENT) * 100, 1)
  )

  other <- data.frame(LP = '',
                      FORMA_PRAWNA ='pozostałe',
                      LICZBA_INWESTYCJI = 0,
                      PERCENT = 0.0)

  if (any(df_main$FORMA_PRAWNA == "pozostałe")) {
    other <- data.frame(LP = "",
                      FORMA_PRAWNA ='pozostałe',
                      LICZBA_INWESTYCJI = df_main$LICZBA_INWESTYCJI[df_main$FORMA_PRAWNA == "pozostałe"],
                      PERCENT = df_main$PERCENT[df_main$FORMA_PRAWNA == "pozostałe"])

    if (as.numeric(df_main$LP[df_main$FORMA_PRAWNA == "pozostałe"]) < 6) {
      lines[lines$FORMA_PRAWNA == "pozostałe", ] <- data.frame(LP = lines$LP[lines$FORMA_PRAWNA == "pozostałe"],
                        FORMA_PRAWNA ='-',
                        LICZBA_INWESTYCJI = 0,
                        PERCENT = 0.0)
    }
  }

  lines <- rbind(lines, other)
  lines$PERCENT[length(lines$PERCENT)] <- round(df_main$PERCENT[df_main$FORMA_PRAWNA == "pozostałe"], 1)
  lines$LICZBA_INWESTYCJI[is.na(lines$LICZBA_INWESTYCJI)] <- 0
  lines$PERCENT[is.na(lines$PERCENT)] <- 0
  lines$FORMA_PRAWNA[is.na(lines$FORMA_PRAWNA)] <- "-"
  
  lines$PERCENT <- as.numeric(gsub("%", "", lines$PERCENT))

  return(lines)
}



INFOGRAPHIC_STAT_004_6 <- function(json_content) {
  TOP_FIVE <- DATA_STAT_004_6(json_content)

  TOP_FIVE$PERCENT <- paste0(format(TOP_FIVE$PERCENT, nsmall = 1, decimal.mark = ','), '%')

  TOP_FIVE$LICZBA_INWESTYCJI <- as.numeric(TOP_FIVE$LICZBA_INWESTYCJI)
  TOP_FIVE$LP <- as.numeric(TOP_FIVE$LP)
  TOP_FIVE$LP[TOP_FIVE$FORMA_PRAWNA == "pozostałe"] = ""

  
  ###TOP 5 Województw prowadzących MRP###
  # TOP_5 <- df_main[with(df_main,order(NM4, decreasing = TRUE)),]
  # TOP_5  <- TOP_5[1:5,]
  # TOP_5 <- as.data.frame(TOP_5)
  # 
  
  #wyliczenie pod prezentacje tabelaryczną
  # TOP_5 <- TOP_5 %>%
  #   select(WOJEWODZTWO, NM4) %>%
  #   mutate(Percent= TOP_5$NM4/sum(df_main$NM4)) %>%
  #   arrange(desc(NM4))
  
  # TOP <- cbind(TOP_5, colors=c('A', 'B', 'C', 'D', 'E'))
  # 
  # TOP_5 <- rbind(TOP, data.frame(WOJEWODZTWO='pozostałe', 
  #                                NM4 = sum(df_main$NM4) - sum(TOP_5$NM4),
  #                                Percent = (sum(df_main$NM4) - sum(TOP_5$NM4))/sum(df_main$NM4),
  #                                colors = 'F'))
  # 
  # TOP_FIVE <- TOP_5 %>%
  #   select(WOJEWODZTWO, NM4) %>%
  #   mutate(L.p. = row_number())
  # 
  # TABELA_TOP <- rbind(TOP_5, data.frame(WOJEWODZTWO='Łącznie:', 
  #                                       NM4 = sum(TOP_5$NM4),
  #                                       Percent = sum(TOP_5$Percent),
  #                                       colors = ''))
  
  #Tabela do prezentacji wizualnej
  
  
  
  # TOP_FIVE <- TOP_FIVE %>% select(L.p., WOJEWODZTWO, NM4)
  
  
  
  TOP_FIVE <- rbind(TOP_FIVE, data.frame(LP = "",
                                         FORMA_PRAWNA='Łącznie', 
                                         LICZBA_INWESTYCJI = sum(TOP_FIVE$LICZBA_INWESTYCJI),
                                         PERCENT="100%"))
  
  
  #TOP_FIVE$FORMA_PRAWNA[TOP_FIVE$FORMA_PRAWNA == 'pozostałe'] <- 'Pozostałe:'
  TOP_FIVE$LP[TOP_FIVE$LP == 6] <- ' '
  
  TOP_FIVE$LICZBA_INWESTYCJI <- format(as.numeric(TOP_FIVE$LICZBA_INWESTYCJI), big.mark=" ")
#  TOP_FIVE$Percent <- formatC(TOP_FIVE$Percent, decimal.mark = ",")
  
  colnames(TOP_FIVE) <- c("Lp.", "Forma prawna","Liczba inwestycji","Udział")
  print(TOP_FIVE)
  
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
  
  # ind_3 <- find_cell(table_grob, nrow, 3, "core-fg")
  # table_grob$grobs[ind_3][[1]][["gp"]] <- gpar(fontsize = 16, fontface="bold", col = "#B43C23", fontfamily="URWDIN-Regular")
  # ind_4 <- find_cell(table_grob, nrow, 4, "core-fg")
  # table_grob$grobs[ind_4][[1]][["gp"]] <- gpar(fontsize = 16, fontface="bold", col = "#B43C23", fontfamily="URWDIN-Regular")
  # ind_5 <- find_cell(table_grob, nrow, 2, "core-fg")
  # table_grob$grobs[ind_5][[1]][["gp"]] <- gpar(fontsize = 16, fontface="bold", fontfamily="URWDIN-Regular")
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
  
  
  
  # title <- textGrob("TOP 5 FORM PRAWNYCH WG LICZBY INWESTYCJI",gp=gpar(fontsize=18, fontfamily="URWDIN-Demi"))
  # 
  # padding <- unit(3.5,"line")
  # table_grob <- gtable_add_rows(table_grob,
  #                               heights = grobHeight(title) + padding,
  #                               pos = 0)
  # 
  # 
  # table_grob <- gtable_add_grob(table_grob, list(title),
  #                               t=1, l=1, 
  #                               r=ncol(table_grob))
  
  
  # Create horizontal dashed lines for other rows
  separators <- replicate(nrow(table_grob) - 2,
                          segmentsGrob(y1 = unit(0, "npc"), gp = gpar(lty = 2)),
                          simplify = FALSE)
  
  # Add horizontal lines after the 2nd row
  table_grob <- gtable_add_grob(table_grob, grobs = separators,
                                t = seq_len(nrow(table_grob) - 2) , b = seq_len(nrow(table_grob) - 2) , l = 1)
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