#' Tworzy obiekt data.frame z dziesięcioma miejscowościmi pod względem największej liczby umów deweloperskich
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami: "MIEJSCOWOSC" i "L_UMOW",
#' zawierającymi dziesięcięć miejscowości pod względem największej liczby umów deweloperskich.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_010_prepared_data i getCorrectedWords.
#' @return Obiekt data.frame z dziesięcioma miejscowościmi pod względem największej liczby umów deweloperskich.
#'
#' @examples
#' DATA_STAT_010_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom getCorrectedWords
#' @importFrom getStat_010_prepared_data
#' @export
DATA_STAT_010_2 <- function(json_content) {
  df_main_2 <- getStat_010_prepared_data_2(json_content)
  colnames(df_main_2) <- c("MIEJSCOWOSC", "L_UMOW")
  df_main <- as.data.frame(df_main_2)
  df_main$L_UMOW <- as.numeric(df_main$L_UMOW)
  TOP_10 <- df_main[with(df_main,order(L_UMOW, decreasing = TRUE)),]
  TOP_10 <- TOP_10[TOP_10$MIEJSCOWOSC != 'pozostałe', ][1:10, ] #TOP_10  <- TOP_10[1:10,]
  TOP_10 <- as.data.frame(TOP_10)

  lines <- data.frame(
    LP = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    MIEJSCOWOSC = TOP_10$MIEJSCOWOSC,
    L_UMOW = TOP_10$L_UMOW,
    PERCENT = round(TOP_10$L_UMOW/sum(df_main$L_UMOW) * 100, 1)
  )
  other <- data.frame(LP = '',
                      MIEJSCOWOSC ='pozostałe',
                      L_UMOW = sum(df_main$L_UMOW) - sum(TOP_10$L_UMOW),
                      PERCENT = round(((sum(df_main$L_UMOW) - sum(TOP_10$L_UMOW))/sum(df_main$L_UMOW)) * 100, 1))
  lines <- rbind(lines, other)
  lines$L_UMOW[is.na(lines$L_UMOW)] <- 0
  lines$PERCENT[is.na(lines$PERCENT)] <- 0
  lines$MIEJSCOWOSC[is.na(lines$MIEJSCOWOSC)] <- "-"
  lines$PERCENT[length(lines$PERCENT)] <- lines$PERCENT[length(lines$PERCENT)]
  lines$PERCENT<- formatC(lines$PERCENT, digits = 1, format = "f", decimal.mark = ".")
  
  return(lines)
}

INFOGRAPHIC_STAT_010_2 <- function(json_content) {
  df_main <- getStat_010_prepared_data_2(json_content)
  df_main$L_UMOW <- as.numeric(df_main$L_UMOW)
  
  ###TOP 10 MIEJSCOWOSCI###
  TOP_10 <- df_main[with(df_main,order(L_UMOW, decreasing = TRUE)),]
  TOP_10 <- TOP_10[TOP_10$MIEJSCOWOSC != 'pozostałe', ][1:10, ] #TOP_10  <- TOP_10[1:10,]
  TOP_10 <- as.data.frame(TOP_10)
  
  TOP_10 <- TOP_10 %>%
    select(MIEJSCOWOSC, L_UMOW) %>%
    mutate(Percent= round(TOP_10$L_UMOW/sum(df_main$L_UMOW) * 100, 1)) %>%
    arrange(desc(L_UMOW))

  TOP_10 <- rbind(TOP_10, data.frame(MIEJSCOWOSC='pozostałe',
                                     L_UMOW = sum(df_main$L_UMOW) - sum(TOP_10$L_UMOW),
                                     Percent = round(((sum(df_main$L_UMOW) - sum(TOP_10$L_UMOW))/sum(df_main$L_UMOW)) * 100, 1)
  ))
  TOP_10$Percent[length(TOP_10$Percent)] <- TOP_10$Percent[length(TOP_10$Percent)]

  TOP_TEN <- TOP_10 %>%
    select(MIEJSCOWOSC, L_UMOW, Percent) %>%
    mutate(L.p. = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,""))#row_number())
  
  TOP_TEN <- TOP_TEN %>% select(L.p., MIEJSCOWOSC, L_UMOW, Percent)
  TOP_TEN$Percent <- sprintf("%.1f", TOP_TEN$Percent)#as.numeric(format(round((TOP_TEN$Percent * 100), 1), nsmall = 1))
  TOP_TEN <- rbind(TOP_TEN, data.frame(L.p. = "",
                                       MIEJSCOWOSC='Łącznie', 
                                       L_UMOW = sum(TOP_10$L_UMOW),
                                       Percent = "100.0"#sum(TOP_10$Percent)
  ))

  TOP_10$L_UMOW[is.na(TOP_10$L_UMOW)] <- 0
  TOP_10$Percent[is.na(TOP_10$Percent)] <- 0
  
  TOP_TEN$L_UMOW <- format(as.numeric(TOP_TEN$L_UMOW), big.mark=" ")
  TOP_TEN$Percent <- paste0(formatC(as.numeric(TOP_TEN$Percent), format = "f", digits = 1, decimal.mark = ","), '%')

  TOP_TEN$MIEJSCOWOSC[is.na(TOP_TEN$MIEJSCOWOSC)] <- "-"
  
  colnames(TOP_TEN) <- c("Lp.", "Miejscowość","Liczba inwestycji", "Udział")

  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = c(rep("plain", 11), "bold"), hjust = 0, x = 0.08, fontsize = 12, fontfamily="URWDIN-Regular"),
      padding=unit.c(unit(50, "mm"), unit(6, "mm"))
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", hjust = 0, x = 0.08, fontsize = 15, col = "#B43C23", fontfamily="URWDIN-Demi")
    ),
    rowhead = list(
      fg_params = list(fontface = "plain", fontsize = 15)
    ),
    rowsep = list(col = "red", lty = 2, lwd = 2), 
    colsep = list(col = "red", lty = 2, lwd = 2)
  )
  
  table_grob <- tableGrob(TOP_TEN, theme = custom_theme, rows = NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob, 13, 3, "core-fg")
  ind2 <- find_cell(table_grob, 13, 4, "core-fg")
  ind3 <- find_cell(table_grob, 12, 3, "core-fg")
  ind4 <- find_cell(table_grob, 12, 4, "core-fg")
  ind5 <- find_cell(table_grob, 13, 2, "core-fg")
  table_grob$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 15, fontface="bold", col = "#B43C23", fontfamily="URWDIN-Demi")
  table_grob$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 15, fontface="bold", col = "#B43C23", fontfamily="URWDIN-Demi")
  table_grob$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 12, fontface="plain", fontfamily="URWDIN-Regular")
  table_grob$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 12, fontface="plain", fontfamily="URWDIN-Regular")
  table_grob$grobs[ind5][[1]][["gp"]] <- gpar(fontsize = 15, fontface="bold", col = "#B43C23", fontfamily="URWDIN-Demi")
    
  # Create horizontal dashed lines for other rows
  separators <- replicate(nrow(table_grob) - 2,
                          segmentsGrob(y1 = unit(0, "npc"), gp = gpar(lty = 2)),
                          simplify = FALSE)
  
  # Add horizontal lines after the 2nd row
  table_grob <- gtable_add_grob(table_grob, grobs = separators,
                                t = seq_len(nrow(table_grob) - 2), b = seq_len(nrow(table_grob) - 2), l = 1)
  table_grob <- gtable_add_grob(table_grob, grobs = separators,
                                t = seq_len(nrow(table_grob) - 2), b = seq_len(nrow(table_grob) - 2), l = 2)
  table_grob <- gtable_add_grob(table_grob, grobs = separators,
                                t = seq_len(nrow(table_grob) - 2), b = seq_len(nrow(table_grob) - 2), l = 3)
  table_grob <- gtable_add_grob(table_grob, grobs = separators,
                                t = seq_len(nrow(table_grob) - 2), b = seq_len(nrow(table_grob) - 2), l = 4)
  table_grob <- gtable_add_grob(table_grob,
                                grobs = segmentsGrob(
                                  x0 = unit(0,"npc"),
                                  y0 = unit(0,"npc"),
                                  x1 = unit(1,"npc"),
                                  y1 = unit(0,"npc"),
                                  gp = gpar(lty = 2)),
                                t = 1, b = 1, l = 1, r = 4)
  
  id <- which(grepl("core-fg", table_grob$layout$name ) & table_grob$layout$l == 3 )
  id2 <- which(grepl("colhead-fg", table_grob$layout$name ) & table_grob$layout$l == 3 )
  id3 <- which(grepl("core-fg", table_grob$layout$name ) & table_grob$layout$l == 4 )
  id4 <- which(grepl("colhead-fg", table_grob$layout$name ) & table_grob$layout$l == 4 )
  
  for (i in id) {
    table_grob$grobs[[i]]$x <- unit(1, "npc")
    table_grob$grobs[[i]]$hjust <- 1
  }
  for (i in id2) {
    table_grob$grobs[[i]]$x <- unit(1, "npc")
    table_grob$grobs[[i]]$hjust <- 1
  }
  for (i in id3) {
    table_grob$grobs[[i]]$x <- unit(1, "npc")
    table_grob$grobs[[i]]$hjust <- 1
  }
  for (i in id4) {
    table_grob$grobs[[i]]$x <- unit(1, "npc")
    table_grob$grobs[[i]]$hjust <- 1
  }
  
  plot <- plot_grid(table_grob, ncol = 1, scale = 1) 
  
  return(plot)
}