#' Tworzy obiekt data.frame z liczbą założonych MRP w podziale na województwa
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwoma kolumnami: "WOJEWODZTWO" i "L_MRP",
#' zawierającymi liczbą założonych MRP w podziale na województwa.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_002_prepared_data i getCorrectedWords.
#' @return Obiekt data.frame z liczbą założonych MRP w podziale na województwa.
#'
#' @examples
#' DATA_STAT_003_5(NULL)
#'
#' @importFrom utils assign
#' @importFrom getCorrectedWords
#' @importFrom getStat_003_prepared_data
#' @export
DATA_STAT_003_5 <- function(json_content) {
  df_main <- getStat_003_prepared_data_4(json_content)
  
  TOP_5 <- df_main[with(df_main,order(L_MRP, decreasing = TRUE)),]
  TOP_5  <- TOP_5[1:5,]
  TOP_5 <- as.data.frame(TOP_5)
  lines <- data.frame(
    LP = c(1, 2, 3, 4, 5),
    WOJEWODZTWO = c(TOP_5$WOJEWODZTWO),
    L_MRP = c(TOP_5$L_MRP),
    PERCENT = c(round(TOP_5$L_MRP/sum(df_main$L_MRP) * 100, 1))
  )
  other <- data.frame(LP='',
                      WOJEWODZTWO='pozostałe',
                      L_MRP = sum(df_main$L_MRP) - sum(TOP_5$L_MRP),
                      PERCENT = round(((sum(df_main$L_MRP) - sum(TOP_5$L_MRP))/sum(df_main$L_MRP)) * 100, 1))
  lines <- rbind(lines, other)
  
  lines <- lines %>% 
    group_by(WOJEWODZTWO)
  
  # lines <- lines %>% 
  #   arrange(desc(LP))
  
  lines$L_MRP[is.na(lines$L_MRP)] <- 0
  lines$PERCENT[is.na(lines$PERCENT)] <- 0
  lines$WOJEWODZTWO[is.na(lines$WOJEWODZTWO)] <- "-"
  lines$L_MRP <- format(as.numeric(lines$L_MRP), big.mark=" ")
  lines$PERCENT[length(lines$PERCENT)] <- lines$PERCENT[length(lines$PERCENT)]
  lines$PERCENT <- paste0(lines$PERCENT, "%")
  
  return(lines)
}

INFOGRAPHIC_STAT_003_5 <- function(json_content) {
  df_main <- getStat_003_prepared_data_4(json_content)
#  df_main$L_MRP <- as.numeric(df_main$L_MRP)
  
  ###TOP 5 Województw wg założonych MRP###
  TOP_5 <- df_main[with(df_main,order(L_MRP, decreasing = TRUE)),]
  TOP_5 <- TOP_5[TOP_5$WOJEWODZTWO != 'pozostałe', ][1:5, ]  #<- TOP_5[1:5,] 
  TOP_5 <- as.data.frame(TOP_5)
  
  #wyliczenie pod prezentacje tabelaryczną
  TOP_5 <- TOP_5 %>%
    select(WOJEWODZTWO, L_MRP) %>%
    mutate(Percent = round((TOP_5$L_MRP/sum(df_main$L_MRP)), 3)) %>%
    arrange(desc(L_MRP))
  
  TOP <- cbind(TOP_5, colors=c('A', 'B', 'C', 'D', 'E'))
  
  
  TOP_5 <- rbind(TOP, data.frame(WOJEWODZTWO='pozostałe', 
                                 L_MRP = sum(df_main$L_MRP) - sum(TOP_5$L_MRP),
                                 Percent = round(((sum(df_main$L_MRP) - sum(TOP_5$L_MRP))/sum(df_main$L_MRP)),3),
                                 colors = 'F'))
    
  TOP_5$L_MRP[is.na(TOP_5$L_MRP)] <- 0
  
  TOP_5$Percent[is.na(TOP_5$Percent)] <- 0
  
  # TOP_5 <- TOP_5 %>%
  #   group_by(WOJEWODZTWO) %>%
  #   arrange(desc(L_MRP))  # Arrange by L_MRP in descending order within each group
  # 
  # TOP_FIVE <- TOP_5 %>%
  #   select(WOJEWODZTWO, L_MRP)
  # 
  # TOP_FIVE <- TOP_FIVE %>% select(WOJEWODZTWO, L_MRP) %>% mutate(L.p. = c(1, 2, 3, 4, 5, ""))
  
  TOP_FIVE <- TOP_5 %>%
    select(WOJEWODZTWO, L_MRP) %>%
    mutate(L.p. = c(1, 2, 3, 4, 5, ""))
  
  TABELA_TOP <- rbind(TOP_5, data.frame(WOJEWODZTWO='Łącznie:', 
                                        L_MRP = sum(TOP_5$L_MRP),
                                        Percent = sum(TOP_5$Percent),
                                        colors = ''))
  
  #Tabela do prezentacji wizualnej
  TOP_FIVE <- TOP_FIVE %>% select(L.p., WOJEWODZTWO, L_MRP)
  #TOP_FIVE$WOJEWODZTWO[TOP_FIVE$WOJEWODZTWO == 'pozostałe'] <- 'Pozostałe'
  
  TOP_FIVE <- rbind(TOP_FIVE, data.frame(L.p. = "",
                                         WOJEWODZTWO='Łącznie:', 
                                         L_MRP = sum(TOP_5$L_MRP)))
  
  TOP_FIVE$L_MRP <- format(as.numeric(TOP_FIVE$L_MRP), big.mark=" ")
  
  TOP_FIVE$WOJEWODZTWO[is.na(TOP_FIVE$WOJEWODZTWO)] <- "-"
  
  colnames(TOP_FIVE) <- c("Lp.", "Województwo","Liczba MRP")
  
custom_theme <- ttheme_minimal(
  core = list(
    bg_params = list(fill = "white", col = NA),
    fg_params = list(fontface = c(rep("plain", 6), "bold"), hjust = 0, x = 0.08, fontsize = 15, fontfamily="URWDIN-Regular"),
    padding=unit.c(unit(40, "mm"), unit(3.5, "mm"))
  ),
  colhead = list(
    bg_params = list(fill = "white", col = NA),
    fg_params = list(fontface = "bold", hjust = 0, x = 0.08, fontsize = 15, col = "#B43C23", fontfamily="URWDIN-Demi")
  ),
  rowhead = list(
    fg_params = list(fontface = "plain", fontsize = 15, fontfamily="URWDIN-Demi")
  ),
  rowsep = list(col = "red", lty = 2, lwd = 2),
    colsep = list(col = NA)
)
  
  table_grob <- tableGrob(TOP_FIVE, theme = custom_theme, rows = NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob, 8, 3, "core-fg")
  ind2 <- find_cell(table_grob, 7, 3, "core-fg")
  ind3 <- find_cell(table_grob, 7, 2, "core-fg")
  ind4 <- find_cell(table_grob, 8, 2, "core-fg")

  table_grob$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 15, fontface="bold", col = "#B43C23", fontfamily="URWDIN-Demi")
  table_grob$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 15, fontface="plain", fontfamily="URWDIN-Regular")
  table_grob$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 15, fontface="plain", fontfamily="URWDIN-Regular")
  table_grob$grobs[ind4][[1]][["gp"]] <- gpar(fontsize = 15, fontface="bold", col = "#B43C23", fontfamily="URWDIN-Demi")
  
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
                                t = seq_len(nrow(table_grob) - 2) , b = seq_len(nrow(table_grob) - 2) , l = 3)
  
  table_grob <- gtable_add_grob(table_grob,
                                grobs = segmentsGrob(
                                  x0 = unit(0,"npc"),
                                  y0 = unit(0,"npc"),
                                  x1 = unit(1,"npc"),
                                  y1 = unit(0,"npc"),
                                  gp = gpar(lty = 2)),
                                t = 1, b = 1, l = 1, r = 3)
  
  id <- which(grepl("core-fg", table_grob$layout$name ) & table_grob$layout$l == 3 )
  id2 <- which(grepl("colhead-fg", table_grob$layout$name ) & table_grob$layout$l == 3 )
  
  for (i in id) {
    table_grob$grobs[[i]]$x <- unit(1, "npc")
    table_grob$grobs[[i]]$hjust <- 1
  }
  for (i in id2) {
    table_grob$grobs[[i]]$x <- unit(1, "npc")
    table_grob$grobs[[i]]$hjust <- 1
  }
  
  # title <- textGrob("TOP 5 WOJEWÓDZTW WG LICZBY PROWADZONYCH MRP",gp=gpar(fontsize=13, fontface="bold", "URW DIN-DEMI"))
  # 
  # padding <- unit(3,"line")
  # table_grob <- gtable_add_rows(table_grob,
  #                               heights = grobHeight(title) + padding,
  #                               pos = 0)
  # 
  # table_grob <- gtable_add_grob(table_grob, list(title),
  #                               t=1, l=1,
  #                               r=ncol(table_grob))
  TOP_5 <- na.omit(TOP_5)
  #donutchart
  colors = c('A'='#D9553B','B'='#E17A65','C'='#EBA799','D'='#F2C8C0', 'E'='#F9E3DF', 'F'='#A7A8A9')
  
  
  TOP_5 <- TOP_5 %>%
    arrange(desc(colors)) %>%
    mutate(
      Percent_cum = cumsum(Percent),
      y = .5 * (Percent_cum + lag(Percent_cum, default = 0))
    )
  # PIEDONUT_WOJ <- ggplot(data = TOP_5, aes(x = 2, y = Percent, fill = colors)) +
  #   geom_col(color = "white") +
  #   coord_polar("y", start = -pi*2, clip = "off") +
  #   geom_segment(aes(x = 2.2, xend = 2.8, y = y, yend = y, group = colors), linetype="dashed", lwd=0.2) +
  #   geom_label(aes(x = 3.2,
  #                  label = ifelse((Percent * 100) < 6,
  #                                 paste0(" ", WOJEWODZTWO, " ", (Percent * 100), "%"),
  #                                 paste0(" ", WOJEWODZTWO, "\n", (Percent * 100), "%"))),
  #              position = position_stack(vjust = 0.5),
  #              fill =  alpha(c("white"),0),
  #              label.size = 0,
  #              hjust = 0.5,
  #              angle = 0,
  #              color = "black"
  #   ) +
  #   geom_point(
  #     position = position_stack(vjust = 0.5),
  #     color = "#B43C23",
  #     size = 3
  #   ) +
  #   theme(
  #     panel.background = element_blank(),
  #     axis.line = element_blank(),
  #     axis.text = element_blank(),
  #     axis.ticks = element_blank(),
  #     axis.title = element_blank(),
  #     plot.title = element_text(hjust = 0.5, size = 18)
  #   ) +
  #   theme(legend.position = "none") +
  #   scale_fill_manual(values = colors, name = "Województwo", labels = TOP_5$WOJEWODZTWO) +
  #   xlim(0.5, 3.4) +
  #   theme(text = element_text(family = "URWDIN-Regular"))
  max_value <- max(TOP_5$Percent, na.rm = TRUE)
  plt1 <- if(nrow(df_main) == 0) {
    ggplot(TOP_5, fill = colors) +
      geom_hline(
        aes(yintercept = y),
        data = data.frame(y = c((max_value*1/4), (max_value*2/4), (max_value*3/4), max_value)),
        color = "lightgrey"
      ) +
      coord_polar(start = pi, clip="off") +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black", family = "URWDIN-Regular", face = "plain", size = 13),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0)
      ) +
      theme(legend.position = "none") +
      geom_text(x = 0, y = -0.17, label = "Brak danych dla wybranych\nparametrów filtrowania.",
                vjust = 0, hjust = 0.5, color = "black", family = "URWDIN-Regular", size = 6)
  } else {
    ggplot(TOP_5, fill = colors) +
    geom_hline(
      aes(yintercept = y),
      data = data.frame(y = c((max_value*1/4), (max_value*2/4), (max_value*3/4), max_value)),
      color = "lightgrey"
    ) +
    geom_col(
      aes(
        x = reorder(TOP_5$WOJEWODZTWO, -ifelse(TOP_5$WOJEWODZTWO == "pozostałe", max(TOP_5$Percent) + 1, TOP_5$Percent)),#WOJEWODZTWO,#reorder(WOJEWODZTWO, -Percent),
        y = Percent,
        fill = colors
      ),
      position = "dodge2",
      show.legend = TRUE,
      alpha = .9
    ) +
    geom_point(
      aes(
        x = reorder(TOP_5$WOJEWODZTWO, -ifelse(TOP_5$WOJEWODZTWO == "pozostałe", max(TOP_5$Percent) + 1, TOP_5$Percent)),#WOJEWODZTWO,#reorder(WOJEWODZTWO, -Percent),
        y = 0
      ),
      size = 3,
      color = "#B43C23"
    ) +
    geom_segment(
      aes(
        x = reorder(TOP_5$WOJEWODZTWO, -ifelse(TOP_5$WOJEWODZTWO == "pozostałe", max(TOP_5$Percent) + 1, TOP_5$Percent)),#WOJEWODZTWO,#reorder(WOJEWODZTWO, -Percent),
        y = 0,
        xend = reorder(TOP_5$WOJEWODZTWO, -ifelse(TOP_5$WOJEWODZTWO == "pozostałe", max(TOP_5$Percent) + 1, TOP_5$Percent)),#WOJEWODZTWO,#reorder(WOJEWODZTWO, -Percent),
        yend = max(Percent),
        group = colors
      ),
      linetype = "dashed",
      color = "gray12"
    ) +
    geom_label(
      aes(
        x = reorder(TOP_5$WOJEWODZTWO, -ifelse(TOP_5$WOJEWODZTWO == "pozostałe", max(TOP_5$Percent) + 1, TOP_5$Percent)),#WOJEWODZTWO,#reorder(WOJEWODZTWO, -Percent),
        y = max(Percent),
        label = paste0(round((Percent * 100),2), "%")
      ),
      position = position_stack(vjust = 0.5),
      fill = alpha("white", 0),
      label.size = 0,
      hjust = 0,
      vjust = 0.5,
      angle = 0,
      label.padding = unit(1.2, "lines"),
      color = "black",
      fontface = "bold"
    ) +
    #coord_polar(start = -pi/2, clip="off") +
    coord_polar(start = ifelse(length(json_content) == 8, pi,
                               (ifelse(length(json_content) == 6, -pi,
                                       (ifelse(length(json_content) == 4, pi,
                                               (ifelse(length(json_content) == 2, pi/2, 3.65))
                                       ))))), clip = "off") +
    theme(
      panel.background = element_blank(),
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(color = "black", family = "URWDIN-Regular", face = "plain", size = 13),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    ) +
    theme(legend.position = "none") +
    scale_fill_manual(values = colors, name = "Województwo", labels = TOP_5$WOJEWODZTWO)
  }
  
  PIEDONUT_WOJ <- plt1 +
    scale_y_continuous(
      limits = c(-0.2, ifelse(sapply(TOP_5$WOJEWODZTWO, function(x) nchar(x) > 16), max_value + 0.06, max_value + 0.12)),
      expand = c(0, 0),
      breaks = c((max_value*1/4), (max_value*2/4), (max_value*3/4), max_value)
    )
  
  
  plot <- plot_grid(table_grob, PIEDONUT_WOJ, ncol = 2, nrow = 1, rel_widths = c(1, 1), rel_heights = c(1, 0.4))
  
  return(plot)
}