#' Tworzy obiekt data.frame z liczbą skorygowanych zasileń per Bank
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwiema kolumnami i wierszami,
#' zawierającymi liczbę skorygowanych zasileń zgrupowanych względem nazw banków.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP002_prepared_data_7.
#' @return Obiekt data.frame z liczbą skorygowanych zasileń.
#'
#' @examples
#' DATA_RAP002_7(NULL)
#'
#' @importFrom utils assign
#' @importFrom getRAP002_prepared_data_7
#' @export
DATA_RAP002_7 <- function(json_content) {
  df_main_2 <- getRAP002_prepared_data_7(json_content)
  colnames(df_main_2) <- c("BANK", "L_SKORYGOWANYCH")
  df_main <- as.data.frame(df_main_2)
  df_main$L_SKORYGOWANYCH <- as.numeric(df_main$L_SKORYGOWANYCH)
  TOP_5 <- df_main[with(df_main,order(L_SKORYGOWANYCH, decreasing = TRUE)),]
  TOP_5  <- TOP_5[1:5,]
  TOP_5 <- as.data.frame(TOP_5)

  lines <- data.frame(
    LP = c(1, 2, 3, 4, 5),
    BANK = c(TOP_5$BANK),
    L_SKORYGOWANYCH = c(TOP_5$L_SKORYGOWANYCH),
    PERCENT = round((TOP_5$L_SKORYGOWANYCH/sum(df_main$L_SKORYGOWANYCH) * 100), 1)
  )
  other <- data.frame(LP = '',
                      BANK ='pozostałe',
                      L_SKORYGOWANYCH = sum(df_main$L_SKORYGOWANYCH) - sum(TOP_5$L_SKORYGOWANYCH),
                      PERCENT = round(((sum(df_main$L_SKORYGOWANYCH) - sum(TOP_5$L_SKORYGOWANYCH))/sum(df_main$L_SKORYGOWANYCH)) * 100, 1))
  lines <- rbind(lines, other)
  lines$L_SKORYGOWANYCH[is.na(lines$L_SKORYGOWANYCH)] <- 0
  lines$PERCENT[is.na(lines$PERCENT)] <- 0
  lines$PERCENT[length(lines$PERCENT)] <- 100 - sum(lines$PERCENT[-length(lines$PERCENT)])
  lines$BANK[is.na(lines$BANK)] <- "-"
  lines$PERCENT<- formatC(lines$PERCENT, digits = 1, format = "f", decimal.mark = ".")
  return(lines)
}

#' Tworzy obiekt data.frame z liczbą skorygowanych zasileń per Bank
#'
#' Funkcja tworzy i zwraca obiekt data.frame z dwiema kolumnami i wierszami,
#' zawierającymi liczbę skorygowanych zasileń zgrupowanych względem nazw banków.
#'
#' @param json_content Argument wykorzystywany w funkcji getRAP002_prepared_data_7.
#' @return Obiekt ggplot z radarowym opisującym liczbę zasileń względem banków dokonujących korekt.
#'
#' @examples
#' INFOGRAPHIC_RAP002_7(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getRAP002_prepared_data_7
#' @export
INFOGRAPHIC_RAP002_7 <- function(json_content) {
  df_main <- getRAP002_prepared_data_7(json_content)
  df_main$L_SKORYGOWANYCH <- as.numeric(df_main$L_SKORYGOWANYCH)
  
  TOP_5 <- df_main[with(df_main,order(L_SKORYGOWANYCH, decreasing = TRUE)),]
  TOP_5  <- TOP_5[1:5,] 
  TOP_5 <- as.data.frame(TOP_5)

  
  #wyliczenie pod prezentacje tabelaryczną
  TOP_5 <- TOP_5 %>%
    select(BANK, L_SKORYGOWANYCH) %>%
    mutate(Percent= round(TOP_5$L_SKORYGOWANYCH/sum(df_main$L_SKORYGOWANYCH), 3)) %>%
    arrange(desc(L_SKORYGOWANYCH))
  
  TOP <- cbind(TOP_5, colors=c('A', 'B', 'C', 'D', 'E'))
  
  TOP_5 <- rbind(TOP, data.frame(BANK='pozostałe', 
                                 L_SKORYGOWANYCH = sum(df_main$L_SKORYGOWANYCH) - sum(TOP_5$L_SKORYGOWANYCH),
                                 Percent = round((sum(df_main$L_SKORYGOWANYCH) - sum(TOP_5$L_SKORYGOWANYCH))/sum(df_main$L_SKORYGOWANYCH), 3),
                                 colors = 'F'))
  TOP_5$L_SKORYGOWANYCH[is.na(TOP_5$L_SKORYGOWANYCH)] <- 0
  TOP_5$Percent[is.na(TOP_5$Percent)] <- 0
  
  TOP_FIVE <- TOP_5 %>%
    select(BANK, L_SKORYGOWANYCH) %>%
    mutate(L.p. = c(1, 2, 3, 4, 5, ""))
  
  TABELA_TOP <- rbind(TOP_5, data.frame(BANK='Łącznie:', 
                                        L_SKORYGOWANYCH = sum(TOP_5$L_SKORYGOWANYCH),
                                        Percent = sum(TOP_5$Percent),
                                        colors = ''))
  
  #Tabela do prezentacji wizualnej
  TOP_FIVE <- TOP_FIVE %>% select(L.p., BANK, L_SKORYGOWANYCH)
  TOP_FIVE <- rbind(TOP_FIVE, data.frame(L.p. = "",
                                         BANK='Łącznie:', 
                                         L_SKORYGOWANYCH = sum(TOP_5$L_SKORYGOWANYCH)))
  TOP_FIVE$L_SKORYGOWANYCH <- format(as.numeric(TOP_FIVE$L_SKORYGOWANYCH), big.mark=" ")
  
  TOP_FIVE$BANK[is.na(TOP_FIVE$BANK)] <- "-"
  TOP_FIVE$BANK <- str_wrap(TOP_FIVE$BANK, width = 25)
  colnames(TOP_FIVE) <- c("Lp.  ", "Bank","   Liczba skorygowanych zasileń")
  

  
  custom_theme <- ttheme_minimal(
    core = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = c(rep("plain", 6), "bold"), hjust = 0, x = 0.08, fontsize = 15, "URWDIN-Regular"),
      padding = unit(c(8, 6), "mm")
    ),
    colhead = list(
      bg_params = list(fill = "white", col = NA),
      fg_params = list(fontface = "bold", hjust = 0, x = 0.08, fontsize = 15, col = "#B43C23", "URWDIN-Demi")
    ),
    rowhead = list(
      fg_params = list(fontface = "plain", fontsize = 15)
    ),
    rowsep = list(col = "red", lty = 2, lwd = 2), 
    colsep = list(col = "red", lty = 2, lwd = 2)
  )
  
  table_grob <- tableGrob(TOP_FIVE, theme = custom_theme, rows = NULL)
  
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  ind <- find_cell(table_grob, 8, 3, "core-fg")
  ind2 <- find_cell(table_grob, 7, 3, "core-fg")
  ind3 <- find_cell(table_grob, 7, 2, "core-fg")
  
  table_grob$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 15, fontface="bold", col = "#B43C23")
  table_grob$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 15, fontface="bold", col = "#B43C23")
  table_grob$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 15, fontface="bold")
  
  # Utworzenie horyzontalnych linii dla pozostałych wierszy
  separators <- replicate(nrow(table_grob) - 2,
                          segmentsGrob(y1 = unit(0, "npc"), gp = gpar(lty = 2)),
                          simplify = FALSE)
  
  # Dodanie horyzontalnych lini po drugim wierszu
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

  
  TOP_5 <- na.omit(TOP_5)
  #donutchart
  colors = c('A'='#D9553B','B'='#E17A65','C'='#EBA799','D'='#F2C8C0', 'E'='#F9E3DF', 'F'='#A7A8A9')
  
  TOP_5 <- TOP_5 %>%
    arrange(desc(colors)) %>%
    mutate(
      Percent_cum = cumsum(Percent),
      y = .5 * (Percent_cum + lag(Percent_cum, default = 0))
    )
  
  TOP_5$BANK <- str_wrap(TOP_5$BANK, width = 15)
  
  max_value <- max(TOP_5$Percent, na.rm = TRUE)

  PIEDONUT_BANK <- if (nrow(df_main) == 0) {

    ggplot(TOP_5, fill = colors) +
      geom_hline(
        aes(yintercept = y),
        data = data.frame(y = c((100*1/4), (100*2/4), (100*3/4), 100)),
        color = "lightgrey"
      ) + 
      coord_polar(start = pi, clip="off") +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black", family = "URW DIN-REGULAR", face = "plain", size = 13),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_blank(),
        plot.margin = unit(c(1.2, 1.2, 1.2, 1.2), "in")
      ) +
      theme(legend.position = "none") +
      geom_text(x = 0, y = 32.5, label = "Brak danych dla wybranych\nparametrów filtrowania.", 
                vjust = 0, hjust = 0.5, color = "black", family = "URW DIN-REGULAR", size = 6)
    
    } else {
      
    ggplot(TOP_5, fill = colors) +
    geom_hline(
      aes(yintercept = y),
      data = data.frame(y = c((max_value*1/4), (max_value*2/4), (max_value*3/4), max_value)),
      color = "lightgrey"
    ) +
    geom_col(
      aes(
        x = reorder(BANK, -Percent),
        y = Percent,
        fill = colors
      ),
      position = "dodge2",
      show.legend = TRUE,
      alpha = .9
    ) +
    geom_point(
      aes(
        x = reorder(BANK, -Percent),
        y = 0
        # y = min(Percent)
      ),
      size = 3,
      color = "#B43C23"
    ) +
    geom_segment(
      aes(
        x = reorder(BANK, -Percent),
        y = min(Percent),
        xend = reorder(BANK, -Percent),
        yend = max(Percent),
        group = colors
      ),
      linetype = "dashed",
      color = "gray12"
    ) +
    geom_label(
      aes(
        x = reorder(BANK, -Percent),
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
    coord_polar(start = ifelse(length(json_content) == 8, pi,
                                   (ifelse(length(json_content) == 6, -pi, 
                                           (ifelse(length(json_content) == 4, pi, 
                                                   (ifelse(length(json_content) == 2, pi/2, 3.65))
                                           ))))), clip = "off") +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(color = "black", family = "URW DIN-REGULAR", face = "plain", size = 13),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank(),
      plot.margin = unit(c(1.2, 1.2, 1.2, 1.2), "in")
    ) +
    theme(legend.position = "none") +
    scale_fill_manual(values = colors, name = "BANK", labels = TOP_5$BANK) +
        scale_y_continuous(
          limits = c(-0.2, max_value + 0.09),
          expand = c(0, 0),
          breaks = c((max_value*1/4), (max_value*2/4), (max_value*3/4), max_value)
        )
    }

  
  plot <- plot_grid(table_grob, PIEDONUT_BANK, ncol = 2, nrow = 1, rel_widths = c(1, 1), rel_heights = c(1, 0.4))
  
  return(plot)
}