#' getStat_002_prepared_data
#'
#' @description
#' Funkcja `getStat_002_prepared_data` przygotowuje dane o miesięcznych raportach z punktów kredytowych w Polsce
#' w postaci obiektu `data.frame`.
#'
#' @usage
#' getStat_002_prepared_data(json_content)
#'
#' @param json_content Obiekt JSON zawierający dane o miesięcznych raportach z punktów kredytowych w Polsce.
#'
#' @return
#' Funkcja zwraca obiekt `data.frame`, w którym nazwy kolumn odpowiadają nazwom kolumn w pliku źródłowym.
#' Wszystkie kolumny liczbowe zostają skonwertowane do formatu numerycznego.
#'
#' @examples
#' data <- '{"RODZAJ_MRP": "POS", "MIESIAC": "2021-01", "WOJEWODZTWO": "MAZOWIECKIE", "NM1": "2900",
#' '"NM2": "20700", "NM3": "72"}'
#' json_content <- jsonlite::fromJSON(data)
#' getStat_002_prepared_data(json_content)
#'
#' @details
#' Funkcja nadaje kolumnom w obiekcie JSON nowe nazwy, odpowiadające nazwom kolumn w obiekcie `data.frame`,
#' który zostanie utworzony. Konwertuje obiekt JSON na `data.frame` przy użyciu funkcji `as.data.frame`.
#' Konwertuje trzy kolumny `NM1`, `NM2` oraz `NM3` do formatu numerycznego przy użyciu
#' funkcji `as.numeric`.

getStat_002_prepared_data_2 <- function(json_content) {
  colnames(json_content) <- c("NM1", "NM2", "NM3")
  df_main <- as.data.frame(json_content)
  df_main$NM1 <- format(as.numeric(df_main$NM1), big.mark=" ")
  df_main$NM2 <- format(as.numeric(df_main$NM2), big.mark=" ")
  df_main$NM3 <- format(as.numeric(df_main$NM3), big.mark=" ")
  return(df_main)
}

#' Tworzy obiekt data.frame z liczbą MRP
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i trzema kolumnami,
#' zawierającymi liczbę MRP, podzielonych na różne kategorie.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_002_prepared_data_2.
#' @return Obiekt data.frame z liczbą MRP.
#'
#' @examples
#' DATA_STAT_002_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom getStat_002_prepared_data_2
#' @export

DATA_STAT_002_2 <- function(json_content) {
  df_main_2 <- getStat_002_prepared_data_2(json_content)
  df <- data.frame(
    C1 = c("Liczba aktywnych mieszkaniowych rachunków powierniczych w wybranym okresie", "Liczba uruchomionych nowych mieszkaniowych rachunków powierniczych",
           "Liczba dezaktywowanych mieszkaniowych rachunków powierniczych"),
    C2 = c(df_main_2$NM1, df_main_2$NM2, df_main_2$NM3)
  )
  
  return(df)
}

#' Tworzy obiekt ggplot z tabelami opisującymi liczbę prowadzonych MRP
#'
#' Funkcja tworzy i zwraca obiekt ggplot z ikoną oraz pięcioma tabelami
#' opisującymi liczbę prowadzonych MRP, umieszczonymi obok siebie.
#'
#' @param json_content Argument wykorzystywany w funkcji getStat_002_prepared_data_2.
#' @return Obiekt ggplot z ikoną oraz pięcioma tabelami opisującymi liczbę prowadzonych MRP.
#'
#' @examples
#' INFOGRAPHIC_STAT_002_2(NULL)
#'
#' @importFrom utils assign
#' @importFrom gridExtra tableGrob
#' @importFrom ggplot2 ggdraw draw_image theme_void annotation_custom plot_grid
#' @importFrom getStat_002_prepared_data_2
#' @export

INFOGRAPHIC_STAT_002_2 <- function(json_content) {
    df_main_2 <- getStat_002_prepared_data_2(json_content)
    
    df1 <- data.frame(var1=c(df_main_2$NM1,"liczba aktywnych MRP\nw wybranym okresie"))
    df2 <- data.frame(var1=c("         w tym:"))
    #df2 <- data.frame(var1=c("w tym:"))
    df3 <- data.frame(var1=c(df_main_2$NM2,"liczba uruchomionych\nnowych MRP"))
    df4 <- data.frame(var1=c(df_main_2$NM3,"liczba\ndezaktywowanych MRP"))
    
    
    df_list <- list(df1,df2,df3,df4)
    
custom_theme <- ttheme_minimal(
      core = list(
        bg_params = list(fill = "white", col = NA),
        fg_params = list(fontface = "plain", fontfamily="URWDIN-Demi", fontsize = 15, just="left"),
        padding = unit(c(0, 6), "mm")
      ),
      colhead = list(
        bg_params = list(fill = "dodgerblue3", col = NA),
        fg_params = list(fontsize = 15, col = "white", just="left")
      )
      
    )
custom_theme_2 <- ttheme_minimal(
  core = list(
    bg_params = list(fill = "white", col = NA),
    fg_params = list(fontface = "plain", fontfamily="URWDIN-Regular", fontsize = 15, just="left")
  ),
  colhead = list(
    bg_params = list(fill = "dodgerblue3", col = NA),
    fg_params = list(fontsize = 15, col = "white", just="left")
  )
  
  
)
    j <-0
    
    for (i in df_list){
      
      j<- j+1
      assign(paste0("table_grob_",j), tableGrob(i, theme = custom_theme, rows=NULL,cols=NULL))
      
      table_grob_2 <- tableGrob(df2, theme = custom_theme_2, rows=NULL,cols=NULL)
      
    }
    
    find_cell <- function(table, row, col, name="core-fg"){
      l <- table$layout
      which(l$t==row & l$l==col & l$name==name)
    }
    
    ind <- find_cell(table_grob_1, 1, 1, "core-fg")
    ind2 <- find_cell(table_grob_3, 1, 1, "core-fg")
    ind3 <- find_cell(table_grob_4, 1, 1, "core-fg")
    table_grob_1$grobs[ind][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
    table_grob_3$grobs[ind2][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
    table_grob_4$grobs[ind3][[1]][["gp"]] <- gpar(fontsize = 24, fontface="bold", fontfamily="URWDIN-Demi", col = "#B43C23")
    
    grob_list <- list(table_grob_1,table_grob_2,table_grob_3,table_grob_4)
    
    
    k <-0
    
    for (i in grob_list){
      
      k<- k+1
      assign(paste0("plot_",k), ggplot() + theme_void() + annotation_custom(i))
      
    }
    
    icon_path <- paste(getwd(), '/icons/skladki_krajowe.svg', sep="")#paste(getwd(), '/icons/zaloz_mrp.svg', sep="")
    icon <- ggdraw() + draw_image(icon_path, scale = 0.9)
    
    plot <- plot_grid(icon, plot_1, plot_2, plot_3, plot_4, ncol = 5, rel_widths = c(1.4,1.2,1,2.3,2.3))#scale=1)
    
    return(plot)
}