#' Tworzy obiekt data.frame z opisem MRP
#'
#' Funkcja tworzy i zwraca obiekt data.frame z jednym wierszem i jedną kolumną,
#' zawierającym tekstowy opis MRP - Mieszkaniowego rachunku powierniczego, który
#' jest rodzajem rachunku bankowego identyfikującym pojedynczą inwestycję
#' chronioną przez Deweloperski Fundusz Gwarancyjny.
#'
#' @param json_content Argument nie jest wykorzystywany w funkcji.
#' @return Obiekt data.frame z opisem MRP.
#'
#' @examples
#' DATA_STAT_001_1(NULL)
#'
#' @export
DATA_STAT_001_1 <- function(json_content) {
  
  df <- data.frame(
    # C1 = '<p style="font-family: URWDIN-Regular, sans-serif; font-weight: 400; font-size: 16px; line-height: 19.2px"><span style="color: #B43C23"><strong>MRP - Mieszkaniowy Rachunek Powierniczy </strong></span> jest rodzajem rachunku bankowego identyfikującym pojedynczą inwestycję chronioną przez Deweloperski Fundusz Gwarancyjny.</p>')
    C1 = '<p style="font-family: URWDIN-Regular, sans-serif; font-weight: 600; font-size: 20px; line-height: 30px"><span style="color: #B43C23"><strong>MRP - Mieszkaniowy Rachunek Powierniczy </strong></span> jest rodzajem rachunku bankowego identyfikującym pojedynczą inwestycję chronioną przez Deweloperski Fundusz Gwarancyjny.</p>')
  
  return(df)
}

FLAT_DATA_STAT_001_1 <- function(json_content) {

  df <- data.frame(
    value = '<p style="font-family: URWDIN-Regular, sans-serif; font-weight: 600; font-size: 20px; line-height: 30px"><span style="color: #B43C23"><strong>MRP - Mieszkaniowy Rachunek Powierniczy </strong></span> jest rodzajem rachunku bankowego identyfikującym pojedynczą inwestycję chronioną przez Deweloperski Fundusz Gwarancyjny.</p>')

  return(df)
}

#' Tworzy obiekt ggplot z tabelą opisującą MRP
#'
#' Funkcja tworzy i zwraca obiekt ggplot z pustym tłem i niestandardową tabelą
#' opisującą MRP - Mieszkaniowy Rachunek Powierniczy, umieszczoną w lewym dolnym
#' rogu wykresu.
#'
#' @param json_content Argument nie jest wykorzystywany w funkcji.
#' @return Obiekt ggplot z tabelą opisującą MRP.
#'
#' @examples
#' INFOGRAPHIC_STAT_001_1(NULL)
#'
#' @import gridExtra
#' @import ggplot2
#' @export
INFOGRAPHIC_STAT_001_1 <- function(json_content) {


  tekst1 <- textGrob(label = "MRP - Mieszkaniowy Rachunek Powierniczy", gp = gpar(fontsize = 11, fontfamily = "URWDIN-Demi", col = "#B43C23"),hjust=0, x=0)
  tekst2 <- textGrob(label = "jest rodzajem rachunku bankowego identyfikującym pojedynczą inwestycję", gp = gpar(fontsize = 11, fontfamily = "URWDIN-Demi"), hjust = 0, x=-0.265)
  tekst3 <- textGrob(label = "chronioną przez Deweloperski Fundusz Gwarancyjny.", gp = gpar(fontsize = 11, fontfamily = "URWDIN-Demi"),hjust=0, x=0)
  
  par(mar = c(0, 0, 0, 0))

  # Ustawienie obszaru rysowania
  grid.newpage()
  vp <- viewport(width = 1, height = 1, just = "left", name = "obszar")
  pushViewport(vp)

  # Dodawanie tekstu na obszarze rysowania
   grid1 <- grid.arrange(tekst1, tekst2, ncol = 2)
   grid2 <- grid.arrange(tekst3, ncol = 1)
  
   
   grid.arrange(grid1, grid2, ncol = 1)

  plot <- recordPlot()

  return(plot)
}





