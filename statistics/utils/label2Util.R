#' Zdefiniowanie formatu wyrażenia liczbowego.
#'
#' Ten skrypt R definiuje funkcje do nadania formatu liczbowego dodając spację jako separator tysięcy
#' zastosowanie funkcji zależy od wielkości wartości liczbowej.
#'
#' @return Zwraca zdefiniowaną sformatowaną kwotę.
#'
#'
#' @export
LABEL2 <- function(value) {
  ifelse(value >= 1000, 
         format(value, format = "f", big.mark = " "),
         paste0(as.character(value), " "))
}