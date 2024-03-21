#' Zdefiniowanie formatu wyrażenia liczbowego.
#'
#' Ten skrypt R definiuje funkcje do nadania formatu liczbowego dodając spację jako separator tysięcy
#' zastosowanie funkcji zależy od wielkości wartości liczbowej.
#'
#' @return Zwraca zdefiniowaną sformatowaną kwotę.
#'
#'
#' @export
LABEL <- function(value) {
  if (value >= 1000) {
    amount <- format(value, format = "f", big.mark=" ")
  } else {
    amount <- format(value, format = "f", big.mark="")
  }
  return(amount)
}