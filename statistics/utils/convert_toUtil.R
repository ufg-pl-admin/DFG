#' Zdefiniowanie formatu wyrażenia numerycznego.
#'
#' Ten skrypt R definiuje funkcje do nadania formatu numerycznego dla kwot dodając separatory i sufix odpowiadający warości liczbowej.
#' Zastosowanie funkcji zależy od wielkości wartości liczbowej.
#'
#' @return Zwraca zdefiniowaną sformatowaną kwotę.
#'
#'
#' @export
convert_to_millions_or_billions_or_thousands <- function(value) {
  if (value >= 1000000000) {
    amount <- formatC(value / 1000000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
    paste0(amount, " mld")
  } else if (value >= 1000000) {
    amount <- formatC(value / 1000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
    paste0(amount, " mln")
  } else if (value >= 1000) {
    amount <- formatC(value / 1000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
    paste0(amount, " tys.")
  } else {
    amount <- formatC(value, digits = 0, format = "f", big.mark=" ")
    paste0(amount, "")
  }
} 

#' Zdefiniowanie formatu wyrażenia numerycznego.
#'
#' Ten skrypt R definiuje funkcje do nadania formatu numerycznego dla liczb dodając separatory i sufix odpowiadający warości liczbowej.
#' Zastosowanie funkcji zależy od wielkości wartości liczbowej.
#'
#' @return Zwraca zdefiniowaną sformatowaną liczb.
#'
#'
#' @export
convert <- function(value) {
  if (value >= 1000000000) {
    amount <- formatC(value / 1000000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
    paste0(amount, " mld")
  } else if (value >= 1000000) {
    amount <- formatC(value / 1000000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
    paste0(amount, " mln")
  } else if (value >= 1000) {
    amount <- formatC(value / 1000, digits = 1, format = "f", decimal.mark = ",", big.mark=" ")
    paste0(amount, " tys.")
  } else {
    amount <- formatC(value, digits = 0, format = "f", big.mark=" ")
    
  }
} 