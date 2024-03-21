#' Obliczenie wektora liczbowego x i wartość domyślnej 5, której celem jest wygenerowanie sekwencji zaokrąglonych punktów przerwania w celu podzielenia zakresu x na około n przedziałów.
#'
#' Ten skrypt R definiuje funkcje do zakorąglenia rozpiętości przedziałów do liczb całkowitych
#' korzysta z pretty(x, n) w celu przerwania zakresu dla pięciu przedziałów.
#'
#' @return Zdefiniowana rozpiętość przedziałów liczbowych.
#'
#'
#' @export
int_breaks <- function(x, n = 5) {
  l <- pretty(x, n)
  l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
}