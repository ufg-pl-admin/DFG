#' Obliczanie rozpiętości przedziału i zaokrąglanie wartości przedziału do liczb całkowitych
#'
#' Ten skrypt R definiuje funkcje do obliczania liczby przedziałów i rozpiętości przedziałów
#' korzysta z modulo w celu dokonania przybliżeń dla wartości granicznych przedziałów.
#'
#' @param word Ciąg znaków, dla którego chcesz poprawić pisownię.
#'
#' @return Zdefiniowana rozpiętość przedziałów liczbowych.
#'
#'
#' @export
get_braeks <- function(df_main, x_min, x_max) {
    N <- nrow(df_main)
    k <- min(sqrt(N), 1 + 3.222 * log10(N), 5 * log10(N))  # liczba przedziałów
    k <- as.integer(k)
    h <- round_any((x_max - x_min) / k, 1, f=ceiling)  # rozpiętość przedziału
  print(sprintf("%f %f %f %d", x_min, x_max, h, k))

  if (h > 100) {
    h <- round_any(h, 100, f=ceiling)
    x_max <- h * k
  } else if (h > 10) {
    h <- round_any(h, 10, f=ceiling)
    x_max <- h * k
  }


  breaks <- seq(from=0, to=x_max, by = h)  # przedziały
  breaks <- round(breaks)
  breaks[1] <- breaks[1] - 1

  if(breaks[length(breaks)] < x_max) {
    breaks[length(breaks)] = x_max
  }
  print(sprintf("%f %f %f %d", x_min, x_max, h, k))
  print(breaks)


  return(breaks)



    # if (h > 100) {
    #   print(breaks)
    #   h_breaks <- breaks + (100 - breaks%%100)
    #   h_breaks[1] <- 0
    #   h_breaks[length(h_breaks)] <- x_max + (100 - x_max%%100)
    #   print(h_breaks)
    #   return(h_breaks)
    # } else {
    #   return(breaks)
    # }
}

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}