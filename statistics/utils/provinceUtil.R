#' Obliczanie Odległości Levenshteina i Poprawianie Pisowni Województw
#'
#' Ten skrypt R definiuje funkcje do obliczania odległości Levenshteina między danym województwem
#' a listą słów w słowniku województw oraz do korekty pisowni poprzez znalezienie najbliższego dopasowania
#' w słowniku.
#'
#' @param word Ciąg znaków, dla którego chcesz poprawić pisownię.
#'
#' @return Poprawiona wersja podanego województwa, znaleziona poprzez zidentyfikowanie najbliższego
#'   dopasowania w predefiniowanym słowniku województw.
#'
#'
#' @export

dictionary = c("dolnośląskie", "kujawsko-pomorskie", "lubelskie",
               "lubuskie", "łódzkie", "małopolskie", "mazowieckie", 
               "opolskie", "podkarpackie", "podlaskie", "pomorskie", 
               "śląskie", "świętokrzyskie", "warmińsko-mazurskie",
               "wielkopolskie", "zachodniopomorskie")

levenshtein_distance <- function(a, b) {
  n <- nchar(a)
  m <- nchar(b)
  
  if (n == 0) {
    return(m)
  }
  
  if (m == 0) {
    return(n)
  }
  
  matrix <- matrix(0, nrow = n + 1, ncol = m + 1)
  rownames(matrix) <- c("", strsplit(a, "")[[1]])
  colnames(matrix) <- c("", strsplit(b, "")[[1]])
  
  for (i in 1:(n + 1)) {
    matrix[i, 1] <- i - 1
  }
  
  for (j in 1:(m + 1)) {
    matrix[1, j] <- j - 1
  }
  
  for (i in 2:(n + 1)) {
    for (j in 2:(m + 1)) {
      cost <- as.integer(rownames(matrix)[i] != colnames(matrix)[j])
      matrix[i, j] <- min(matrix[i - 1, j] + 1, matrix[i, j - 1] + 1, matrix[i - 1, j - 1] + cost)
    }
  }
  
  return(matrix[n + 1, m + 1])
}

correctSpelling <- function(word) {
  distances <- sapply(dictionary, function(d) levenshtein_distance(word, d))
  minIndex <- which.min(distances)
  return(dictionary[minIndex])
}

getCorrectedWords <- function(words) {
  return(lapply(words, correctSpelling))
}