#' Funkcja do generowania tabeli danych.
#'
#' Funkcja generuje tabelę danych na podstawie przekazanych danych.
#' @param functionName Nazwa funkcji generującej tabelę danych.
#' @param data dane przekazane w żądaniu w formacie JSON.
#'
#' @return Tabela danych.
#'
#' @export

flatData <- function(functionName, data) {
    flatData <- eval(parse(text=paste("FLAT_DATA_", functionName, "(data)", sep="")))
    return(flatData)
}