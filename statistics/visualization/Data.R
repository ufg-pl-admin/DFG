#' Funkcja do generowania danych bez analizy.
#'
#' Funkcja generuje płaskie dane na podstawie przekazanych danych.
#' @param functionName Nazwa funkcji generującej płaskie dane.
#' @param data dane przekazane w żądaniu w formacie JSON.
#'
#' @return Płaskie dane.
#'
#' @export

analyzedData <- function(functionName, data) {
    analyzedData <- eval(parse(text=paste("DATA_", functionName, "(data)", sep="")))
    return(analyzedData)
}