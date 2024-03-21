source(paste(getwd(),'/visualization/Visualization.R', sep=""))
source(paste(getwd(),'/visualization/Data.R', sep=""))
source(paste(getwd(),'/visualization/FlatData.R', sep=""))

#' Funkcja do generowania infografiki.
#'
#' Funkcja generuje infografikę na podstawie przekazanych danych.
#' @param req Obiekt żądania HTTP.
#' @param name Nazwa infografiki.
#'
#' @return Infografika w formacie PNG.
#'
#' @export
returnPlot <- function(req, name) {
  json_content <- jsonlite::fromJSON(req$postBody)
  print(redirectedPlot(name, json_content))
}

#* Plot a histogram
#* @serializer png list(width = 960, height = 540, res = 96)
#* @post /statisticsapp/plot/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 1200, height = 550, res = 96)
#* @post /statisticsapp/plot/1200/550/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 1200, height = 440, res = 96)
#* @post /statisticsapp/plot/1200/440/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 1200, height = 350, res = 96)
#* @post /statisticsapp/plot/1200/350/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 960, height = 320, res = 96)
#* @post /statisticsapp/plot/960/320/<name>
#* @param input:object
returnPlot


#* Plot a histogram
#* @serializer png list(width = 960, height = 80, res = 96)
#* @post /statisticsapp/plot/960/80/<name>
#* @param input:object
returnPlot


#* Plot a histogram
#* @serializer png list(width = 1100, height = 150, res = 96)
#* @post /statisticsapp/plot/1100/150/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 1100, height = 280, res = 96)
#* @post /statisticsapp/plot/1100/280/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2000, height = 100, res = 240)
#* @post /statisticsapp/plot/2000/100/<name>
#* @param input:object
returnPlot


#* Plot a histogram
#* @serializer png list(width = 3000, height = 300, res = 240)
#* @post /statisticsapp/plot/3000/300/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 3200, height = 400, res = 240)
#* @post /statisticsapp/plot/3200/400/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 3600, height = 400, res = 240)
#* @post /statisticsapp/plot/3600/400/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 3900, height = 700, res = 240)
#* @post /statisticsapp/plot/3900/700/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2100, height = 1500, res = 240)
#* @post /statisticsapp/plot/2100/1500/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2200, height = 1500, res = 240)
#* @post /statisticsapp/plot/2200/1500/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2800, height = 1500, res = 240)
#* @post /statisticsapp/plot/2800/1500/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2200, height = 760, res = 240)
#* @post /statisticsapp/plot/2200/760/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2600, height = 760, res = 240)
#* @post /statisticsapp/plot/2600/760/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2800, height = 760, res = 240)
#* @post /statisticsapp/plot/2800/760/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 3072, height = 1728, res = 240)
#* @post /statisticsapp/plot/3072/1728/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 4096, height = 2304, res = 240)
#* @post /statisticsapp/plot/4096/2304/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 3000, height = 1080, res = 240)
#* @post /statisticsapp/plot/2000/1080/<name>
#* @param input:object
returnPlot


#* Plot a histogram
#* @serializer png list(width = 3000, height = 400, res = 240)
#* @post /statisticsapp/plot/3000/400/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2400, height = 600, res = 240)
#* @post /statisticsapp/plot/2400/600/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2800, height = 800, res = 240)
#* @post /statisticsapp/plot/2800/800/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2400, height = 300, res = 240)
#* @post /statisticsapp/plot/2400/300/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2400, height = 400, res = 240)
#* @post /statisticsapp/plot/2400/400/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2600, height = 600, res = 240)
#* @post /statisticsapp/plot/2600/600/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2600, height = 400, res = 240)
#* @post /statisticsapp/plot/2600/400/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2800, height = 1400, res = 240)
#* @post /statisticsapp/plot/2800/1400/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 3000, height = 2000, res = 240)
#* @post /statisticsapp/plot/3000/2000/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2000, height = 540, res = 240)
#* @post /statisticsapp/plot/2000/540/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 2750, height = 700, res = 240)
#* @post /statisticsapp/plot/2750/700/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 3000, height = 800, res = 240)
#* @post /statisticsapp/plot/3000/800/<name>
#* @param input:object
returnPlot

#* Plot a histogram
#* @serializer png list(width = 3000, height = 720, res = 240)
#* @post /statisticsapp/plot/3000/720/<name>
#* @param input:object
returnPlot


#* Plot a histogram
#* @serializer png list(width = 3000, height = 1200, res = 240)
#* @post /statisticsapp/plot/3000/1200/<name>
#* @param input:object
returnPlot


#* Plot a histogram
#* @serializer png list(width = 3000, height = 1600, res = 240)
#* @post /statisticsapp/plot/3000/1600/<name>
#* @param input:object
returnPlot


#' Funkcja do generowania tabeli danych.
#'
#' Funkcja generuje tabelę danych na podstawie przekazanych danych.
#' @param req Obiekt żądania HTTP.
#' @param name Nazwa tabeli danych.
#'
#' @return Tabela przeanalizowanych danych.
#'
#' @export
#* Analize data
#* @post /statisticsapp/data/<name>
#* @param input:object
function(req, name) {
    json_content <- jsonlite::fromJSON(req$postBody)
    print(analyzedData(name, json_content))
}

#' Funkcja do generowania danych bez analizy.
#'
#' Funkcja generuje płaskie dane na podstawie przekazanych danych.
#' @param req Obiekt żądania HTTP.
#' @param name Nazwa płaskich danych.
#'
#' @return Płaskie dane.
#'
#' @export
#* Return flat data
#* @post /statisticsapp/flatdata/<name>
#* @param input:object
function(req, name) {
    json_content <- jsonlite::fromJSON(req$postBody)
    print(flatData(name, json_content))
}

#' Żądanie GET do weryfikacji czy aplikacja odpowiada
#'
#' @param req Obiekt body HTTP.
#'
#' @return Odpowiedź z potwierdzeniem funkcjonującej aplikacji.
#'
#' @export
#* Żądanie GET do weryfikacji czy aplikacja odpowiada
#* @get /statisticsapp/healthcheck
#* @serializer text
function(msg="", res){
  res$body <- ""
}