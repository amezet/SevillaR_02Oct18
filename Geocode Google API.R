library(RCurl)
library(RJSONIO)

API.key = "________" # Introducir la API de Google Cloud

# Funcion para construir la url para consultar la API Geocode de Google
# https://developers.google.com/maps/documentation/geocoding/intro
url <- function(country, city, return.call = "json", sensor = "false") {
  root <- "https://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?components=country:",country, "%7Clocality:", city, "&sensor=", sensor, sep = "", "&key=", API.key)
  return(URLencode(u))
}


geoCode <- function(country, city, verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(country, city)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA, NA))
  }
}

geoCode('España', 'Sevilla')
