# This file is a part of rOpenGov/sorvi (http://ropengov.github.com/sorvi/)

# Copyright (C) 2010-2013 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


#' Get Finnish postal codes vs. municipalities table from Wikipedia. 
#' @aliases get.postal.codes
#'
#' @param ... Arguments to be passed
#'
#' @return A data frame with following fields: postal.code: postal code; municipality: Name of the municipality (kunnan nimi); municipality.scandless: Municpality name without special chars 
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Juuso Parkkinen and Leo Lahti \email{louhos@@googlegroups.com}
#' @note Previous version of this function was named: get.postal.codes	      
#' @examples 
#' #postal.code.table <- GetPostalCodeInfo() 
#' @keywords utilities

GetPostalCodeInfo <- function (...) {

  url <- "http://fi.wikipedia.org/wiki/Luettelo_Suomen_postinumeroista_kunnittain"
  message(paste("Downloading data from", url))

  .InstallMarginal("plyr")

  # Read URL site
  txt <- readLines(url)

  # Pick list of postal codes
  txt <- txt[grep("^<li>", txt)]

  # Separate municipality names and postal codes
  cnt <- 0
  map <- list()
  for (i in 1:length(txt)) {
    li <- txt[[i]]
    if (substr(li, 1, 11) == "<li><a href") {            
      # Parsi kunnan nimi otsikkorivilta
      municipality <- unlist(strsplit(unlist(strsplit(li, ">"))[[3]], "<"))[[1]]
    } else {
      tmp1 <- unlist(strsplit(li, ">"))[[2]]      
      tmp0 <- unlist(strsplit(tmp1, "/"))
      postinro <- unlist(strsplit(tmp0[[1]], " "))[[1]] 
      cnt <- cnt + 1
      map[[cnt]] <- c(postinro, municipality)
    }
  }

  map <- plyr::ldply(map)
  colnames(map) <- c("postal.code", "municipality")
  map$municipality.ascii <- sorvi::korvaa.skandit(map$municipality)

  # Remove the last row
  map <- map[-nrow(map),]

  map
}






#' Get geographic coordinates for a given IP-address from 
#' http://www.datasciencetoolkit.org//ip2coordinates/
#'
#' @param ip IP address as character
#'
#' @return Latitude and longitude as a numeric vector
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @note Modified from original version by Kay Cichini
#' @examples # ip_location("137.224.252.10")
#' @keywords utilities

ip_location <- function(ip) {

   url <- "http://www.datasciencetoolkit.org//ip2coordinates/"
   URL_IP <- paste(url, ip, sep = "")

   message(paste("Retrieving the IP", ip, "from ", url))

   api_return <- readLines(URL_IP, warn = F)
   lon1 <- api_return[grep("longitude", api_return)]
   lon <- gsub("[^[:digit:].]", "", lon1)
   lat1 <- api_return[grep("latitude", api_return)]
   lat <- gsub("[^[:digit:].]", "", lat1)
   return(c(lat, lon))

}

