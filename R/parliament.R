# Copyright (C) Juuso Haapanen 2012-2013, <juuso(at)haapanen.biz> All rights reserved
# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This software has been published as part of the Louhos project (louhos.github.com) 

# Versio 0.1

#' Hakee kaikki aanestykset eduskuntarajapinnasta
#' @param ... parameters to be passed
#' @return list
#' @author Juuso Haapanen, Leo Lahti \email{louhos@@googlegroups.com}
#' @examples
#' # AAnestysten tunnisteet ovat muotoa a{aanestyksen numero}_{istunnon numero}-{vuosi}
#' # aanestykset <- GetParliamentaryVotes(...)
#' @keywords eduskunta
#' @references citation("sorvi") 
#' @export

GetParliamentVotes <- function(...) {

  .InstallMarginal("XML")

  url <- "http://www.biomi.org/eduskunta/"
  kaikki.tree <- XML::xmlParse(url)
  tunnisteet <- XML::getNodeSet(kaikki.tree, path='//luettelo/aanestys/tunniste')
  out <- XML::xmlToDataFrame(tunnisteet)
  out <- as.character(out$text)
  return(out)
  
}

#' Hakee tietyn aanestyksen tulokset edustajaittain
#' 
#' @param aanestys aanestyksen ID (Aanestysten tunnisteet ovat muotoa 
#'        a{aanestyksen numero}_{istunnon numero}-{vuosi}), esim. "a3_80-2011"
#' @return data.frame jossa valinta, puolue ja nimi
#' @author Juuso Haapanen, Leo Lahti \email{louhos@@googlegroups.com}
#' @examples 
#' # edustajat <- GetEdustajaData('a3_80-2011')
#' @references See citation("sorvi") 
#' @keywords eduskunta
#' @export
GetEdustajaData <- function(aanestys)
{

  .InstallMarginal("XML")

  baseurl <- "http://www.biomi.org/eduskunta/?haku=aanestys&id="
  if(is.na(aanestys)) {
     stop('Param aanestys not defined ')
  }
  else {
    search_url <- paste(baseurl,aanestys,sep="")
    ekdat.tree <- XML::xmlParse(search_url)
    ekdat.edustajat <- XML::getNodeSet(ekdat.tree, path="//edustajat/edustaja")
    if(length(ekdat.edustajat) == 0) {
      stop('Virheellinen Aanestys-id')
    }
    df <- XML::xmlToDataFrame(ekdat.edustajat)
    df$valinta <- as.factor(df$valinta)
    df$puolue <- as.factor(df$puolue)
    df$nimi <- as.character(df$nimi)
  }
  return(df)
}

#' Hakee tietyn kansanedustajan aanestykset
#' @param edustaja edustajan nimi muodossa Sukunimi Etunimi
#'
#' @return data.frame
#'
#' @author Juuso Haapanen \email{louhos@@googlegroups.com}
#'
#' @examples
#' # paavo <- GetEdustajanAanestykset('Lipponen Paavo')
#'
#' @references See citation("sorvi") 
#' @keywords eduskunta
#' @export
GetEdustajanAanestykset <- function(edustaja) {

  .InstallMarginal("XML")

  edustaja <- URLencode(edustaja)
  url <- "http://www.biomi.org/eduskunta/?haku=edustaja&id"
  url.haku <- paste(url, edustaja, sep="=")
  edustaja.puu <- XML::xmlParse(url.haku)
  aanestykset <- XML::getNodeSet(edustaja.puu, path='//edustaja/aanestys/tiedot')
  df <- XML::xmlToDataFrame(aanestykset)
  
  return(df)
}

#' Hakee hakusanalla aanestyksia eduskuntarajapinnasta
#' @param hakusana string
#' @return data.frame
#' @author Juuso Haapanen \email{louhos@@googlegroups.com}
#' @examples
#' # luonto <- haeHakusanalla('luonto')
#' @export
#' @references See citation("sorvi") 
#' @keywords eduskunta 

haeHakusanalla <- function(hakusana) {

  .InstallMarginal("XML")
  
  hakusana <- URLencode(hakusana)
  url <- "http://www.biomi.org/eduskunta/?haku=sanahaku&id"
  url.haku <- paste(url, hakusana, sep="=")
  aanestykset.puu <- XML::xmlParse(url.haku)
  aanestykset <- XML::getNodeSet(aanestykset.puu, path="//aanestykset/aanestys/tiedot")
  df <- XML::xmlToDataFrame(aanestykset)
  return(df)
}


