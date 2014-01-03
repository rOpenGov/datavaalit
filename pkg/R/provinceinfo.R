# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2013 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' ConversionTableForMunicipalities
# '
#' Used to create conversion table between municipality names and codes
#'
#' @return conversion table
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ConversionTableForMunicipalities <- function () {

  #sp <- LoadMML(data.id = "kunta1_p", resolution = "1_milj_Shape_etrs_shape")	
  sp <- LoadMML(map.id = "Yleiskartta-1000", data.id = "HallintoAlue")
			 
  df <- as.data.frame(sp)
 
  conversion.table <- df[, c("Kunta", "Kunta.FI")]
  names(conversion.table) <- c("id", "name")

  conversion.table$id <- as.character(conversion.table$id)
  conversion.table$name <- as.character(conversion.table$name)

  #write.csv(conversion.table, file = "~/conversiontable.tab", quote = FALSE, row.names =FALSE)

  conversion.table

}

#' fi.en.maakunnat data documentation 
#'
#' Mappings between Finnish and English province (maakunta) names
#'
#' @name fi.en.maakunnat
#' @docType data
#' @author Leo Lahti \email{louhos@@googlegroups.com} 
#' @usage translations <- LoadData("translations")
#' @format list
#' @keywords data
NULL



#' Get information of Finnish provinces.
#' @aliases get.province.info
#' @param ... Arguments to be passed
#' @return A data frame. With the following entries: Maakunta: province; Pinta-ala: area; Vakiluku: population; Vaestotiheys: population density
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # tab <- GetProvinceInfo()
#' @keywords utilities

GetProvinceInfo <- function (...) {

  .InstallMarginal("XML")
  url <- "http://fi.wikipedia.org/wiki/V%C3%A4est%C3%B6tiheys"
  message(paste("Downloading data from", url))

  # Read tables from the website
  tables <- XML::readHTMLTable(url)

  # Population density in regions (maakunnat)
  tab <- tables[[1]]		

  # tab$Maakunta <- iconv(tab$Maakunta, "latin1", "UTF-8")

  names(tab) <- c("Maakunta", "Pinta.ala", "Vakiluku", "Vaestotiheys")
  tabPinta.ala <- as.numeric(as.character(tab$Pinta.ala))
  tab$Vakiluku <- as.numeric(as.character(tab$Vakiluku))
  tab$Vaestotiheys <- as.numeric(gsub(",", ".", tab$Vaestotiheys))

  tab

}


#' Convert municipality names into standard versions
#'
#' @param municipality.names municipality names to convert
#' @return standardized municipality names
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # tmp <- ConvertMunicipalityNames(municipality.names)
#' @keywords utilities
ConvertMunicipalityNames <- function (municipality.names) {
			 
  municipality.names <- gsub("H\xe4meenkyr\xf6-Tavastkyro", "H\xe4meenkyr\xf6", municipality.names)
  municipality.names <- gsub("M\xe4ntt\xe4", "M\xe4ntt\xe4-Vilppula", municipality.names)
  municipality.names <- gsub("M\xe4ntt\xe4-Vilppula-Vilppula", "M\xe4ntt\xe4-Vilppula", municipality.names)
  municipality.names <- gsub("Peders\xf6ren kunta", "Peders\xf6re", municipality.names)
  municipality.names <- gsub("Maarianhamina - Mariehamn", "Maarianhamina", municipality.names)
  municipality.names <- gsub("L\xe4nsi-Turunmaa", "Parainen", municipality.names)
  municipality.names <- gsub("Koski Tl", "Koski.Tl", municipality.names)
  municipality.names <- gsub("Loimaan kunta", "Loimaan.kunta", municipality.names)
  municipality.names <- gsub("Pieks\xe4m\xe4en mlk", "Pieks\xe4m\xe4en.mlk", municipality.names)
  municipality.names <- gsub("Jyv\xe4skyl\xe4n mlk", "Jyv\xe4skyl\xe4n.mlk", municipality.names)
  municipality.names <- gsub("Rovaniemen mlk", "Rovaniemen.mlk", municipality.names)

  municipality.names <- gsub("Hameenkyr\xf6-Tavastkyro", "Hameenkyr\xf6", municipality.names)
  municipality.names <- gsub("Mantta", "Mantta-Vilppula", municipality.names)
  municipality.names <- gsub("Mantta-Vilppula-Vilppula", "Mantta-Vilppula", municipality.names)
  municipality.names <- gsub("Peders\xf6ren kunta", "Peders\xf6re", municipality.names)
  municipality.names <- gsub("Pedersoren kunta", "Pedersore", municipality.names)
  municipality.names <- gsub("Maarianhamina - Mariehamn", "Maarianhamina", municipality.names)
  municipality.names <- gsub("Lansi-Turunmaa", "Parainen", municipality.names)
  municipality.names <- gsub("Koski Tl", "Koski.Tl", municipality.names)
  municipality.names <- gsub("Loimaan kunta", "Loimaan.kunta", municipality.names)
  municipality.names <- gsub("Pieksamaen mlk", "Pieksamaen.mlk", municipality.names)
  municipality.names <- gsub("Jyvaskylan mlk", "Jyvaskylan.mlk", municipality.names)
  municipality.names <- gsub("Rovaniemen mlk", "Rovaniemen.mlk", municipality.names)


  municipality.names

}

#' Get information of Finnish municipalities from Statistics Finland 2012 
#  (C) Tilastokeskus 2012 http://www.stat.fi/tup/atilastotietokannat/index.html
#' and Maanmittauslaitos (C) MML 2011. For details of MML data, see 
#' help(GetShapeMML).
#' 
#' @aliases get.municipality.info
#' @param ... Arguments to be passed
#' @return A data frame with municipality data
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # municipality.info <- GetMunicipalityInfo()
#' @keywords utilities

GetMunicipalityInfo <- function (...) {

  message("Reading MML data")		    
  mml <- sorvi::GetMunicipalityInfoMML()    # (C) MML 2012

  message("Reading StatFi data")		    
  statfi <- sorvi::GetMunicipalityInfoStatFi() # (C) Tilastokeskus 2013

  message("Combine municipality information from StatFi and MML")
  municipalities <- rownames(statfi)
  municipality.table <- cbind(statfi[municipalities, ], mml[municipalities, ])

  message("Done.")
  # FIXME: merge GetPopulationRegister function in here
  municipality.table

}


#' Get information of Finnish municipalities from Statistics Finland 2013 
#  (C) Tilastokeskus 2013 http://www.stat.fi/tup/atilastotietokannat/index.html
#' 
#' @param verbose verbose 
#' @param ... Arguments to be passed

#' @return A data frame with municipality data
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # tmp <- GetMunicipalityInfo()
#' @keywords utilities

GetMunicipalityInfoStatFi <- function (verbose = TRUE, ...) {

  url <- "http://pxweb2.stat.fi/Database/Kuntien%20perustiedot/Kuntien%20perustiedot/Kuntaportaali.px"			  

  message(paste("Downloading data from", url))

  .InstallMarginal("reshape2")
  .InstallMarginal("reshape")

  # FIXME: merge GetPopulationRegister function in here

  # Get municipality information from Tilastokeskus
  municipality.info <- sorvi::get_statfi(url)

  if (verbose) { message("Cleaning up municipality names") }
  # FIXME: scandinavic characters cause error in Windows systems, find solution
  municipality.info$Alue <- sapply(strsplit(as.character(municipality.info[[grep("Alueluokitus", names(municipality.info))]]), " - "), function (x) {x[[1]]})

  if (verbose) {message("Arrange the data")}
  municipality.info$value <- municipality.info$dat
  
  if (verbose) {message("Converting to wide format")}
  municipality.info <- reshape::cast(municipality.info[, c("Alue", "Tunnusluku", "value")], Alue ~ Tunnusluku) 

  if (verbose) {message("Municipality name conversions")}
  kuntanimi.statfin <- as.character(municipality.info$Alue)
  municipality.info[, "Alue"] <- sorvi::ConvertMunicipalityNames(municipality.info[, "Alue"])
  municipality.info$Alue <- factor(municipality.info$Alue)
  municipality.info[["Kunta.Tilastokeskus"]] <- kuntanimi.statfin
  municipality.info$Kunta <- factor(municipality.info$Alue)
  rownames(municipality.info) <- as.character(municipality.info[["Alue"]])

  # FIXME: Kunta is factor but Maakunta is character and 
  # UTF-8 does not seem to be working with Maakunta field

  if (verbose) {message("All OK. Returning municipality.info")}
  municipality.info

}


#' Get information of Finnish municipalities from Land Survey Finland 2012.
#' (C) Maanmittauslaitos MML 2012. For details of MML data, see 
#' help(GetShapeMML).
#' 
#' @return A data frame with municipality data
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # tab <- GetMunicipalityInfoMML()
#' @keywords utilities

GetMunicipalityInfoMML <- function () {

  .InstallMarginal("reshape2")
  .InstallMarginal("reshape")

  # Municipality information table from Maanmittauslaitos
  mml.table <- LoadMML("Yleiskartta-1000", "HallintoAlue")
  mml.table$Kunta.MML <- mml.table$Kunta.FI
  mml.table <- mml.table[c("AVI.FI", "Kieli.FI", "Suuralue.FI", "Maakunta.FI", "Seutukunta", "Kunta.FI", "Kunta.MML")]
  names(mml.table) <- c("AVI", "Kieli", "Suuralue", "Maakunta", "Seutukunta", "Kunta", "Kunta.MML")
  # Hammarland appears in the table twice but only SHAPE_Leng and SHAPE_Area
  # differ, otherwise the two are identical -> Remove the duplicate 
  rmind <- which(mml.table$Kunta == "Hammarland")[[2]]
  mml.table <- as.data.frame(mml.table)[-rmind,]

  # Use MML municipality names except Parainen:
  # Lansi-Turunmaa changed its name to Parainen since 2012
  kuntanimi <- as.character(mml.table$Kunta)
  kuntanimi[kuntanimi == "L\xensi-Turunmaa"] <- "Parainen"
  rownames(mml.table) <- kuntanimi
  # Drop off Kunta field as redundant
  mml.table <- mml.table[, -which(colnames(mml.table) == "Kunta")]

  # FIXME: Kunta is factor but Maakunta is character and 
  # UTF-8 does not seem to be working with Maakunta field
  
  mml.table

}


#' List province for each municipality in Finland.
#' @aliases municipality2province
#' @param municipalities NULL 
#' @param municipality.info NULL 
#' @return Mapping vector listing the province for each municipality in Finland.
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples 
#' # Info table for municipalities:
#' # municipality.info <- GetMunicipalityInfo()
#' # List all municipalities: 
#' # all.municipalities <- as.character(municipality.info$Kunta) 
#' # Pick province for given municipalities:
#' # mapping between municipalities (kunta) and provinces (maakunta)
#' # m2p <- FindProvince(c("Helsinki", "Tampere", "Turku")) 
#' # Speed up by providing predefined table of municipality info:
#' # m2p <- FindProvince(c("Helsinki", "Tampere", "Turku"), municipality.info)
#' @keywords utilities

FindProvince <- function (municipalities = NULL, municipality.info = NULL) {

  if (is.null(municipality.info)) { 
    municipality.info <- sorvi::GetMunicipalityInfo()
  }

  m2p <- as.character(municipality.info$Maakunta)
  names(m2p) <- as.character(municipality.info$Kunta)

  if (!is.null(municipalities)) {
    m2p <- m2p[municipalities]
  }

  m2p

}


#' Convert between municipality codes and names
#'
#' @param ids NULL 
#' @param names NULL 
#'
#' @return Depending on the input. Converted id or name vector, or full conversion table.
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples  # conversion.table <- ConvertMunicipalityCodes()
#' @keywords utilities

ConvertMunicipalityCodes <- function (ids = NULL, names = NULL) {

  # Created with ConversionTableForMunicipalities()
  conversion.table <- read.csv(paste(system.file("extdata", package = "sorvi"), "/conversiontable.tab", sep = ""))
  conversion.table$id <- as.character(conversion.table$id)
  conversion.table$name <- as.character(conversion.table$name)

  res <- conversion.table

  if (!is.null(ids)) {
    res <- conversion.table$name[match(as.character(ids), conversion.table$id)]
  } else if (!is.null(names)) {
    res <- conversion.table$id[match(as.character(names), conversion.table$name)]
  } 

  res

}
