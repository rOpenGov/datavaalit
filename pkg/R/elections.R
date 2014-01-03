# This file is a part of the sorvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2013 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


# Function for reading the Presidentti2012 data
# For documentation, see
# http://www2.hs.fi/extrat/hsnext/Vaalikone_API_20111207.pdf

#' Load Vaalipiiri information
#' Useful for mapping election data to other municipality information
#'
#' @param ... Arguments to be passed
#'
#' @return data.frame listing election regions (Vaalipiiri), region IDs (Aluenumero) and municipalities (Alue)
#' 
#' @author Juuso Parkkinen and Leo Lahti \email{louhos@@googlegroups.com}
#' @export

GetVaalipiiri <- function (...) {

  url <- "http://www.stat.fi/meta/luokitukset/vaalipiiri/001-2012/luokitusavain_kunta.html"
  message(paste("Downloading data from", url))

  .InstallMarginal("XML")
  
  message(paste("Reading Vaalipiiri information from ", url))
  # Read info of municipalities and election areas from Tilastoteskus
  temp <- XML::readHTMLTable(url)

  # Extract info that we want
  municipalities <- temp[[1]][-1,]
  municipalities$Vaalipiiri <- paste(as.vector(municipalities[,1]), as.vector(municipalities[,2]))
  municipalities <- municipalities[3:5]
  names(municipalities) <- c("Aluenumero", "Alue", "Vaalipiiri")

  # Fill missing Vaalipiiri info
  current.piiri <- NA
  for (i in 1:nrow(municipalities)) {
    # If vaalipiiri given, save it as current
#     if (!nchar(gsub(" ", "", municipalities[i,"Vaalipiiri"])) == 2) {
    if (municipalities[i,"Vaalipiiri"]!=" ") {
      current.piiri <- as.vector(municipalities[i,"Vaalipiiri"])
    } else { # Else add current vaalipiiri
      municipalities[i, "Vaalipiiri"] <- current.piiri
    }
  }

  municipalities

}

  
#' GetParliamentaryElectionData
#'
#' Get parliamentary election data at selected regional level.
#' 
#' @param level Indicate whether to get data at the level of municipality or election.region
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities
GetParliamentaryElectionData <- function (level) {

  .InstallMarginal("reshape2")
  .InstallMarginal("reshape")
  .InstallMarginal("plyr")  

  if (level == "municipality") {

    # http://pxweb2.stat.fi/database/StatFin/vaa/evaa/evaa_fi.asp

    # 2.2 Aanioikeutetut ja aanestaneet seka ennakolta aanestaneet sukupuolen mukaan kunnittain eduskuntavaaleissa 2011 ja 2007
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/evaa_2011/120_evaa_tau_104_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- try(as.data.frame(px))
    # Manually remove special chars from header
    #names(df) <- c("Aanestystiedot", "Lukumaaratiedot", "Vaalipiiri.ja.kunta", "dat")
    names(df) <- korvaa.skandit(names(df))
    for (i in 1:ncol(df)) {
      df[, i] <- korvaa.skandit(df[, i])
    }

    kaava <- as.formula("Vaalipiiri.ja.kunta~Aanestystiedot~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")

    # Separate tables and preprocess
    tab1 <- tmp[,,"Lukumaara 2007"]
    tab2 <- tmp[,,"Lukumaara 2011"]
    tab3 <- tmp[,,"-Osuus aanista"]
    tab4 <- tmp[,,"- Osuus aanista"]

    colnames(tab1) <- paste(colnames(tmp[,,"Lukumaara 2007"]), "(Lukumaara 2007)")
    colnames(tab2) <- paste(colnames(tmp[,,"Lukumaara 2011"]), "(Lukumaara 2011)")
    colnames(tab3) <- paste(colnames(tmp[,,"-Osuus aanista"]), "(Osuus 2011)")
    colnames(tab4) <- paste(colnames(tmp[,,"- Osuus aanista"]), "(Osuus 2007)")
    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Niista Ruotsissa", rnams)]
    rnams <- rnams[-grep("Suomessa asuvat Suomen kansalaiset", rnams)]
    rnams <- rnams[-grep("Ulkomailla asuvat Suomen kansalaiset", rnams)]
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # FIXME: make generic function to unify municipality names that have multiple versions
    # across different data sets
    rownames(tab) <- gsub("Pedersoren kunta", "Pedersore", rownames(tab))

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})

    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]

    # TODO
    #8.2 Pienin aanimaara ja vertausluku, jolla ehdokas on tullut valituksi 
    # puolueittain ja vaalipiireittain eduskuntavaaleissa 2011
    #url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/186_evaa_tau_102_fi.px"
    #Alue~Puolue~Pienimmat.luvut

  } else if (level == "election.region") {

    #http://pxweb2.stat.fi/database/StatFin/vaa/evaa/evaa_fi.asp

    #2.3 Hylatyt aanestysliput hylkaysperusteen ja vaalipiirin mukaan 
    # eduskuntavaaleissa 2011
    #http://pxweb2.stat.fi/database/StatFin/vaa/evaa/120_evaa_tau_105_fi.px

    # 8.1 Vaaliliitot ja niiden aanimaarat vaalipiireittain eduskuntavaaleissa 2011
    #url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/185_evaa_tau_101_fi.csv.gz"  
    #Vaaliliitto.Puolue.Vaalipiiri~Lukumaara
  
    #2.1 Aanioikeutetut ja aanestaneet seka ennakolta aanestaneet sukupuolen 
    # mukaan vaalipiireittain eduskuntavaaleissa 2011
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/120_evaa_tau_103_fi.px"

    # Read election data from Statistics Finland			 
    px <- sorvi::read.px(url, na.strings='"-"') 
    df <- try(as.data.frame(px))
    names(df) <- korvaa.skandit(names(df))
    #names(df) <- c("Aanestystiedot", "Lukumaaratiedot", "Vaalipiiri.ja.kunta", "dat")

    kaava <- as.formula("Vaalipiiri~Aanestystiedot~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")

    # Separate the tables
    tab1 <- tmp[,,1]
    tab2 <- tmp[,,2]
    colnames(tab1) <- paste(colnames(tmp[,,"Lukumaara"]), "(Lukumaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus aanista"]), "(Osuus aanista)")
    tab <- cbind(tab1, tab2)

    # Keep only election.region level data
    rnams <- rownames(tab)
    rnams <- rnams[grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    colnames(tab) <- paste("Eduskuntavaalit 2011", colnames(tab))

    tab$Vaalipiiri <- sapply(rnams, function (s) {ss <- strsplit(s, " ")[[1]]; paste(ss[-1], collapse = " ")})
    tab$Vaalipiiri.Koodi <- sapply(rnams, function (s) {strsplit(s, " ")[[1]][[1]]})

    # Read more election data from Statistics Finland			 
    px <- sorvi::read.px("http://pxweb2.stat.fi/database/StatFin/vaa/evaa/120_evaa_tau_105_fi.px", na.strings='"-"') 
    df <- try(as.data.frame(px))
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri~Hylkaysperuste")
    tab2 <- reshape::cast(df, kaava, value="dat")

    # Keep only election.region level data
    rownames(tab2) <- as.character(tab2[,1])
    rnams <- rownames(tab2)
    rnams <- rnams[grep("vaalipiiri", rnams)]
    tab2 <- as.data.frame(tab2[rnams, ])

    colnames(tab2) <- paste("Eduskuntavaalit 2011", colnames(tab2))

    tab2$Vaalipiiri <- sapply(rnams, function (s) {ss <- strsplit(s, " ")[[1]]; paste(ss[-1], collapse = " ")})
    tab2$Vaalipiiri.Koodi <- sapply(rnams, function (s) {strsplit(s, " ")[[1]][[1]]})


    tab <- cbind(tab, tab2[match(tab$Vaalipiiri, tab2$Vaalipiiri),])
  
  }

  rownames(tab) <- tab$Kunta
  colnames(tab) <- paste("Eduskuntavaalit_2007_2011", colnames(tab))
  

  tab

}


  
#' GetMunicipalElectionData2000
#'
#' Get municipal election data from Statistics Finland (C) 2012
#' http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/kvaa_2000_fi.asp
#' 
#' @param which Indicate which of the available Statistics Finland data sets to parse. Options: election.statistics, candidates, selected.candidates.by.region, selected.candidates.all, parties, all.municipality.level.data
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities
GetMunicipalElectionData2000 <- function (which = "election.statistics") {

  .InstallMarginal("plyr")
  .InstallMarginal("reshape2")
  .InstallMarginal("reshape")

  if (which == "election.statistics") {

    #Kunnallisvaalit 2000, aanestystiedot
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/010_kvaa_2000_2008-10-17_tau_101_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Alue ~ Aanestystiedot")
    tab <- reshape::cast(df, kaava, value = "dat")
    rownames(tab) <- korvaa.skandit(as.character(tab$Alue))

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(as.character(rownames(tab)), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "candidates") {

    #Ehdokkaat puolueittain vaalipiirin ja kunnan mukaan kunnallisvaaleissa 2000
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/020_kvaa_2000_2008-10-17_tau_102_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)

    tmp <- reshape::cast(df, Alue ~ Puolue ~ Ehdokastiedot, value="dat")

    tab1 <- tmp[,,"Ehdokkaiden lkm"]
    tab2 <- tmp[,,"Ehdokkaiden osuus (%)"]
    tab3 <- tmp[,,"Naisehdokkaiden lkm"]
    tab4 <- tmp[,,"Naisten osuus ehdokkaista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Ehdokkaiden lkm"]), "(Ehdokkaiden lkm)")
    colnames(tab2) <- paste(colnames(tmp[,,"Ehdokkaiden osuus (%)"]), "(Ehdokkaiden osuus)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naisehdokkaiden lkm"]), "(Naisehdokkaiden lkm)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisten osuus ehdokkaista (%)"]), "(Naisten osuus ehdokkaista)")
    tab <- cbind(tab1, tab2, tab3, tab4)
    rownames(tab) <- korvaa.skandit(rownames(tab))

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "selected.candidates.by.region") {

    #Valitut puolueittain vaalipiirin ja kunnan mukaan kunnallisvaaleissa 2000
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/030_kvaa_2000_2008-10-17_tau_103_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    tmp <- reshape::cast(df, Alue ~ Puolue ~ Valittujen.tiedot, value="dat")

    tab1 <- tmp[,,"Valittujen lkm"]
    tab2 <- tmp[,,"Valittujen osuus (%)"]
    tab3 <- tmp[,,"Valittujen naisten lkm"]
    tab4 <- tmp[,,"Naisten osuus valituista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Valittujen lkm"]), "(Valittujen lkm)")
    colnames(tab2) <- paste(colnames(tmp[,,"Valittujen osuus (%)"]), "(Valittujen osuus)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valittujen naisten lkm"]), "(Naisehdokkaiden lkm)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisten osuus valituista (%)"]), "(Naisten osuus valituista)")
    tab <- cbind(tab1, tab2, tab3, tab4)
    rownames(tab) <- korvaa.skandit(rownames(tab))

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "parties") {

    #Kunnallisvaalit 2000, puolueiden kannatus
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/040_kvaa_2000_2008-10-17_tau_104_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)

    tmp <- reshape::cast(df, Alue ~ Puolue ~ Kannatustiedot, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aania yhtensa"]
    tab2 <- tmp[,,"Ennakkoaanet"]
    tab3 <- tmp[,,"Naisehdokkaiden aanimaara"]
    tab4 <- tmp[,,"Naisehdokkaiden osuus aanista (%)"]
    tab5 <- tmp[,,"Osuus aanista (%)"]
    tab6 <- tmp[,,"Osuus ennakkoaanista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aania yhtensa"]), "(Aania yhteensa)")
    colnames(tab2) <- paste(colnames(tmp[,,"Ennakkoaanet"]), "(Ennakkoaanet)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naisehdokkaiden aanimaara"]), "(Naisehdokkaiden aanimaara)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisehdokkaiden osuus aanista (%)"]), "(Naisehdokkaiden osuus aanista (%))")
    colnames(tab5) <- paste(colnames(tmp[,,"Osuus aanista (%)"]), "(Osuus aanista (%))")
    colnames(tab6) <- paste(colnames(tmp[,,"Osuus ennakkoaanista (%)"]), "(Osuus ennakkoaanista (%))")

    tab <- cbind(tab1, tab2, tab3, tab4, tab5, tab6)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "selected.candidates.all") {

    #Kunnallisvaalit 2000, valitut ehdokkaat
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/050_kvaa_2000_2008-10-17_tau_105_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    tab <- reshape::cast(df, Ehdokas ~ Ehdokastiedot, value="dat")

  } else if (which == "all.municipality.level.data") {

    tab1 <- sorvi::GetMunicipalElectionData2000("election.statistics")
    tab2 <- sorvi::GetMunicipalElectionData2000("candidates")
    tab3 <- sorvi::GetMunicipalElectionData2000("selected.candidates.by.region")
    tab4 <- sorvi::GetMunicipalElectionData2000("parties")

    municipalities <- sort(rownames(tab1))
    tab <- cbind(tab1[municipalities, ],
             tab2[municipalities, ],
      	     tab3[municipalities, ],
      	     tab4[municipalities, ])
  }

  tab

}

  
#' GetMunicipalElectionData2004
#'
#' Get municipal election data from Statistics Finland 2012
#' 
#' Taulukot tilastossa: 5. Kunnallisvaalit 2004 - vaalitulos, aanestaminen
#' http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/2004_05_fi.asp
#'
#' @param which Indicate which of the available Statistics Finland data sets to parse. 
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities
GetMunicipalElectionData2004 <- function (which = "election.statistics") {

  .InstallMarginal("plyr")
  .InstallMarginal("reshape2")
  .InstallMarginal("reshape")

  if (which == "election.statistics") {

    #Kunnallisvaalit 2004, aanestystiedot
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/010_KVAA_2004_2008-07-23_TAU_101_FI.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))    

    kaava <- as.formula("Alue~Aanestystiedot~Sukupuoli")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Sukupuolet yhteensa"]
    tab2 <- tmp[,,"Miehet"]
    tab3 <- tmp[,,"Naiset"]

    colnames(tab1) <- paste(colnames(tmp[,,"Sukupuolet yhteensa"]), "(Sukupuolet yhteensa)")
    colnames(tab2) <- paste(colnames(tmp[,,"Miehet"]), "(Miehet)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naiset"]), "(Naiset)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep(" 00", rnams)]
    rnams <- rnams[-grep(" 01", rnams)]
    rnams <- rnams[-grep(" 02", rnams)]
    rnams <- rnams[-grep(" 03", rnams)]
    rnams <- rnams[-grep(" 04", rnams)]
    rnams <- rnams[-grep(" 05", rnams)]
    rnams <- rnams[-grep(" 06", rnams)]
    rnams <- rnams[-grep(" 07", rnams)]
    rnams <- rnams[-grep(" 08", rnams)]
    rnams <- rnams[-grep(" 09", rnams)]
    rnams <- rnams[-grep(" 1", rnams)]
    rnams <- rnams[-grep(" 2", rnams)]
    rnams <- rnams[-grep(" 3", rnams)]
    rnams <- rnams[-grep(" 4", rnams)]
    rnams <- rnams[-grep(" 5", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    #rnams <- rnams[-grep(" 6", rnams)]
    #rnams <- rnams[-grep(" 7", rnams)]
    #rnams <- rnams[-grep(" 8", rnams)]
    #rnams <- rnams[-grep(" 9", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: coarse election region (vaalipiiri) information also available but discarded
    # NOTE: detailed election region information also available (below municipality level) but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 aanestystiedot", colnames(tab))

  } else if (which == "selected.candidates.by.election.region") {

    warning("Vaalipiiri level information; TODO")

    #Valittujen lukumaara ja prosenttiosuudet puolueittain ja vaalipiireittain kunnallisvaaleissa 2004
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-28_tau_107_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Puolue~Vaalipiiri~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Valtuutettujen lukumaara"]
    tab2 <- tmp[,,"Puolueen osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Valtuutettujen lukumaara"]), "(Valtuutettujen lukumaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Puolueen osuus"]), "(Puolueen osuus)")

    tab <- cbind(tab1, tab2)
    
    tab <- NULL

  } else if (which == "selected.candidates.count") {

    # Kunnallisvaalit 2004, valittujen lukumaara
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/010_kvaa_2004_2008-08-28_tau_103_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Alue~Puolue~Sukupuoli~Valittujen.lukumaara")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Kaikki ehdokkaat", "Valittujen lukumaara"]
    colnames(tab1) <- paste("Kaikki ehdokkaat", "Valittujen lukumaara", colnames(tab1))

    tab2 <- tmp[,,"Miesehdokkaat", "Valittujen lukumaara"]
    colnames(tab2) <- paste("Miesehdokkaat", "Valittujen lukumaara", colnames(tab2))

    tab3 <- tmp[,,"Naisehdokkaat", "Valittujen lukumaara"]
    colnames(tab3) <- paste("Naisehdokkaat", "Valittujen lukumaara", colnames(tab3))

    tab4 <- tmp[,,"Kaikki ehdokkaat", "Osuus valituista %"]
    colnames(tab4) <- paste("Kaikki ehdokkaat", "Osuus valituista %", colnames(tab4))

    tab5 <- tmp[,,"Miesehdokkaat", "Osuus valituista %"]
    colnames(tab5) <- paste("Miesehdokkaat", "Osuus valituista %", colnames(tab5))

    tab6 <- tmp[,,"Naisehdokkaat", "Osuus valituista %"]
    colnames(tab6) <- paste("Naisehdokkaat", "Osuus valituista %", colnames(tab6))

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,], 
    	         tab4[regs,], tab5[regs,], tab6[regs,])

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    
    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 valittujen lukumaara", colnames(tab))
    
  } else if (which == "selected.candidates.by.party") {  

    # Valittujen lukumaara ja prosenttiosuudet puolueittain ja 
    # vaalipiireittain kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/650_kvaa_2004_2009-11-02_tau_141_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    #tmp <- reshape::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumaara)

  } else if (which == "selected.candidates.count") {

    warning("Puoluetason tietoa, ei kuntia. TODO.")
    tab <- NULL

    #Valitut ikaryhmittain sukupuolen ja puolueen mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-07-15_tau_110_fi.px"

  } else if (which == "selected.candidates.count") {

    #Valitut ikaryhmittain sukupuolen mukaan vaalipiireittain kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/670_kvaa_2004_2009-11-02_tau_143_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    #tmp <- reshape::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumaara)

  } else if (which == "parties") {

    #Kunnallisvaalit 2004, puolueiden kannatus
    #url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/010_KVAA_2004_2008-08-28_TAU_102_FI.px"
    #df <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    # -> Segmentation fault
    warning("Segmentation fault at Kunnallisvaalit 2004, puolueiden kannatus, ignoring.")
    tab <- NULL

  } else if (which == "parties.per.region") {

    # Puolueiden aanimaarat ja prosenttiosuudet seka aanestysprosentit 
    # vaalipiireittain kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-27_tau_114_fi.px"
    warning("Vaalipiiri level, TODO")
    tab <- NULL

  } else if (which == "parties.change") {

    # Puolueiden aanimaarat ja aanestysprosentti seka valittujen lukumaara 
    # kunnittain kunnallisvaaleissa 2004 ja muutos edellisiin vaaleihin 
    # verrattuna
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-27_tau_111_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri.ja.kunta~Puolue~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aanimaara"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Muutos edelliseen vaaliin verrattuna"]
    tab4 <- tmp[,,"Valittujen lukumaara"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aanimaara"]), "(Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Muutos edelliseen vaaliin verrattuna"]), "(Muutos edelliseen vaaliin verrattuna)")
    colnames(tab4) <- paste(colnames(tmp[,,"Valittujen lukumaara"]), "(Valittujen lukumaara)")

    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 puolueiden aanimaarat: ", colnames(tab))

  } else if (which == "party.votes") {

    #Puolueiden aanimaarat ja valittujen lukumaara kunnittain (pienet puolueet), hylatyt liput seka ennakkoaanestaneet kunnallisvaaleissa 2004
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-27_tau_114_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri.ja.kunta~Aanestystiedot.ja.puolueiden.kannatus~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aanimaara"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Valittujen lukumaara"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aanimaara"]), "(Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valittujen lukumaara"]), "(Valittujen lukumaara)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 puolueiden kannatus: ", colnames(tab))

  } else if (which == "voting.stats") {

    # Aanioikeutetut ja aanestaneet sukupuolen mukaan, hyvaksytyt aanestysliput, valtuutetuiksi valitut ja ennakkoaanet puolueittain seka hylattyjen 
    # aanestyslippujen lukumaara kunnittain kunnallisvaaleissa 2004
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-28_tau_116_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri.ja.kunta~Aanestystiedot.ja.puolueiden.kannatus~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Lukumaara / Aanimaara"]
    tab2 <- tmp[,,"Osuus aanista"]
    tab3 <- tmp[,,"Valitut"]
    tab4 <- tmp[,,"Osuus valituista"]
    tab5 <- tmp[,,"Ennakkoaanet"]
    tab6 <- tmp[,,"Ennakkoaanten osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Lukumaara / Aanimaara"]), "(Lukumaara / Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus aanista"]), "(Osuus aanista)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valitut"]), "(Valitut)")
    colnames(tab4) <- paste(colnames(tmp[,,"Osuus valituista"]), "(Osuus valituista)")
    colnames(tab5) <- paste(colnames(tmp[,,"Ennakkoaanet"]), "(Ennakkoaanet)")
    colnames(tab6) <- paste(colnames(tmp[,,"Ennakkoaanten osuus"]), "(Ennakkoaanten osuus)")

    tab <- cbind(tab1, tab2, tab3, tab4, tab5, tab6)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 muuta: ", colnames(tab))

  } else if (which == "previous.experience") {

    # Valituiksi tulleiden aikaisempi kokemus valtuustossa kuntatyypin, sukupuolen ja puolueen mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/730_kvaa_2004_2009-12-30_tau_149_fi.px"
    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "rejected") {

    #Hylatyt aanestysliput hylkaysperusteen ja vaalipiirin mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/740_kvaa_2004_2009-12-30_tau_150_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "results") {

    #Kunnallisvaalit 2004, tulosanalyysi
    #http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_06/2004_06_fi.asp
    
    warning("No municipality level data available. TODO.")
    # NOTE: vaalipiiri level available
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_06/810_kvaa_2004_2004-10-27_tau_150_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    tab <- NULL

  } else if (which == "pre") {

    #Ennakkoon aanestaneet aanestyspaikan ja vaalipiirin mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/750_kvaa_2004_2009-12-30_tau_151_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad") {

    # Suomen ulkomaan edustustoissa ja laivoissa aanestaneet sukupuolen mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/760_kvaa_2004_2009-12-30_tau_152_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad2") {

    #Aanioikeutetut ja aanestaneet ulkomaalaiset vaalipiirin mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/770_kvaa_2004_2009-12-30_tau_153_fi.px"
    kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "all.municipal") {

    tab1 <- sorvi::GetMunicipalElectionData2004("voting.stats")
    tab2 <- sorvi::GetMunicipalElectionData2004("party.votes")
    tab3 <- sorvi::GetMunicipalElectionData2004("parties.change")
    tab4 <- sorvi::GetMunicipalElectionData2004("selected.candidates.count")
    tab6 <- sorvi::GetMunicipalElectionData2004("election.statistics")

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,],
    	         tab4[regs,], tab6[regs,])

  } 
  tab

}


#' GetElectedCandidates
#'
#' Get data on elected candidates 
#' 
#' @param year election year
#' @param election election type (municipal / parliament / president / ...)
#' @param election.district election.district in numeric or character format (for instance: 2 or "Uudenmaan vaalipiiri")
#' @param verbose verbose
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities
GetElectedCandidates <- function (year, election, election.district, verbose = TRUE) {

  .InstallMarginal("plyr")
  .InstallMarginal("reshape2")
  .InstallMarginal("reshape")

  if (verbose) {message(paste(election.district))}		     

  # Convert IDs to names if needed
  convtab <- .datavaalit.idconversions(type = "election.district.id") 
  if (as.character(election.district) %in% convtab$id) {
    election.district.id <- election.district
    election.district.name <- .datavaalit.idconversions(election.district, type = "election.district.id")
  } else{
    election.district.name <- election.district
    election.district.id <- .datavaalit.idconversions(election.district, type = "election.district.id")
  }

  if (as.numeric(year) == 2012 && election == "municipal") {

    message(paste(election, year))

    # List URLs for Statfi election candidate tables 2012
    # Source (C) Tilastokeskus:
    # http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/2012_04_fi.asp
    urls <- c()
    urls[["Helsingin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/410_kvaa_2012_2013-01-07_tau_123_fi.px"               
    urls[["Uudenmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/420_kvaa_2012_2013-01-07_tau_124_fi.px"
    urls[["Varsinais-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/430_kvaa_2012_2013-01-07_tau_125_fi.px"
    urls[["Satakunnan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/440_kvaa_2012_2013-01-07_tau_126_fi.px"
    urls[["Hameen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/460_kvaa_2012_2013-01-07_tau_127_fi.px"
    urls[["Pirkanmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/470_kvaa_2012_2013-01-07_tau_128_fi.px"
    urls[["Kymen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/480_kvaa_2012_2013-01-07_tau_129_fi.px"
    urls[["Etela-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/490_kvaa_2012_2013-01-07_tau_130_fi.px"
    urls[["Pohjois-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/500_kvaa_2012_2013-01-07_tau_131_fi.px"
    urls[["Pohjois-Karjalan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/510_kvaa_2012_2013-01-07_tau_132_fi.px"
    urls[["Vaasan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/520_kvaa_2012_2013-01-07_tau_133_fi.px"
    urls[["Keski-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/530_kvaa_2012_2013-01-07_tau_134_fi.px"
    urls[["Oulun vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/540_kvaa_2012_2013-01-07_tau_135_fi.px"
    urls[["Lapin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2012_04/550_kvaa_2012_2013-01-07_tau_136_fi.px"

    url <- urls[[election.district.name]]

  } else if (as.numeric(year) == 2008 && election == "municipal") {

    # List URLs for Statfi election candidate tables 2008
    # Source (C) Tilastokeskus:
    # http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/2008_04_fi.asp
    urls <- list()
    urls[["Helsingin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/410_kvaa_2008_2009-11-02_tau_123_fi.px"
    urls[["Uudenmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/420_kvaa_2008_2009-11-02_tau_124_fi.px"
    urls[["Varsinais-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/430_kvaa_2008_2009-11-02_tau_125_fi.px"
    urls[["Satakunnan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/440_kvaa_2008_2009-11-02_tau_126_fi.px"
    urls[["Hameen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/460_kvaa_2008_2009-11-02_tau_127_fi.px"
    urls[["Pirkanmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/470_kvaa_2008_2009-11-02_tau_128_fi.px"
    urls[["Kymen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/480_kvaa_2008_2009-11-02_tau_129_fi.px"
    urls[["Etela-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/490_kvaa_2008_2009-11-02_tau_130_fi.px"
    urls[["Pohjois-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/500_kvaa_2008_2009-11-02_tau_131_fi.px"
    urls[["Pohjois-Karjalan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/510_kvaa_2008_2009-11-02_tau_132_fi.px"
    urls[["Vaasan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/520_kvaa_2008_2009-11-02_tau_133_fi.px"
    urls[["Keski-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/530_kvaa_2008_2009-11-02_tau_134_fi.px"
    urls[["Oulun vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/540_kvaa_2008_2009-11-02_tau_135_fi.px"
    urls[["Lapin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_04/550_kvaa_2008_2009-11-02_tau_136_fi.px"

    url <- urls[[election.district.name]]

  } else if (as.numeric(year) == 2004 && election == "municipal") {
    # List URLs for Statfi election candidate tables 2004
    # Source (C) Tilastokeskus:
    # http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/2004_04_fi.asp

    urls <- list()
    urls[["Helsingin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_101_FI.px"
    urls[["Uudenmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_102_FI.px"
    urls[["Varsinais-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_103_FI.px"
    urls[["Satakunnan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_104_FI.px"
    urls[["Hameen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_106_FI.px"
    urls[["Pirkanmaan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_107_FI.px"
    urls[["Kymen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_108_FI.px"
    urls[["Etela-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_109_FI.px"
    urls[["Pohjois-Savon vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_110_FI.px"
    urls[["Pohjois-Karjalan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_111_FI.px"
    urls[["Vaasan vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_112_FI.px"
    urls[["Keski-Suomen vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_113_FI.px"
    urls[["Oulun vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_114_FI.px"
    urls[["Lapin vaalipiiri"]] <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_04/040_KVAA_2004_2008-07-17_TAU_115_FI.px"

    url <- urls[[election.district.name]]

  } else {
    warning(paste("Option", election, year, "not implemented"))
  }

  if (verbose) { message("Reading PC Axis file") }
  px <- read.px(url)

  if (verbose) { message("Converting to data frame") }
  df <- as.data.frame(px)

  # Circumvent problems with special chars
  df$Aanestysalue <- df[[grep("nestysalue", names(df))]]
  df$Aanestystiedot <- df[[grep("nestystiedot", names(df))]]

  if (verbose) { message("Splitting by candidate") }
  df <- split(df, df$Ehdokas)

  if (verbose) { message("Converting into more compact table format") }
  
  kaava <- as.formula("Ehdokas + Aanestysalue ~ Aanestystiedot")

  df <- lapply(df, function(dff) {m <- reshape2::melt(dff, c("Ehdokas", "Aanestysalue", "Aanestystiedot"), "dat"); mc <- reshape::cast(m, kaava); mc <- mc[!mc[["Ehdokkaan numero"]] == 0, ]})
  
  df <- do.call(rbind, df)

  if (verbose) { message("Preprocessing fields") }
  df$Ehdokas <- gsub(" / ", "/", as.character(df$Ehdokas))
  ehd <- do.call(rbind, strsplit(df$Ehdokas, "/"))
  df[["Ehdokkaan nimi"]] <- ehd[, 1]
  df[["Puolue_lyhenne_fi"]] <- ehd[, 2]
  rm(ehd)
  df$Sukunimi <- sapply(strsplit(df[["Ehdokkaan nimi"]], " "), function (x) {x[[1]]})
  df$Etunimi <- sapply(strsplit(df[["Ehdokkaan nimi"]], " "), function (x) {paste(x[-1], collapse = " ")})
  df[["Ehdokkaan nimi"]] <- NULL

  if (verbose) { message("Preprocessing region fields") }
  df[["Aanestysalue"]] <- gsub(" / ", "/", as.character(df[["Aanestysalue"]]))
  alue <- do.call(rbind, strsplit(df[["Aanestysalue"]], "/"))
  df$Kunta <- alue[, 1]
  df$Alue <- alue[, 2]
  rownames(df) <- NULL

  # Add fields for compatibility
  df$Vaalipiirinumero <- election.district.id
  df$Vaalipiiri_fi <- election.district.name
  df$Vaalilaji <- "K"
  df[["Ehdokasnumero"]] <- df[["Ehdokkaan numero"]]
  df[["Ehdokkaan numero"]] <- NULL
  
  df$Vaalilaji_nimi_fi <- .datavaalit.idconversions(tolower(df$Vaalilaji), type = "election.id") 

  # Clean up memory
  gc()

  df

}

  
#' GetMunicipalElectionData2008
#'
#' Get municipal election data 
#' 
#' @param which Indicate which of the available Statistics Finland data sets to parse. Options: 
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities
GetMunicipalElectionData2008 <- function (which = "election.statistics") {

  .InstallMarginal("plyr")
  .InstallMarginal("reshape")
  .InstallMarginal("reshape2")

  # Taulukot tilastossa: 5. Kunnallisvaalit 2008 - vaalitulos, aanestaminen
  # http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/2008_05_fi.asp

  if (which == "election.statistics") {

    #Kunnallisvaalit 2008, aanestystiedot
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/610_kvaa_2008_2009-10-30_tau_137_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Alue~Aanestystiedot~Sukupuoli")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Sukupuolet yhteensa"]
    tab2 <- tmp[,,"Miehet"]
    tab3 <- tmp[,,"Naiset"]

    colnames(tab1) <- paste(colnames(tmp[,,"Sukupuolet yhteensa"]), "(Sukupuolet yhteensa)")
    colnames(tab2) <- paste(colnames(tmp[,,"Miehet"]), "(Miehet)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naiset"]), "(Naiset)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep(" 00", rnams)]
    rnams <- rnams[-grep(" 01", rnams)]
    rnams <- rnams[-grep(" 02", rnams)]
    rnams <- rnams[-grep(" 03", rnams)]
    rnams <- rnams[-grep(" 04", rnams)]
    rnams <- rnams[-grep(" 05", rnams)]
    rnams <- rnams[-grep(" 06", rnams)]
    rnams <- rnams[-grep(" 07", rnams)]
    rnams <- rnams[-grep(" 08", rnams)]
    rnams <- rnams[-grep(" 09", rnams)]
    rnams <- rnams[-grep(" 1", rnams)]
    rnams <- rnams[-grep(" 2", rnams)]
    rnams <- rnams[-grep(" 3", rnams)]
    rnams <- rnams[-grep(" 4", rnams)]
    rnams <- rnams[-grep(" 5", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    #rnams <- rnams[-grep(" 6", rnams)]
    #rnams <- rnams[-grep(" 7", rnams)]
    rnams <- rnams[-grep(" 8", rnams)]
    #rnams <- rnams[-grep(" 9", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: coarse election region (vaalipiiri) information also available but discarded
    # NOTE: detailed election region information also available (below municipality level) but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 aanestystiedot", colnames(tab))

  } else if (which == "woman.candidates") {

    #Naisehdokkaitten vaalitiedot puolueen ja kunnan mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/720_kvaa_2008_2009-12-30_tau_148_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    tmp <- reshape::cast(df, Kunta~Puolue~Naisehdokastiedot)
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aanimaara"]
    tab2 <- tmp[,,"Osuus aanista (%)"]
    tab3 <- tmp[,,"Ehdokkaat"]
    tab4 <- tmp[,,"Osuus ehdokkaista (%)"]
    tab5 <- tmp[,,"Valitut"]
    tab6 <- tmp[,,"Osuus valituista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aanimaara"]), "(Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus aanista (%)"]), "(Osuus aanista (%))")
    colnames(tab3) <- paste(colnames(tmp[,,"Ehdokkaat"]), "(Ehdokkaat)")
    colnames(tab4) <- paste(colnames(tmp[,,"Osuus ehdokkaista (%)"]), "(Osuus ehdokkaista (%))")
    colnames(tab5) <- paste(colnames(tmp[,,"Valitut"]), "(Valitut)")
    colnames(tab6) <- paste(colnames(tmp[,,"Osuus valituista (%)"]), "(Osuus valituista (%))")

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,], tab4[regs,], tab5[regs,], tab6[regs,])

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 naisehdokkaat", colnames(tab))

  } else if (which == "selected.candidates.by.election.region") {

    warning("Vaalipiiri level information; TODO")

    #Valittujen lukumaara ja prosenttiosuudet puolueittain ja vaalipiireittain kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/690_kvaa_2008_2009-11-02_tau_145_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    kaava <- as.formula("Puolue~Vaalipiiri~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Valtuutettujen lukumaara"]
    tab2 <- tmp[,,"Puolueen osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Valtuutettujen lukumaara"]), "(Valtuutettujen lukumaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Puolueen osuus"]), "(Puolueen osuus)")

    tab <- cbind(tab1, tab2)
    
    tab <- NULL

  } else if (which == "selected.candidates.count") {

    # Kunnallisvaalit 2008, valittujen lukumaara
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/630_kvaa_2008_2009-10-30_tau_139_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))    

    kaava <- as.formula("Alue~Puolue~Sukupuoli~Valittujen.lukumaara")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Kaikki ehdokkaat", "Valittujen lukumaara"]
    colnames(tab1) <- paste("Kaikki ehdokkaat", "Valittujen lukumaara", colnames(tab1))

    tab2 <- tmp[,,"Miesehdokkaat", "Valittujen lukumaara"]
    colnames(tab2) <- paste("Miesehdokkaat", "Valittujen lukumaara", colnames(tab2))

    tab3 <- tmp[,,"Naisehdokkaat", "Valittujen lukumaara"]
    colnames(tab3) <- paste("Naisehdokkaat", "Valittujen lukumaara", colnames(tab3))

    tab4 <- tmp[,,"Kaikki ehdokkaat", "Osuus valituista %"]
    colnames(tab4) <- paste("Kaikki ehdokkaat", "Osuus valituista %", colnames(tab4))

    tab5 <- tmp[,,"Miesehdokkaat", "Osuus valituista %"]
    colnames(tab5) <- paste("Miesehdokkaat", "Osuus valituista %", colnames(tab5))

    tab6 <- tmp[,,"Naisehdokkaat", "Osuus valituista %"]
    colnames(tab6) <- paste("Naisehdokkaat", "Osuus valituista %", colnames(tab6))

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,], 
    	         tab4[regs,], tab5[regs,], tab6[regs,])

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    
    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 valittujen lukumaara", colnames(tab))
    
  } else if (which == "selected.candidates.by.party") {  

    # Valittujen lukumaara ja prosenttiosuudet puolueittain ja 
    # vaalipiireittain kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/650_kvaa_2008_2009-11-02_tau_141_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    #tmp <- reshape::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumaara)

  } else if (which == "selected.candidates.count") {

    warning("Puoluetason tietoa, ei kuntia. TODO.")
    tab <- NULL

    # Valitut ikaryhmittain sukupuolen ja puolueen mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/660_kvaa_2008_2009-11-02_tau_142_fi.px"

  } else if (which == "selected.candidates.count") {

    #Valitut ikaryhmittain sukupuolen mukaan vaalipiireittain kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/670_kvaa_2008_2009-11-02_tau_143_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    #tmp <- reshape::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumaara)

  } else if (which == "parties") {

    #Kunnallisvaalit 2008, puolueiden kannatus
    #url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/620_kvaa_2008_2009-10-30_tau_138_fi.px"
    #df <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    # -> Segmentation fault
    warning("Segmentation fault at Kunnallisvaalit 2008, puolueiden kannatus, ignoring.")
    tab <- NULL

  } else if (which == "parties.per.region") {

    # Puolueiden aanimaarat ja prosenttiosuudet seka aanestysprosentit vaalipiireittain kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/640_kvaa_2008_2009-11-02_tau_140_fi.px"

    warning("Vaalipiiri level, TODO")
    tab <- NULL

  } else if (which == "parties.change") {

    # Puolueiden aanimaarat ja aanestysprosentti seka valittujen lukumaara
    # kunnittain kunnallisvaaleissa 2008 ja muutos edellisiin vaaleihin 
    # verrattuna
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/680_kvaa_2008_2009-11-02_tau_144_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri.ja.kunta~Puolue~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aanimaara"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Muutos edelliseen vaaliin verrattuna"]
    tab4 <- tmp[,,"Valittujen lukumaara"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aanimaara"]), "(Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Muutos edelliseen vaaliin verrattuna"]), "(Muutos edelliseen vaaliin verrattuna)")
    colnames(tab4) <- paste(colnames(tmp[,,"Valittujen lukumaara"]), "(Valittujen lukumaara)")

    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 puolueiden aanimaarat: ", colnames(tab))

  } else if (which == "party.votes") {

    #Puolueiden aanimaarat ja valittujen lukumaara kunnittain (pienet puolueet), hylatyt liput seka ennakkoaanestaneet kunnallisvaaleissa 2008
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/700_kvaa_2008_2009-11-02_tau_146_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))

    kaava <- as.formula("Vaalipiiri.ja.kunta~Aanestystiedot.ja.puolueiden.kannatus~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Aanimaara"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Valittujen lukumaara"]

    colnames(tab1) <- paste(colnames(tmp[,,"Aanimaara"]), "(Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valittujen lukumaara"]), "(Valittujen lukumaara)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 puolueiden kannatus: ", colnames(tab))

  } else if (which == "voting.stats") {

    # Aanioikeutetut ja aanestaneet sukupuolen mukaan, hyvaksytyt aanestysliput, valtuutetuiksi valitut ja ennakkoaanet puolueittain seka 
    # hylattyjen aanestyslippujen lukumaara kunnittain kunnallisvaaleissa 2008
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/710_kvaa_2008_2009-11-02_tau_147_fi.px"

    px <- sorvi::read.px(url, na.strings='"-"')

    df <- as.data.frame(px)
    names(df) <- korvaa.skandit(names(df))    

    kaava <- as.formula("Vaalipiiri.ja.kunta~Aanestystiedot.ja.puolueiden.kannatus~Lukumaaratiedot")
    tmp <- reshape::cast(df, kaava, value="dat")
    dimnames(tmp) <- lapply(dimnames(tmp), korvaa.skandit)

    tab1 <- tmp[,,"Lukumaara / Aanimaara"]
    tab2 <- tmp[,,"Osuus aanista"]
    tab3 <- tmp[,,"Valitut"]
    tab4 <- tmp[,,"Osuus valituista"]
    tab5 <- tmp[,,"Ennakkoaanet"]
    tab6 <- tmp[,,"Ennakkoaanten osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Lukumaara / Aanimaara"]), "(Lukumaara / Aanimaara)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus aanista"]), "(Osuus aanista)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valitut"]), "(Valitut)")
    colnames(tab4) <- paste(colnames(tmp[,,"Osuus valituista"]), "(Osuus valituista)")
    colnames(tab5) <- paste(colnames(tmp[,,"Ennakkoaanet"]), "(Ennakkoaanet)")
    colnames(tab6) <- paste(colnames(tmp[,,"Ennakkoaanten osuus"]), "(Ennakkoaanten osuus)")

    tab <- cbind(tab1, tab2, tab3, tab4, tab5, tab6)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niista Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 muuta: ", colnames(tab))

  } else if (which == "previous.experience") {

    # Valituiksi tulleiden aikaisempi kokemus valtuustossa kuntatyypin, sukupuolen ja puolueen mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/730_kvaa_2008_2009-12-30_tau_149_fi.px"
    #px <- sorvi::read.px(url, na.strings='"-"')
    #df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "rejected") {

    # Hylatyt aanestysliput hylkaysperusteen ja vaalipiirin mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/740_kvaa_2008_2009-12-30_tau_150_fi.px"
    px <- sorvi::read.px(url, na.strings='"-"')
    df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "results") {

    #Kunnallisvaalit 2008, tulosanalyysi
    #http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_06/2008_06_fi.asp
    
    warning("No municipality level data available. TODO.")
    # NOTE: vaalipiiri level available
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_06/810_kvaa_2008_2008-10-27_tau_150_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    tab <- NULL

  } else if (which == "pre") {

    #Ennakkoon aanestaneet aanestyspaikan ja vaalipiirin mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/750_kvaa_2008_2009-12-30_tau_151_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad") {

    #Suomen ulkomaan edustustoissa ja laivoissa aanestaneet sukupuolen mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/760_kvaa_2008_2009-12-30_tau_152_fi.px"
    #kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad2") {

    #Aanioikeutetut ja aanestaneet ulkomaalaiset vaalipiirin mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/770_kvaa_2008_2009-12-30_tau_153_fi.px"
    kvaa <- as.data.frame(sorvi::read.px(url, na.strings='"-"'))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "all.municipal") {

    tab1 <- GetMunicipalElectionData2008("voting.stats")
    tab2 <- GetMunicipalElectionData2008("party.votes")
    tab3 <- GetMunicipalElectionData2008("parties.change")
    tab4 <- GetMunicipalElectionData2008("selected.candidates.count")
    # tab5 <- GetMunicipalElectionData2008("woman.candidates") # Error
    tab6 <- GetMunicipalElectionData2008("election.statistics")

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,],
    	         tab4[regs,], tab6[regs,])

  }

  tab

}



#' Load data sets from datavaalit.fi web service
#'
#' @param data.id Data set ID
#'
#' @return rjson object
#' 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export

ReadDatavaalit <- function (data.id) {

  # Read election info
  if (data.id ==  "election.data") { 

    f <- "http://beta.datavaalit.fi/api/v1/election/?format=json&limit=500"
    dat <- fromJSON(paste(readLines(f), collapse = ""))
    
  } else if (data.id == "municipality.data") {

    f <- "http://beta.datavaalit.fi/api/v1/municipality/?format=json&limit=500"
    dat <- fromJSON(paste(readLines(f), collapse = ""))

  } else if (data.id == "hel.council.members") {
    f <- "http://beta.datavaalit.fi/api/v1/council_member/?format=json&limit=85"
    # FIXME: Extremely bad idea to have the function to return different value
    # types depending on the data.id
    dat <- new("council", f)
  }
  dat  
}


#' Description:
#' Function for reading in Finnish Municipal Election candidate data published
#' by Ministry of justice. As of 27-09-2012, the data and descriptions are
#' available from http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
#'
#' Candidate data comes in divided into 14 Election districts (vaalipiiri).
#'
#' @param district.id integer marking the election district ID. Options: [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Joona Lehtomaki \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadCandidates <- function(district.id, cache=NA) {

  ReadElectionData("candidates", district.id, cache)
  
}

#' Description:
#' Wrapper function for ReadCandidates that gets all 14 districts and returns
#' all data in a single data frame.
#'
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Joona Lehtomaki \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadAllCandidates <- function(cache=NA) {
  
  election.district.ids  <- 1:15
  # Remember, there is no id 5!
  election.district.ids  <- election.district.ids[-c(5)]

  # Determine the cache dir if needed
  # cache = "."  
  all.districts <- lapply(election.district.ids, 
                          function(x) {ReadCandidates(x, cache)})
  
  # Bind everything into a single data frame
  candidates <- do.call("rbind", all.districts)

  candidates$RowIndex <- 1:nrow(candidates)
  
  return(candidates)
}

#' Description:
#' Wrapper function for ReadParties that gets all 14 districts and returns
#' all data in a single data frame.
#'
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadAllParties <- function(cache=NA) {
 
  # District 5 does not exist!
  election.district.ids  <- setdiff(1:15, 5)

  # Determine the cache dir if needed
  # cache = "."  
  all.districts <- lapply(election.district.ids, 
                          function(x) {ReadParties(x, cache)})
  
  # Bind everything into a single data frame
  parties <- do.call("rbind", all.districts)

  parties$RowIndex <- 1:nrow(parties)
  
  return(parties)
}


# Private functions -------------------------------------------------------

.readCommonData <- function() {

  .InstallMarginal("rjson")

  data.file <- system.file("extdata/common_data.json", package = "sorvi")
  return(fromJSON(paste(readLines(data.file), collapse = "")))
} 

.datavaalit.idconversions <- function (ids = NULL, type = "election.id") {

  if (type == "election.id") {

    conversion.table <- rbind(c("pv", "presidentin vaali"),
	                      c("e", "eduskuntavaalit"),
                  		  c("k", "kunnallisvaalit"),
                  		  c("epv", "europarlamenttivaalit"),
                  		  c("mkv", "aluevaali"),
                  		  c("vka", "kansanaanestys"))
    colnames(conversion.table) <- c("id", "name")
    conversion.table <- as.data.frame(conversion.table)
  } else if (type == "stage.id") {

    conversion.table <- rbind(c("a", "alustava"),
	                   c("t", "tarkastus"))
    colnames(conversion.table) <- c("id", "name")
    conversion.table <- as.data.frame(conversion.table)
  } else if (type == "data.id") {

    conversion.table <- rbind(c("a", "alue"),
             	      c("e", "ehdokas"),
             	      c("p", "puolue"),
             	      c("k", "kansanaanestys"))
    colnames(conversion.table) <- c("id", "name")
    conversion.table <- as.data.frame(conversion.table)	      
  } else if (type == "info.id") {

    conversion.table <- rbind(c("a", "aanestysaluetaso"),
        	          c("t", "tilastotiedot"),
        	          c("y", "ei.aanestysaluetasoa"),
        	          c("", ""))
    colnames(conversion.table) <- c("id", "name")
    conversion.table <- as.data.frame(conversion.table)	      
  } else if (type == "election.district.id") {

    conversion.table <- rbind(c(1, "Helsingin vaalipiiri"),
    		     c("2", "Uudenmaan vaalipiiri"),
		     c("3", "Varsinais-Suomen vaalipiiri"),
		     c("4", "Satakunnan vaalipiiri"),
		     # 5 is intentionally missing here
		     c("6", "Hameen vaalipiiri"),
		     c("7", "Pirkanmaan vaalipiiri"),
		     c("8", "Kymen vaalipiiri"),
		     c("9", "Etela-Savon vaalipiiri"),
		     c("10", "Pohjois-Savon vaalipiiri"),
		     c("11", "Pohjois-Karjalan vaalipiiri"),
		     c("12", "Vaasan vaalipiiri"),
		     c("13", "Keski-Suomen vaalipiiri"),
		     c("14", "Oulun vaalipiiri"),
		     c("15", "Lapin vaalipiiri"),
		     c("16", "Koko maa"),
		     c("maa", "Koko maa"))

    colnames(conversion.table) <- c("id", "name")
    conversion.table <- as.data.frame(conversion.table)	      

  }

  if (is.null(ids)) {
    return(conversion.table)
  }

  ids <- as.character(ids)

  if (any(ids %in% conversion.table$id)) {
    as.character(conversion.table$name[match(ids, conversion.table$id)])
  } else if (any(ids %in% conversion.table$name)) {
    as.character(conversion.table$id[match(ids, conversion.table$name)])
  }

}

# ---------------------------------------------------------------


#' Description:
#' Function for reading in Finnish Municipal Election political party data 
#  published by Ministry of justice. As of 27-09-2012, the data and 
#  descriptions are
#' available from http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
#  
#' Party data comes divided into 14 Election districts (vaalipiiri).
#'
#' @param district.id integer marking the election district ID. Options: [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadParties <- function(district.id, cache=NA) {
  
  ReadElectionData("parties", district.id, cache)

}


#' Description:
#' Function for reading in Finnish Municipal Election data 
#  published by Ministry of justice. As of 27-09-2012, the data and 
#  descriptions are
#' available from http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
#  
#' Data comes divided into 14 Election districts (vaalipiiri).
#' @param which.data Options: "candidates", "parties"
#' @param district.id integer marking the election district ID. Options: [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadElectionData <- function(which.data, district.id, cache=NA) {
 
  #  which.data <- "parties";  district.id <- 1;  cache <- NA

  # Body of the filename is always the same
  if (which.data == "parties") { 
    file.name.body <- "puo_"
  } else if (which.data == "candidates") { 
    file.name.body <- "ehd_"
  }

  # Convert plain names into numerical IDs if needed
  convtab <- .datavaalit.idconversions(type = "election.district.id")
  if (district.id %in% convtab$name) {
    district.id <- .datavaalit.idconversions(district.id, type = "election.district.id")
  }

  # Coerce the district id into a character for building file paths / urls
  district.id.char <- as.character(district.id)

  # Padding with leading zeros if needed
  if (nchar(district.id.char) == 1) {
    district.id.char <- paste("0", district.id.char, sep="")
  }
  
  # Construct the file name
  file.name <- paste(file.name.body, district.id.char, ".csv", sep="")
  
  # Either use the cached files or fetch over network
  if (is.na(cache)) {
                          
    data.source <- paste("http://192.49.229.35/K2012/s/ehd_listat/",
                          file.name, sep="")

    message(paste("Reading data from URL", data.source))
    
  } else {
    
    if (file.exists(cache)) {
      data.source <- file.path(cache, file.name)
      
      # Check if the actual file exists
      if (!file.exists(data.source)) {
        stop(paste("File", data.source, "does not exist."))
      } else {
        message(paste("Using cached version", data.source))
      }
      
    } else {
      stop("Cache requested, but not found")
    }
    
    # Read the table over network, use the encodign provided by MoJ
  }
  # Read the data from selected data source
  raw.data <- read.csv(data.source, sep=";", as.is=TRUE, strip.white=TRUE, fileEncoding="iso-8859-1")
  
  # The the suitable header names from common_data.json
  header <- .readCommonData()
  
  # In the original csv file, there is also a trailing ";" -> there really is
  # only 29 / 35 columns (as of 27.9.2012); more columns will appear 
  # on the election day
  if (which.data == "parties") {
    raw.data <- raw.data[1:35]
    header <- header$OMpuolueet$header
  } else if (which.data == "candidates") {
    raw.data <- raw.data[1:29]
    header <- header$OMehdokkaat$header
  }

  # Set the header
  colnames(raw.data) <- header[1:length(raw.data)]
  
  # Column pre-processing

  dat <- .preprocessElectionData(raw.data, which.data)

  dat <- cbind(as.character(1:nrow(dat)), dat)  
  colnames(dat) <- c("RowIndex", colnames(dat)[-1])
  rownames(dat) <- NULL

  return(dat)
  
}




#' Description:
#' Internal function for election data preprocessing
#'  
#' @param dat election data frame
#' @param which.data "elections" or "candidates"
#' @return Data frame
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

.preprocessElectionData <- function (dat, which.data) {

  dat <- as.data.frame(dat)

  # Get conversions between municipality IDs and names from MML data
  # (C) MML 2011-2012
  # FIXME: replace with GetVaalipiiri data and Kuntanumero from another server.

  dat$Vaalilaji_nimi_fi <- .datavaalit.idconversions(tolower(dat$Vaalilaji), type = "election.id") 
  dat$Vaalipiiri_fi <- .datavaalit.idconversions(as.character(dat$Vaalipiirinumero), type = "election.district.id") 

  dat$Kuntanumero[nchar(dat$Kuntanumero) == 1] <- paste("00", dat$Kuntanumero[nchar(dat$Kuntanumero) == 1], sep = "")
  dat$Kuntanumero[nchar(dat$Kuntanumero) == 2] <- paste("0", dat$Kuntanumero[nchar(dat$Kuntanumero) == 2], sep = "")
  dat$Kunta <- ConvertMunicipalityCodes(ids = dat$Kuntanumero)
  dat$Kommun <- as.character(dat$Alueen_nimi_sv)

  dat$Puolue_lyhenne_fi <- dat$Nimilyhenne_fi
  dat$Puolue_lyhenne_sv <- dat$Nimilyhenne_sv
  
  dat$Nimilyhenne_fi <- NULL
  dat$Nimilyhenne_sv <- NULL

  if (which.data == "candidates") {
    
    dat$Sukupuoli <- factor(dat$Sukupuoli, labels=c("Mies", "Nainen"))
    dat$Ehdokas <- paste(dat$Sukunimi, " ", dat$Etunimi, " / ",  dat$Puolue_lyhenne_fi, " / ", dat$Kunta, sep = "")

  } 

  dat

}



