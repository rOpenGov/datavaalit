# To call in the statistician after the experiment is done may be no more
# than asking him to perform a post-mortem examination: he may be able to
# say what the experiment died of. ~ Sir Ronald Aylmer Fisher

# This file is a part of the sorvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2013 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


#' LouhosStoragePath
#'
#' Arguments:
#'   ... Arguments to pass
#'
#' Return:
#' @return URL for Louhos data
#'
#' @examples # url <- LouhosStoragePath()
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @keywords utilities
LouhosStoragePath <- function () {
  # Louhos data is stored in Github avoindata repo: 
  # https://github.com/avoindata/ which is 
  # mirrored on Datavaalit server
  "http://www.datavaalit.fi/storage/avoindata/"
}

#' LoadData
#'
#' Arguments:
#' @param data.id data ID to download (suffix before .rda). Investigate the contents of the url path to check data.ids
#' @param verbose verbose 
#'
#' Return:
#' @return translations 
#'
#' @examples # translations <- LoadData("translations")
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @keywords utilities

LoadData <- function(data.id, verbose = TRUE) {

  # Circumvent warnings
  fi.en.maakunnat <- NULL
  kuntarajat.maa.shp <- NULL

  url <- LouhosStoragePath()
  filepath <- paste(url, "/louhos/", data.id, ".rda", sep = "")
  if (verbose) {message(paste("Loading ", filepath, sep = ""))}
  #load(url(filepath), envir = .GlobalEnv)  
  load(url(filepath))  
  #print(kuntarajat.maa.shp)
  if (data.id == "translations") {return(fi.en.maakunnat)}
  if (data.id == "kuntarajat.maa.shp") {return(kuntarajat.maa.shp)}

}

#' translations data documentation 
#'
#' This data set contains translations for common terms for which
#' primary data is available in a non-Finnish language
#' Contents: fi.en.maakunnat: Province names Finnish-English
#'
#' @name translations
#' @docType data
#' @author Leo Lahti \email{louhos@@googlegroups.com} 
#' @references See cite(sorvi)
#' @usage LoadData("translations")
#' @format list
#' @keywords data misc
NULL


#' Replace special characters with standard ones.
#'
#' @param s string from which the special chars should be removed
#' @return string with special chars replaced by standard ones
#' @export
#' @note iconv function provides better tools for these purposes and is now the main tool
#' This function is kept for compatibility with the older versions.
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # korvaa.skandit("my.string.here") # if no, special chars, the same string is returned
#' @keywords utilities

korvaa.skandit <- function (s) {
 
  sclass <- class(s)

  s <- gsub("\\xe4", "a", s)
  s <- gsub("\\xC4", "A", s)
  s <- gsub("\\xD6", "O", s)
  s <- gsub("\\xf6", "o", s)
  s <- gsub("\\xE5", "a", s)
  s <- gsub("\\U3e34633c", "A", s)

  # Return a factor if the original input vector was a factor
  if (sclass == "factor") {
    s <- factor(s)
  } 

  s

}

#' Check if the given object is an url string
#'
#' Arguments:
#'  @param s input object to check
#'
#' Returns:
#'  @return TRUE/FALSE indicating whether the input string is a valid URL.
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # is.url("http://aa.px")
#' @keywords utilities
is.url <- function (s) {
  (class(s) == "character" && substr(s,1,7) == "http://")
}




#' Remove spaces from a string (single string or vector/list of strings).
#'
#' @param s string or vector/list of strings
#' @return string without spaces
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # strstrip("a b") # returns "ab"
#' @keywords utilities


strstrip <- function (s) {

  if (length(s) == 1) {
    stripped <- strstrip.single(s)
  } else {
    stripped <- sapply(s, strstrip.single)
  }

  stripped
}


#' Remove spaces from a single string
#'
#' @param s string
#' @return string without spaces
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # strstrip.single("a b") # returns "ab"
#' @keywords utilities

strstrip.single <- function (s) {

  # Remove spaces from a string

  # (C) Leo Lahti 2008-2011
  # FreeBSD license (keep this notice)

  # Strip string i.e. remove spaces from the beginning and end
  while (substr(s,1,1)==" ") {
    s <- substr(s,2,nchar(s))
  }
  while (substr(s,nchar(s),nchar(s))==" ") {
    s <- substr(s,1,nchar(s)-1)
  }
  s
}

#' Strip string i.e. remove spaces from the beginning and end
#' @param s string or character vector
#'
#' @return Stripped string
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples 
#' #s2 <- Strip(s) 
#' @keywords utilities
Strip <- function (s) {

  ss <- c()
  for (i in 1:length(s)) {
    si <- s[[i]]

    # Strip string i.e. remove spaces from the beginning and end
    while (substr(si,1,1)==" ") {
      si <- substr(si, 2, nchar(si))
    }
    while (substr(si, nchar(si), nchar(si))==" ") {
      si <- substr(si, 1, nchar(si) - 1)
    }
    ss[[i]] <- si
  }
  ss
}


#' Install marginal dependencies on-demand from other functions.
#' 
#' Function is always supposed to be called from a parent function and if the
#' marginal depedency package is not installed, the function will report the 
#' name of the parent function requiring the package. Note that the whole call
#' stack is not reported, only the immediate parent.
#'
#' @param package String name for the name to be installed
#' @param ... further arguments passed on to install.packages
#'
#' @return Invisible NULL
#' 
#' @note meant for package internal use only
#' @export
#' @author Joona Lehtomaki \email{louhos@@googlegroups.com}
#' @keywords utilities

.InstallMarginal <- function(package, ...) {
  if (suppressWarnings(!require(package, character.only=TRUE, quietly=TRUE))) { 
    parent.function <- sys.calls()[[1]][1]
    message(paste("Function ", parent.function, " requires package: ", package,
                  ". Package not found, installing...", sep=""))
    install.packages(package, ...) # Install the packages
    require(package, character.only=TRUE) # Remember to load the library after installation
  }
}


#' ReadASC: read ASC file
#' Routines for 3D landscape visualization
#' contributed by Janne Aukia 2013
#'
#' Arguments:
#'   @param filename Input file name
#'
#' Returns:
#'   @return data matrix
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Janne Aukia. Contact: \email{louhos@@googlegroups.com}
#' @seealso ReadXYZ
#' @examples # 
#' @keywords utilities

ReadASC <- function (filename) {

  dat   <- read.table(filename, sep = " ", skip = 6)
  mdat  <- data.matrix(dat)
  tmdat <- t(mdat)[,nrow(mdat):1]
  return(tmdat)

}


#' ReadXYZ: read XYZ coordinate file
#' Routines for 3D landscape visualization
#' contributed by Janne Aukia 2013
#'
#' Arguments:
#'   @param filename Input file name
#'
#' Returns:
#'   @return data matrix
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Janne Aukia. Contact: \email{louhos@@googlegroups.com}
#' @seealso ReadASC
#' @examples # 
#' @keywords utilities

ReadXYZ <- function(filename) {

  .InstallMarginal("Matrix")

  dat <- read.table(filename)
  xp <- (dat$V1-min(dat$V1))/10
  yp <- (dat$V2-min(dat$V2))/10

  mat <- data.matrix(spMatrix(max(xp)+1,max(yp)+1,xp+1,yp+1,dat$V3))  
  return(mat)

}


