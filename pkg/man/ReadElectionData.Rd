\name{ReadElectionData}
\alias{ReadElectionData}
\title{Description:
Function for reading in Finnish Municipal Election data
available from http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
Data comes divided into 14 Election districts (vaalipiiri).}
\usage{
  ReadElectionData(which.data, district.id, cache = NA)
}
\arguments{
  \item{which.data}{Options: "candidates", "parties"}

  \item{district.id}{integer marking the election district
  ID. Options: [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14,
  15]}

  \item{cache}{character directory path to location where
  files are cached}
}
\value{
  Data frame
}
\description{
  Description: Function for reading in Finnish Municipal
  Election data available from
  http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
  Data comes divided into 14 Election districts
  (vaalipiiri).
}
\examples{
#
}
\author{
  Leo Lahti \email{louhos@googlegroups.com}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

