\name{ReadParties}
\alias{ReadParties}
\title{Description:
Function for reading in Finnish Municipal Election political party data
available from http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
Party data comes divided into 14 Election districts (vaalipiiri).}
\usage{
  ReadParties(district.id, cache = NA)
}
\arguments{
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
  Election political party data available from
  http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
  Party data comes divided into 14 Election districts
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

