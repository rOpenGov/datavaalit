\name{hetu}
\alias{hetu}
\title{Extract information from a Finnish personal identification number (HETU).}
\usage{
  hetu(hetu)
}
\arguments{
  \item{hetu}{Finnish personal identification number as a
  string}
}
\value{
  Finnish personal identification number object. Returns NA
  if the given string is not a valid Finnish personal
  identification number. \item{hetu}{Finnish personal
  identification number as a string.} \item{gender}{Gender
  of the person. Male or Female.}
  \item{personal.number}{Personal number part of the
  identification number.} \item{checksum}{Checksum for the
  personal identification number.} \item{date}{Birthdate.}
  \item{day}{Day of the birthdate.} \item{month}{Month of
  the birthdate.} \item{year}{Year of the birthdate.}
  \item{century.char}{Century of the birthdate: + (1800), -
  (1900) or A (2000). }
}
\description{
  Extract information from a Finnish personal
  identification number (HETU).
}
\examples{
hetu("111111-111C")
hetu("111111-111C")$date
hetu("111111-111C")$gender
}
\author{
  Jussi Paananen \email{louhos@googlegroups.com}
}
\seealso{
  \code{\link{valid.hetu}} For validating Finnish personal
  identification number.
}

