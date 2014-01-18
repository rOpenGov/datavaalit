\name{valid.hetu}
\alias{valid.hetu}
\title{Validate a Finnish personal identification number (HETU).}
\usage{
  valid.hetu(hetu)
}
\arguments{
  \item{hetu}{Finnish personal identification number as a
  string.}
}
\value{
  Is the given string a valid Finnish personal
  identification number, TRUE or FALSE.
}
\description{
  Validate a Finnish personal identification number (HETU).
}
\examples{
valid.hetu("010101-0101") # TRUE
valid.hetu("010101-010A") # FALSE
}
\author{
  Jussi Paananen \email{louhos@googlegroups.com}
}
\seealso{
  \code{\link{hetu}} For extracting information from a
  Finnish personal identification number.
}

