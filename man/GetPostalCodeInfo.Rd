\name{GetPostalCodeInfo}
\alias{GetPostalCodeInfo}
\alias{get.postal.codes}
\title{Get Finnish postal codes vs. municipalities table from Wikipedia.}
\usage{
  GetPostalCodeInfo(...)
}
\arguments{
  \item{...}{Arguments to be passed}
}
\value{
  A data frame with following fields: postal.code: postal
  code; municipality: Name of the municipality (kunnan
  nimi); municipality.scandless: Municpality name without
  special chars
}
\description{
  Get Finnish postal codes vs. municipalities table from
  Wikipedia.
}
\note{
  Previous version of this function was named:
  get.postal.codes
}
\examples{
#postal.code.table <- GetPostalCodeInfo()
}
\author{
  Juuso Parkkinen and Leo Lahti
  \email{louhos@googlegroups.com}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

