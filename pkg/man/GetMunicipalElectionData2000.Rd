\name{GetMunicipalElectionData2000}
\alias{GetMunicipalElectionData2000}
\title{GetMunicipalElectionData2000}
\usage{
  GetMunicipalElectionData2000(which = "election.statistics")
}
\arguments{
  \item{which}{Indicate which of the available Statistics
  Finland data sets to parse. Options: election.statistics,
  candidates, selected.candidates.by.region,
  selected.candidates.all, parties,
  all.municipality.level.data}
}
\value{
  data.frame
}
\description{
  Get municipal election data from Statistics Finland (C)
  2012
  http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/kvaa_2000_fi.asp
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

