\name{GetElectedCandidates}
\alias{GetElectedCandidates}
\title{GetElectedCandidates}
\usage{
  GetElectedCandidates(year, election, election.district,
    verbose = TRUE)
}
\arguments{
  \item{year}{election year}

  \item{election}{election type (municipal / parliament /
  president / ...)}

  \item{election.district}{election.district in numeric or
  character format (for instance: 2 or "Uudenmaan
  vaalipiiri")}

  \item{verbose}{verbose}
}
\value{
  data.frame
}
\description{
  Get data on elected candidates
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

