\name{GetParliamentVotes}
\alias{GetParliamentVotes}
\title{Hakee kaikki aanestykset eduskuntarajapinnasta}
\usage{
  GetParliamentVotes(...)
}
\arguments{
  \item{...}{parameters to be passed}
}
\value{
  list
}
\description{
  Hakee kaikki aanestykset eduskuntarajapinnasta
}
\examples{
# AAnestysten tunnisteet ovat muotoa a{aanestyksen numero}_{istunnon numero}-{vuosi}
# aanestykset <- GetParliamentaryVotes(...)
}
\author{
  Juuso Haapanen, Leo Lahti \email{louhos@googlegroups.com}
}
\references{
  citation("sorvi")
}
\keyword{eduskunta}

