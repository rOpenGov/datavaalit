\name{GetEdustajaData}
\alias{GetEdustajaData}
\title{Hakee tietyn aanestyksen tulokset edustajaittain}
\usage{
  GetEdustajaData(aanestys)
}
\arguments{
  \item{aanestys}{aanestyksen ID (Aanestysten tunnisteet
  ovat muotoa a{aanestyksen numero}_{istunnon
  numero}-{vuosi}), esim. "a3_80-2011"}
}
\value{
  data.frame jossa valinta, puolue ja nimi
}
\description{
  Hakee tietyn aanestyksen tulokset edustajaittain
}
\examples{
# edustajat <- GetEdustajaData('a3_80-2011')
}
\author{
  Juuso Haapanen, Leo Lahti \email{louhos@googlegroups.com}
}
\references{
  See citation("sorvi")
}
\keyword{eduskunta}

