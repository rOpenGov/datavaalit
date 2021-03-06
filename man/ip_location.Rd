\name{ip_location}
\alias{ip_location}
\title{Get geographic coordinates for a given IP-address from
http://www.datasciencetoolkit.org//ip2coordinates/}
\usage{
  ip_location(ip)
}
\arguments{
  \item{ip}{IP address as character}
}
\value{
  Latitude and longitude as a numeric vector
}
\description{
  Get geographic coordinates for a given IP-address from
  http://www.datasciencetoolkit.org//ip2coordinates/
}
\note{
  Modified from original version by Kay Cichini
}
\examples{
# ip_location("137.224.252.10")
}
\author{
  Leo Lahti \email{louhos@googlegroups.com}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

