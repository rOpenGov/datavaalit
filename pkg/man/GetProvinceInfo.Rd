\name{GetProvinceInfo}
\alias{GetProvinceInfo}
\alias{get.province.info}
\title{Get information of Finnish provinces.}
\usage{
  GetProvinceInfo(...)
}
\arguments{
  \item{...}{Arguments to be passed}
}
\value{
  A data frame. With the following entries: Maakunta:
  province; Pinta-ala: area; Vakiluku: population;
  Vaestotiheys: population density
}
\description{
  Get information of Finnish provinces.
}
\examples{
# tab <- GetProvinceInfo()
}
\author{
  Leo Lahti \email{louhos@googlegroups.com}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

