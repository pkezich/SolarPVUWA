\name{SolarPVUWA}
\docType{package}
\title{
SolarPVUWA
}
\description{
Generates Estimates for Solar PV for Postcode in Western Australia
}
\details{
\tabular{ll}{
Package: \tab SolarPVUWA\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-19\cr
License: \tab GNU-GPL 3\cr
}
This package generates an estimate for the PV output of Western Australian Postcodes by first creating models for irradiation and ambient temperature and then passing them through the model for a solar PV installation obtained from the SolaR package. The central function of the package calculate_postcode_pv() produces a data fram containing the mean and upper and lower bounds of the 90th percentile for PV output.
}
\author{
Dr. Grant Coble-Neal\cr
Baopeng Hu\cr
Philip Kezich\cr
Melissa Liu\cr
Joseph John Penipe\cr
Simon Truong\cr
Sheng Yang\cr

Maintainer: Philip Kezich <philip.kezich@gmail.com>
}
\references{
solaR Documentation <http://cran.r-project.org/web/packages/solaR/solaR.pdf>

Temperature Interpolation Paper <http://www.dtic.mil/dtic/tr/fulltext/u2/a240394.pdf>
}
\keyword{ Solar PV, Irradiation Estimation, Renewable Energy }
\examples{
calculate_postcode_pv('C:/Model Data','C:/Processed Data','6000','05-17')
}
