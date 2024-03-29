% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growingDegDays.R
\name{growingDegDays}
\alias{growingDegDays}
\title{Growing degree days}
\usage{
growingDegDays(meantempstack, baseTemp, tempScale = 1)
}
\arguments{
\item{meantempstack}{SpatRaster of mean monthly temperature in deg C}

\item{baseTemp}{base temperature in degrees C.}

\item{tempScale}{integer; scaling factor for the temperature data, see \link{envirem} for 
additional details.}
}
\value{
rasterLayer in degrees C * days.
}
\description{
Growing degree days above some base temperature.
}
\details{
growing degree days = sum of all monthly temps greater than baseTemp, 
multiplied by total number of days
}
\examples{
# Find example rasters
rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
env <- rast(rasterFiles)

meantemp <- env[[grep('tmean', names(env), value=TRUE)]]
growingDegDays(meantemp, 10, tempScale = 10)
}
\references{
Prentice, I.C., Cramer, W., Harrison, S.P., Leemans, R., Monserud, R.A. & Solomon, A.M.
(1992). A Global Biome Model Based on Plant Physiology and Dominance, Soil Properties
 and Climate. \emph{Journal of Biogeography}, \strong{19}, 117–134.
}
\author{
Pascal Title
}
