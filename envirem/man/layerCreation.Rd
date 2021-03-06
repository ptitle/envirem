% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layerCreation.R
\name{layerCreation}
\alias{layerCreation}
\title{Creates all layers}
\usage{
layerCreation(
  masterstack,
  solradstack = NULL,
  var,
  tempScale = 1,
  precipScale = 1
)
}
\arguments{
\item{masterstack}{rasterStack containing all monthly precipitation, 
min temperature, max temperature, and optionally mean temperature rasters.}

\item{solradstack}{rasterStack of monthly solar radiation, can be \code{NULL} if not needed.}

\item{var}{vector of names of variables to generate, see Details.}

\item{tempScale}{integer; scaling factor for the temperature data, see \link{envirem} for 
additional details.}

\item{precipScale}{integer; scaling factor for the precipitation data, see \link{envirem}
for additional details.}
}
\value{
rasterStack
}
\description{
Generates all rasterLayers for one particular input dataset.
For the distinction between this function and \code{\link{generateRasters}}, 
see \code{Details}.
}
\details{
The function \code{\link{verifyFileStructure}} should be used to 
verify that the appropriate rasters are present in \code{masterstack}.

This function is called internally by \code{\link{generateRasters}}. 

The function \code{layerCreation} will generate envirem rasters from input R 
objects (rasterStacks) and will return the result as an R object. In contrast, 
the function \code{\link{generateRasters}} reads in input rasters from a specified directory, 
splits input rasters into tiles if necessary, internally calls 
\code{layerCreation} and writes the result to file. 

Possible variables to generate include:\cr
\cr
annualPET \cr
aridityIndexThornthwaite \cr
climaticMoistureIndex \cr
continentality \cr
embergerQ \cr
growingDegDays0 \cr
growingDegDays5 \cr
maxTempColdest \cr
minTempWarmest \cr
meanTempColdest \cr
meanTempWarmest \cr
monthCountByTemp10 \cr
PETColdestQuarter \cr
PETDriestQuarter \cr
PETseasonality \cr
PETWarmestQuarter \cr
PETWettestQuarter \cr
thermicityIndex \cr

If \code{var = 'all'}, then all of the variables will be generated.
}
\examples{
\donttest{
# Find example rasters
rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)

# create stack of temperature and precipitation rasters
# and stack of solar radiation rasters
solradFiles <- grep('solrad', rasterFiles, value=TRUE)
worldclim <- stack(setdiff(rasterFiles, solradFiles))
solar <- stack(solradFiles)

# set up naming scheme - only precip is different from default
assignNames(precip = 'prec_##')

# generate all possible envirem variables
layerCreation(worldclim, solar, var='all', tempScale = 10)

# set back to defaults
assignNames(reset = TRUE)
}
}
\seealso{
This function is called internally by \code{\link{generateRasters}}.
}
\author{
Pascal Title
}
