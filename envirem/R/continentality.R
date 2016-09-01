##' @title Continentality
##'
##' @description Generate Continentality index.
##'
##' @param tmax rasterLayer of average temperature of the warmest month

##' @param tmin rasterLayer of average temperature of the coldest month
##'
##' @details \code{continentality index = tmax - tmin}
##'
##' @return rasterLayer in units of degrees C.
##'
##' @references
##' Rivas-Martínez, S. & Rivas-Sáenz, S. “Synoptical Worldwide Bioclimatic 
##' Classification System”.  Available online at \url{http://www.globalbioclimatics.org/}
##' [accessed 15 February 2016]
##' 
##' Sayre, R., Comer, P., Warner, H. & Cress, J. (2009) \emph{A new map of standardized
##' terrestrial ecosystems of the conterminous United States: US Geological Survey 
##' Professional Paper 1768}. Reston, VA. 
##'
##' @author Pascal Title
##'
##' @seealso \link{thermicityIndex}
##'
##' @examples
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' env <- stack(rasterFiles)
##'
##' # identify appropriate layers
##' tmean <- grep('tmean', names(env))
##' tmin <- grep('tmin', names(env))
##' tmax <- grep('tmax', names(env))
##'
##' tmean <- env[[tmean]]
##' tmin <- env[[tmin]]
##' tmax <- env[[tmax]]
##' 
##' # calculate temperature extremes
##' temp <- otherTempExtremes(tmean, tmin, tmax)
##'
##' meantempWarmest <- temp[['meanTempWarmest']]
##' meantempColdest <- temp[['meanTempColdest']]
##'
##' continentality(meantempWarmest, meantempColdest)
##' @export

# continentality index
## mean temperature of warmest month - mean temperature of coldest month
## assuming temp variables are degrees C * 10

continentality <- function(tmax, tmin) {
	res <- tmax / 10 - tmin / 10
	names(res) <- 'continentality'
	return(res)
}