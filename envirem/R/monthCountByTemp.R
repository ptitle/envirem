##' @title Month count by temperature
##'
##' @description Number of months with mean temperature greater than some base temp.
##'
##' @param tempStack rasterStack of monthly mean temperature in degrees C
##'
##' @param minTemp reference temperature in degrees C
##'
##'	@param tempScale integer; scaling factor for the temperature data, see \link{envirem} for 
##' 	additional details. 
##'
##' @return rasterLayer with values representing counts of months.
##'
##' @references
##' Metzger, M.J., Bunce, R.G.H., Jongman, R.H.G., Sayre, R., Trabucco, A. & Zomer, R. (2013). 
##' A high-resolution bioclimate map of the world: a unifying framework for global 
##' biodiversity research and monitoring. \emph{Global Ecology and Biogeography}, 
##' \strong{22}, 630-638.
##' 
##' @author Pascal Title
##'
##' @examples
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' env <- stack(rasterFiles)
##'
##' # identify the appropriate layers
##' meantemp <- grep('mean', names(env), value=TRUE)
##' meantemp <- env[[meantemp]]
##' monthCountByTemp(meantemp, 10, tempScale = 10)
##' @export


#Number of months with a mean temp > T deg
# needed = monthly mean temp
monthCountByTemp <- function(tempStack, minTemp = 10, tempScale = 1) {
	
	if (!all(grepl(.var$tmean, names(tempStack)))) {
		stop('tempStack does not appear to have the naming scheme defined for tmean. See ?assignNames.')
	}
	
	tempStack <- tempStack[[order(as.numeric(gsub(paste0(.var$tmean, '([0-9]+)', .var$tmean_post), "\\1", names(tempStack))))]]
	
	if (tempScale != 1) {	
		tempStack <- tempStack / tempScale
	}
	
	#create logical rasters, by minTemp
	tempStack <- tempStack > minTemp
	res <- sum(tempStack)
	names(res) <- paste0('monthCountByTemp', minTemp)
	return(res)
}
