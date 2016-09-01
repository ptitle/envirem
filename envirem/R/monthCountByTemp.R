##' @title Month count by temperature
##'
##' @description Number of months with mean temperature greater than some base temp.
##'
##' @param tempStack rasterStack of monthly mean temperature in degrees C * 10

##' @param minTemp reference temperature in degrees C
##'
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
##' monthCountByTemp(meantemp, 10)
##' @export


#Number of months with a mean temp > T deg
# needed = monthly mean temp
monthCountByTemp <- function(tempStack, minTemp = 10) {
	
	tempStack <- tempStack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(tempStack))))]]
	
	minTemp <- minTemp * 10 #worldclim temps are multiplied by 10
	#create logical rasters, by minTemp
	tempStack <- tempStack > minTemp
	res <- sum(tempStack)
	names(res) <- paste0('monthCountByTemp', minTemp / 10)
	return(res)
}
