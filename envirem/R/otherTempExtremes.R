##' @title Temperature Extremes
##'
##' @description Generates max temp of the coldest month, min temp of the warmest month, mean temp
##'of the coldest month, mean temp of the warmest month.
##'
##' @param meantempStack rasterStack of monthly mean temperature
##'
##' @param mintempStack rasterStack of monthly min temperature
##'
##' @param maxtempStack rasterStack of monthly max temperature
##'
##'
##' @return rasterStack of maxTempColdest, minTempWarmest, meanTempColdest, meanTempWarmest, in 
##' same units as input rasters.
##'
##' @author Pascal Title
##'
##' @examples
##' \dontrun{
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
##' otherTempExtremes(tmean, tmin, tmax)
##' }
##' @export


#function returns a stack of max temp of coldest month and min temp of warmest month
otherTempExtremes <- function(meantempStack, mintempStack, maxtempStack) {
	
	# enforce ordering
	meantempStack <- meantempStack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(meantempStack))))]]
	mintempStack <- mintempStack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(mintempStack))))]]
	maxtempStack <- maxtempStack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(maxtempStack))))]]
	
	# Max temp of coldest month
	##identify coldest month by mean temp
	## get index of coldest month by cell	
	coldestMonth <- which.min(meantempStack)
	maxTempColdest <- raster::raster(coldestMonth)
	#get max temp of this month
	# we will loop through 1:12, and identify which cells have this value, then extract temp for those cells
	for (i in 1:12) {
		cells <- which(raster::values(coldestMonth) == i)
		maxTempColdest[cells] <- maxtempStack[[i]][cells]
	}
	names(maxTempColdest) <- 'maxTempColdest'

	#Min temp of warmest month
	##identify warmest month by mean temp
	## get index of coldest month by cell	
	warmestMonth <- which.max(meantempStack)
	minTempWarmest <- raster::raster(warmestMonth)
	#get max temp of this month
	# we will loop through 1:12, and identify which cells have this value, then extract temp for those cells
	for (i in 1:12) {
		cells <- which(raster::values(warmestMonth) == i)
		minTempWarmest[cells] <- mintempStack[[i]][cells]
	}
	names(minTempWarmest) <- 'minTempWarmest'
	
	# mean temp of coldest month
	meanTempColdest <- min(meantempStack)
	names(meanTempColdest) <- 'meanTempColdest'

	# mean temp of warmest month
	meanTempWarmest <- max(meantempStack)
	names(meanTempWarmest) <- 'meanTempWarmest'	

	return(raster::stack(list(maxTempColdest, minTempWarmest, meanTempColdest, meanTempWarmest)))
}
