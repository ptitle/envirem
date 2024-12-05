##' @title Continentality
##'
##' @description Generate Continentality index.
##'
##' @param tmax rasterLayer of average temperature of the warmest month
##'
##' @param tmin rasterLayer of average temperature of the coldest month
##'
##'	@param tempScale integer; scaling factor for the temperature data, see \link{envirem} for 
##' 	additional details. 
##'
##' @details \code{continentality index = tmax - tmin}
##'
##' @return rasterLayer in units of degrees C.
##'
##' @references
##' Rivas-Martínez, S. & Rivas-Sáenz, S. “Synoptical Worldwide Bioclimatic 
##' Classification System”. [accessed 15 February 2016]
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
##' \donttest{
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' env <- rast(rasterFiles)
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
##' continentality(meantempWarmest, meantempColdest, tempScale = 10)
##' }
##' @export

# continentality index
## mean temperature of warmest month - mean temperature of coldest month

continentality <- function(tmax, tmin, tempScale = 1) {
	res <- tmax - tmin
	res <- res / tempScale
	names(res) <- 'continentality'
	return(res)
}


# # # calculation according to Conrad 1946
# continentality <- function(tmax, tmin, tempScale = 1, conrad = FALSE) {
	# res <- tmax - tmin
	# res <- res / tempScale
	
	# if (conrad) {
		# lats <- terra::crds(res, df = TRUE, na.rm = FALSE)
		# lats <- terra::vect(lats, geom = colnames(lats), crs = terra::crs(res))
		# if (!terra::is.lonlat(res)) {
			# lats <- terra::project(lats, 'EPSG:4326')
		# }
		# lats <- abs(terra::crds(lats)[,2])
		# lats <- sin(lats + 10)
		
		# res <- ((1.7 * as.numeric(terra::values(res))) / lats) - 14
		# continentality <- tmax[[1]]
		# terra::values(continentality) <- res

		# names(continentality) <- 'continentality'
		# return(continentality)

	# } else {
		# names(res) <- 'continentality'
		# return(res)		
	# }
# }



