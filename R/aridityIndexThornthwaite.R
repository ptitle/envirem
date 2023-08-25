##' @title aridityIndexThornthwaite
##'
##' @description Generates thornthwaite aridity index raster.
##'
##' @param precipStack SpatRaster of monthly precipitation.
##'
##' @param PETstack SpatRaster of monthly potential evapotranspiration. 
##' 	Layer names are assumed to end in the month number. 
##'
##'	@param precipScale integer; scaling factor for the precipitation data, see \link{envirem}
##' 	for additional details. 
##'
##' @details \code{Thornthwaite aridity index = 100d / n}
##'	where d = sum of monthly differences between precipitation and PET for months where precip < PET
##'	where n = sum of monthly PET for those months
##'
##' @return RasterLayer, unitless
##'
##' @author Pascal Title
##' 
##' @references 
##' Thornthwaite, C.W. (1948). An approach toward a rational classification of climate.
##' \emph{Geographical Review}, \strong{38}, 55-94.
##'
##' @seealso Requires rasters created with \code{\link{monthlyPET}}. 
##'
##' @examples
##' \donttest{
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' env <- rast(rasterFiles)
##'
##' # identify the appropriate layers
##' meantemp <- grep('mean', names(env), value=TRUE)
##' solar <- grep('solrad', names(env), value=TRUE)
##' maxtemp <- grep('tmax', names(env), value=TRUE)
##' mintemp <- grep('tmin', names(env), value=TRUE)
##' 
##' # read them in as SpatRasters
##' meantemp <- env[[meantemp]]
##' solar <- env[[solar]]
##' maxtemp <- env[[maxtemp]]
##' mintemp <- env[[mintemp]]
##' tempRange <- abs(maxtemp - mintemp)
##' 
##' # get monthly PET
##' pet <- monthlyPET(meantemp, solar, tempRange)
##'
##' precip <- grep('prec', names(env), value=TRUE)
##' precip <- env[[precip]]
##' 
##' # set up naming scheme - only precip is different from default
##' assignNames(precip = 'prec_##')
##'
##' aridityIndexThornthwaite(precip, pet)
##'
##' # set back to defaults
##' assignNames(reset = TRUE)
##' }
##' 
##' @export


aridityIndexThornthwaite <- function(precipStack, PETstack, precipScale = 1) {
	
	# confirm naming scheme
	if (!all(paste0(.var$precip, 1:12, .var$precip_post) %in% names(precipStack)) & !all(paste0(.var$precip, sprintf("%02d", 1:12), .var$precip_post) %in% names(precipStack))) {
		stop('Defined naming scheme does not match layer names of precipStack.')
	}

	#enforce ordering of stack according to numbers in names
	precipStack <- precipStack[[order(as.numeric(gsub(paste0(.var$precip, '([0-9]+)', .var$precip_post), "\\1", names(precipStack))))]]
	PETstack <- PETstack[[order(as.numeric(gsub(paste0(.var$pet, '([0-9]+)', .var$pet_post), "\\1", names(PETstack))))]]
	
	if (precipScale != 1) {
		precipStack <- precipStack / precipScale
	}

	#create receiving rasters
	d <- precipStack[[1]]
	terra::values(d)[!is.na(terra::values(d))] <- 0
	n <- precipStack[[1]]
	terra::values(n)[!is.na(terra::values(n))] <- 0

	# for each month, determine which cells have precip < PET
	# for those cells, add value of diff to receiver rasters
	# for those cells, add PET to n
	for (i in 1:12) {
		
		isPrecipLess <- precipStack[[i]] < PETstack[[i]]
		cells <- which(terra::values(isPrecipLess) == 0)
		tmp <- PETstack[[i]] - precipStack[[i]]
		tmp[cells] <- 0
		d <- d + tmp

		tmp <- PETstack[[i]]
		terra::values(tmp)[cells] <- 0
		n <- n + tmp
	}

	res <- 100*d
	res <- res / n
	nzeroes <- which(terra::values(n) == 0)
	res[nzeroes] <- 0
	names(res) <- 'aridityIndexThornthwaite'
	return(res)

}
