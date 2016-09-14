##' @title aridityIndexThornthwaite
##'
##' @description Generates thornthwaite aridity index raster.
##'
##' @param precipStack rasterStack of monthly mean precipitation.
##'
##' @param PETstack rasterStack of monthly potential evapotranspiration.
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
##' \dontrun{
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' env <- stack(rasterFiles)
##'
##' # identify the appropriate layers
##' meantemp <- grep('mean', names(env), value=TRUE)
##' solar <- grep('solrad', names(env), value=TRUE)
##' maxtemp <- grep('tmax', names(env), value=TRUE)
##' mintemp <- grep('tmin', names(env), value=TRUE)
##' 
##' # read them in as rasterStacks
##' meantemp <- stack(env[[meantemp]])
##' solar <- stack(env[[solar]])
##' maxtemp <- stack(env[[maxtemp]])
##' mintemp <- stack(env[[mintemp]])
##' tempRange <- abs(maxtemp - mintemp)
##' 
##' # get monthly PET
##' pet <- monthlyPET(meantemp, solar, tempRange)
##'
##' precip <- grep('prec', names(env), value=TRUE)
##' precip <- stack(env[[precip]])
##'
##' aridityIndexThornthwaite(precip, pet)
##' }
##' 
##' @export


aridityIndexThornthwaite <- function(precipStack, PETstack) {

	#enforce ordering of stack according to numbers in names
	precipStack <- precipStack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(precipStack))))]]
	PETstack <- PETstack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(PETstack))))]]

	#create receiving rasters
	d <- precipStack[[1]]
	raster::values(d)[!is.na(raster::values(d))] <- 0
	n <- precipStack[[1]]
	raster::values(n)[!is.na(raster::values(n))] <- 0

	# for each month, determine which cells have precip < PET
	# for those cells, add value of diff to receiver raster s
	# for those cells, add PET to n
	for (i in 1:12) {
		
		isPrecipLess <- precipStack[[i]] < PETstack[[i]]
		cells <- which(raster::values(isPrecipLess) == 0)
		tmp <- PETstack[[i]] - precipStack[[i]]
		tmp[cells] <- 0
		d <- d + tmp

		tmp <- PETstack[[i]]
		raster::values(tmp)[cells] <- 0
		n <- n + tmp
	}

	res <- 100*d
	res <- res / n
	nzeroes <- which(raster::values(n) == 0)
	res[nzeroes] <- 0
	names(res) <- 'aridityIndexThornthwaite'
	return(res)

}
