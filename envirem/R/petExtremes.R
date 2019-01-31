##' @title PET Extremes
##' @description Calculates mean PET of the coldest, warmest, wettest and driest quarters.
##'
##' @param PETstack rasterStack of monthly PET,
##' 	layer names assumed to end in month numbers
##' @param precipStack rasterStack of monthly precipitation
##' @param meantempStack rasterStack of monthly mean temperature
##' 
##' @details Generates mean monthly PET for the warmest, coldest, wettest and driest 
##' 3 consecutive months.
##'
##' @return rasterStack of PETColdestQuarter, PETWarmestQuarter, PETWettestQuarter, PETDriestQuarter
##' in mm / month.
##' 
##' @seealso \code{\link{monthlyPET}}
##' 
##' @references
##' Metzger, M.J., Bunce, R.G.H., Jongman, R.H.G., Sayre, R., Trabucco, A. & Zomer, R. (2013). 
##' A high-resolution bioclimate map of the world: a unifying framework for global 
##' biodiversity research and monitoring. \emph{Global Ecology and Biogeography}, 
##' \strong{22}, 630-638.
##' 
##' @author Pascal Title
##' @examples
##' \donttest{
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' env <- stack(rasterFiles)
##'
##' # identify the appropriate layers
##' meantemp <- grep('mean', names(env), value=TRUE)
##' solar <- grep('solrad', names(env), value=TRUE)
##' maxtemp <- grep('tmax', names(env), value=TRUE)
##' mintemp <- grep('tmin', names(env), value=TRUE)
##' precip <- grep('prec', names(env), value=TRUE)
##' 
##' # read them in as rasterStacks
##' meantemp <- stack(env[[meantemp]])
##' solar <- stack(env[[solar]])
##' maxtemp <- stack(env[[maxtemp]])
##' mintemp <- stack(env[[mintemp]])
##' tempRange <- abs(maxtemp - mintemp)
##' precip <- stack(env[[precip]])
##'
##' # set up naming scheme - only precip is different from default
##' assignNames(precip = 'prec_##')
##' 
##' # get monthly PET
##' pet <- monthlyPET(meantemp, solar, tempRange)
##'
##' petExtremes(pet, precip, meantemp)
##'
##' # set back to defaults
##' assignNames(reset = TRUE)
##' }
##' @export

petExtremes <- function(PETstack, precipStack, meantempStack) {
	
	#enforce ordering
	meantempStack <- meantempStack[[order(as.numeric(gsub(paste0(.var$tmean, '([0-9]+)', .var$tmean_post), "\\1", names(meantempStack))))]]
	precipStack <- precipStack[[order(as.numeric(gsub(paste0(.var$precip, '([0-9]+)', .var$precip_post), "\\1", names(precipStack))))]]
	PETstack <- PETstack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(PETstack))))]]

	possibleQuarters <- c(1:12, 1, 2)
	
	# calculate mean temperature and mean precipitation for each possible quarter. 
	tempQuarters <- vector('list', length = 12)
	precipQuarters <- vector('list', length = 12)
	for (i in 1:12) {
		tempQuarters[[i]] <- mean(meantempStack[[possibleQuarters[i:(i + 2)]]])
		precipQuarters[[i]] <- mean(precipStack[[possibleQuarters[i:(i + 2)]]])
	}
	
	tempQuarters <- raster::stack(tempQuarters)
	precipQuarters <- raster::stack(precipQuarters)

	# mean PET of coldest quarter
	QuarterInd <- which.min(tempQuarters)
	PETColdestQuarter <- raster::raster(QuarterInd)
	for (i in 1:12) {
		cells <- which(raster::values(QuarterInd) == i)
		PETColdestQuarter[cells] <- mean(PETstack[[possibleQuarters[i:(i + 2)]]])[cells]	
	}
	names(PETColdestQuarter) <- 'PETColdestQuarter'
	
	# mean PET of warmest quarter
	QuarterInd <- which.max(tempQuarters)
	PETWarmestQuarter <- raster::raster(QuarterInd)
	for (i in 1:12) {
		cells <- which(raster::values(QuarterInd) == i)
		PETWarmestQuarter[cells] <- mean(PETstack[[possibleQuarters[i:(i + 2)]]])[cells]	
	}
	names(PETWarmestQuarter) <- 'PETWarmestQuarter'
	
	# mean PET of wettest quarter
	QuarterInd <- which.max(precipQuarters)
	PETWettestQuarter <- raster::raster(QuarterInd)
	for (i in 1:12) {
		cells <- which(raster::values(QuarterInd) == i)
		PETWettestQuarter[cells] <- mean(PETstack[[possibleQuarters[i:(i + 2)]]])[cells]	
	}
	names(PETWettestQuarter) <- 'PETWettestQuarter'

	# mean PET of driest quarter
	QuarterInd <- which.min(precipQuarters)
	PETDriestQuarter <- raster::raster(QuarterInd)
	for (i in 1:12) {
		cells <- which(raster::values(QuarterInd) == i)
		PETDriestQuarter[cells] <- mean(PETstack[[possibleQuarters[i:(i+2)]]])[cells]	
	}
	names(PETDriestQuarter) <- 'PETDriestQuarter'

	return(raster::stack(PETColdestQuarter, PETWarmestQuarter, PETWettestQuarter, PETDriestQuarter))
	
}