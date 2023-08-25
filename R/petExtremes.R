##' @title PET Extremes
##' @description Calculates summed PET of the coldest, warmest, wettest and driest quarters.
##'
##' @param PETstack SpatRaster of monthly PET,
##' 	layer names assumed to end in month numbers
##' @param precipStack SpatRaster of monthly precipitation
##' @param meantempStack SpatRaster of monthly mean temperature
##' 
##' @details Generates summed monthly PET for the warmest, coldest, wettest and driest 
##' 3 consecutive months. Previous versions of the envirem package incorrectly 
##' calculated mean quarterly PET. 
##'
##' @return SpatRaster of PETColdestQuarter, PETWarmestQuarter, PETWettestQuarter, PETDriestQuarter
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
##' env <- rast(rasterFiles)
##'
##' # identify the appropriate layers
##' meantemp <- grep('mean', names(env), value=TRUE)
##' solar <- grep('solrad', names(env), value=TRUE)
##' maxtemp <- grep('tmax', names(env), value=TRUE)
##' mintemp <- grep('tmin', names(env), value=TRUE)
##' precip <- grep('prec', names(env), value=TRUE)
##' 
##' # read them in as SpatRasters
##' meantemp <- env[[meantemp]]
##' solar <- env[[solar]]
##' maxtemp <- env[[maxtemp]]
##' mintemp <- env[[mintemp]]
##' tempRange <- abs(maxtemp - mintemp)
##' precip <- env[[precip]]
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
	PETstack <- PETstack[[order(as.numeric(gsub(paste0(.var$pet, '([0-9]+)', .var$pet_post), "\\1", names(PETstack))))]]
	
	possibleQuarters <- c(1:12, 1, 2)
	
	# calculate mean temperature and summed precipitation for each possible quarter. 
	tempQuarters <- vector('list', length = 12)
	precipQuarters <- vector('list', length = 12)
	for (i in 1:12) {
		tempQuarters[[i]] <- mean(meantempStack[[possibleQuarters[i:(i + 2)]]])
		precipQuarters[[i]] <- sum(precipStack[[possibleQuarters[i:(i + 2)]]])
	}
	
	tempQuarters <- terra::rast(tempQuarters)
	precipQuarters <- terra::rast(precipQuarters)

	# summed PET of coldest quarter
	QuarterInd <- which.min(tempQuarters)
	PETColdestQuarter <- terra::rast(QuarterInd)
	for (i in 1:12) {
		cells <- which(terra::values(QuarterInd) == i)
		PETColdestQuarter[cells] <- sum(PETstack[[possibleQuarters[i:(i + 2)]]])[cells]	
	}
	names(PETColdestQuarter) <- 'PETColdestQuarter'
	
	# summed PET of warmest quarter
	QuarterInd <- which.max(tempQuarters)
	PETWarmestQuarter <- terra::rast(QuarterInd)
	for (i in 1:12) {
		cells <- which(terra::values(QuarterInd) == i)
		PETWarmestQuarter[cells] <- sum(PETstack[[possibleQuarters[i:(i + 2)]]])[cells]	
	}
	names(PETWarmestQuarter) <- 'PETWarmestQuarter'
	
	# summed PET of wettest quarter
	QuarterInd <- which.max(precipQuarters)
	PETWettestQuarter <- terra::rast(QuarterInd)
	for (i in 1:12) {
		cells <- which(terra::values(QuarterInd) == i)
		PETWettestQuarter[cells] <- sum(PETstack[[possibleQuarters[i:(i + 2)]]])[cells]	
	}
	names(PETWettestQuarter) <- 'PETWettestQuarter'

	# summed PET of driest quarter
	QuarterInd <- which.min(precipQuarters)
	PETDriestQuarter <- terra::rast(QuarterInd)
	for (i in 1:12) {
		cells <- which(terra::values(QuarterInd) == i)
		PETDriestQuarter[cells] <- sum(PETstack[[possibleQuarters[i:(i+2)]]])[cells]	
	}
	names(PETDriestQuarter) <- 'PETDriestQuarter'

	return(c(PETColdestQuarter, PETWarmestQuarter, PETWettestQuarter, PETDriestQuarter))
	
}