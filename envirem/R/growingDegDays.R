##' @title Growing degree days
##'
##' @description Growing degree days above some base temperature.
##'
##' @param meantempstack rasterStack of mean monthly temperature in deg C
##'
##' @param baseTemp base temperature in degrees C.
##'
##'	@param tempScale integer; scaling factor for the temperature data, see \link{envirem} for 
##' 	additional details. 
##'
##' @details growing degree days = sum of all monthly temps greater than baseTemp, 
##' multiplied by total number of days
##'
##' @return rasterLayer in degrees C * days.
##'
##' @references

##' Prentice, I.C., Cramer, W., Harrison, S.P., Leemans, R., Monserud, R.A. & Solomon, A.M.
##' (1992). A Global Biome Model Based on Plant Physiology and Dominance, Soil Properties
##'  and Climate. \emph{Journal of Biogeography}, \strong{19}, 117–134.
##' 
##' @author Pascal Title
##'
##' @examples
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' env <- stack(rasterFiles)
##'
##' meantemp <- env[[grep('tmean', names(env), value=TRUE)]]
##' growingDegDays(meantemp, 10, tempScale = 10)
##' @export

# Growing degree-days on 0 or 5 degC base
# needed = mean monthly temp for all months
growingDegDays <- function(meantempstack, baseTemp, tempScale = 1) {
	
	# we are operating in deg C, rather than deg C * tempScale
	if (tempScale != 1) {
		meantempstack <- meantempstack / tempScale
	}
	
	Ndays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
	
	gdd <- meantempstack
	for (i in 1:raster::nlayers(meantempstack)) {
		gdd[[i]] <- meantempstack[[i]] - baseTemp
		gdd[[i]][gdd[[i]] < 0] <- 0
		gdd[[i]] <- gdd[[i]] * Ndays[i]
	}
	
	gdd <- sum(gdd)
	names(gdd) <- paste0("growingDegDays", baseTemp)
	
	return(gdd)
}
