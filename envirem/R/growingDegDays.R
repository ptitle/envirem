##' @title Growing degree days
##'
##' @description Growing degree days above some base temperature.
##'
##' @param meantempstack rasterStack of mean monthly temperature in deg C * 10
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
##' meantemp <- env[[grep('tmean', names(env), value=TRUE)]]
##' growingDegDays(meantemp, 10, tempScale = 10)
##' @export

# Growing degree-days on 0 or 5 degC base
# needed = mean monthly temp for all months
growingDegDays <- function(meantempstack, baseTemp, tempScale = 1) {
	
	meantempstack <- meantempstack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(meantempstack))))]]
	
	# we are operating in deg C, rather than deg C * tempScale
	meantempstack <- meantempstack / tempScale
	
	#get raster of number of months > baseTemp
	logicstack <- meantempstack > baseTemp
	logicsum <- sum(logicstack)
	
	#get rasterstack of temps above baseTemp (set all temps below baseTemp to 0)
	for (i in 1:raster::nlayers(meantempstack)) {
		cells <- which(raster::values(meantempstack[[i]]) < baseTemp)
		if (length(cells) > 0) {
			meantempstack[[i]][cells] <- 0
		}
	}
	
	#formula: summation of temps for all months with temp > baseTemp * total number of days in those months
	#sum temps
	meantempstack <- sum(meantempstack)
	res <- raster::overlay(meantempstack, logicsum, fun = function(a,b) {return(a * b * 30)})
	names(res) <- paste0('growingDegDays', baseTemp)
	return(res)
}
