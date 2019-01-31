##' @title monthly PET
##'
##' @description Monthly potential evapotranspiration
##'
##' @param Tmean rasterStack of monthly mean temperature

##' @param RA rasterStack of monthly extraterrestrial solar radiation
##'
##' @param TD rasterStack of monthly temperature range
##'
##'	@param tempScale integer; scaling factor for the temperature data, see \link{envirem} for 
##' 	additional details. 
##'
##' @details \code{PET = 0.0023 * RA * (Tmean + 17.8) * TD ^ 0.5}
##'
##' @return rasterStack of monthly PET in mm / month
##'
##' @references
##' Hargreaves, G. L., Hargreaves, G. H., & Riley, J. P. (1985). Irrigation water requirements 
##' for Senegal River basin. \emph{Journal of Irrigation and Drainage Engineering}, 
##' \strong{111}, 265-275.
##' 
##' Zomer, R.J., Trabucco, A., Bossio, D.A. & Verchot, L.V. (2008). Climate change mitigation: 
##' A spatial analysis of global land suitability for clean development mechanism afforestation
##' and reforestation. \emph{Agriculture, Ecosystems and Environment}, \strong{126}, 67-80.
##' 
##' Zomer, R.J., Trabucco, A., Van Straaten, O. & Bossio, D.A. (2006) \emph{Carbon, Land and Water: 
##' A Global Analysis of the Hydrologic Dimensions of Climate Change Mitigation through
##' Afforestation/Reforestation. International Water Management Institute Research Report 101}.
##' Colombo, Sri Lanka.
##'
##' @author Pascal Title
##'
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
##' 
##' # read them in as rasterStacks
##' meantemp <- stack(env[[meantemp]])
##' solar <- stack(env[[solar]])
##' maxtemp <- stack(env[[maxtemp]])
##' mintemp <- stack(env[[mintemp]])
##' tempRange <- abs(maxtemp - mintemp)
##' 
##' monthlyPET(meantemp, solar, tempRange, tempScale = 10)
##' }
##' 
##' @export


#monthly potential evapotranspiration
# method taken from CGIAR-CSI
# PET = 0.0023 * RA * (Tmean + 17.8) * TD^0.5 = monthly average PET (mm/month)
# where RA = extra terrestrial radiation, Tmean = mean temp, TD = daily temp range (monthly Bio2)
# as we want annual mean, we'll just use annual mean temp, and mean daily temp range
monthlyPET <- function(Tmean, RA, TD, tempScale = 1) {
	
	if (raster::nlayers(Tmean) != 12) {
		stop('Tmean should have 12 layers.')
	}
	if (raster::nlayers(RA) != 12) {
		stop('RA should have 12 layers.')
	}
	if (raster::nlayers(TD) != 12) {
		stop('TD should have 12 layers.')
	}

	names(TD) <- names(Tmean)
	
	Tmean <- Tmean[[order(as.numeric(gsub(paste0(.var$tmean, '([0-9]+)', .var$tmean_post), "\\1", names(Tmean))))]]
	RA <- RA[[order(as.numeric(gsub(paste0(.var$solrad, '([0-9]+)', .var$solrad_post), "\\1", names(RA))))]]
	TD <- TD[[order(as.numeric(gsub(paste0(.var$tmean, '([0-9]+)', .var$tmean_post), "\\1", names(TD))))]]
	
	if (tempScale != 1) {
		Tmean <- Tmean / tempScale
		TD <- TD / tempScale
	}
	
	res <- 0.0023 * (RA * 30) * (Tmean + 17.8) * TD ^ 0.5
	for (i in 1:raster::nlayers(res)) {
		raster::values(res[[i]])[which(raster::values(res[[i]]) < 0)] <- 0
	}
	names(res) <- paste0('PET_', sprintf("%02d", 1:12))
	return(res)
}
