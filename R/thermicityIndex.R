##' @title Compensated Thermicity index
##'
##' @description Compensated Thermicity index
##'
##' @param annualTemp rasterLayer, mean annual temperature
##'
##' @param minTemp rasterLayer, min temp of the coldest month
##'
##' @param maxTemp rasterLayer, max temp of the coldest month
##'
##' @param continentality rasterLayer, continentality index
##'
##' @param returnCompensated logical: if \code{FALSE}, regular thermicity index is returned.
##'
##'	@param tempScale integer; scaling factor for the temperature data, see \link{envirem} for 
##' 	additional details. 
##'
##'
##' @details 	thermicity index = tempRange + minTemp + maxTemp
##'
##'	The compensated thermicity index incorporates corrections designed to make this metric
##'	more appropriately comparable across the globe.
##'
##' @return rasterLayer in degrees C
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
##' @seealso \link{continentality}
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
##' ci <- continentality(temp[['meanTempWarmest']], temp[['meanTempColdest']], tempScale = 10)
##'
##' # compensated thermicity index
##' thermicityIndex(env[['bio_1']], env[['bio_6']], temp[['maxTempColdest']], ci, tempScale = 10)
##' }
##' @export


## compensated thermicity Index
# (annual mean temp, min temp of coldest month, max temp of coldest month) * 10
thermicityIndex <- function(annualTemp, minTemp, maxTemp, continentality, returnCompensated = TRUE, tempScale = 1) {
	
	annualTemp <- annualTemp / tempScale * 10
	minTemp <- minTemp / tempScale * 10
	maxTemp <- maxTemp / tempScale * 10
	
	thermicity <- annualTemp + minTemp + maxTemp
	
	if (returnCompensated) {
		# use continentality values to implement corrections for compensated metric
		ind <- which(terra::values(continentality) < 9)
		c1 <- continentality
		c1[setdiff(1:terra::ncell(c1), ind)] <- NA
		c1[ind] <- (9 - c1[ind]) * 10
		thermicity[ind] <- thermicity[ind] - c1[ind]		
		
		ind <- intersect(which(terra::values(continentality) > 18), which(terra::values(continentality) <= 21))
		c1 <- continentality
		c1[setdiff(1:terra::ncell(c1), ind)] <- NA
		c1[ind] <- (c1[ind] - 18) * 5
		thermicity[ind] <- thermicity[ind] + c1[ind]
		
		ind <- intersect(which(terra::values(continentality) > 21), which(terra::values(continentality) <= 28))
		c1 <- continentality
		c1[setdiff(1:terra::ncell(c1), ind)] <- NA
		c1[ind] <- ((c1[ind] - 21) * 15) + 15
		thermicity[ind] <- thermicity[ind] + c1[ind]
		
		ind <- intersect(which(terra::values(continentality) > 28), which(terra::values(continentality) <= 46))
		c1 <- continentality
		c1[setdiff(1:terra::ncell(c1), ind)] <- NA
		c1[ind] <- ((c1[ind] - 28) * 25) + 15 + 105
		thermicity[ind] <- thermicity[ind] + c1[ind]
	
		ind <- intersect(which(terra::values(continentality) > 46), which(terra::values(continentality) <= 65))
		c1 <- continentality
		c1[setdiff(1:terra::ncell(c1), ind)] <- NA
		c1[ind] <- ((c1[ind] - 46) * 30) + 15 + 105 + 425
		thermicity[ind] <- thermicity[ind] + c1[ind]
		
	}

	names(thermicity) <- 'thermicity'
	
	return(thermicity)
}



# thermicity:
# 1,1,1,1
# 1,NA,1,1
# 1,1,NA,1
# 1,1,1,1

# continentality:
# 2,3,24,45
# 19,NA,50,21
# 75,29,NA,21
# 31,35,52,4

# thermicity <- raster(nrow=4,ncol=4)
# values(thermicity) <- c(1,1,1,1,1,NA,1,1,1,1,NA,1,1,1,1,1)

# continentality <- raster(nrow=4,ncol=4)
# values(continentality) <- c(2,3,24,45,19,NA,50,21,75,29,NA,21,31,35,52,4)

