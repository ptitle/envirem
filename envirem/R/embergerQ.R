##' @title Emberger's pluviometric quotient
##'
##' @description Calculate Emberger's pluviometric quotient.
##'
##' @param P rasterLayer, mean annual precipitation

##' @param M rasterLayer, mean max temperature of the warmest month
##'
##' @param m rasterLayer, mean min temperature of the coldest month
##'
##'	@param tempScale integer; scaling factor for the temperature data, see \link{envirem} for 
##' 	additional details. 
##'
##' @details \code{Q = 2000 P / [(M + m + 546.4) * (M - m)]}
##'
##' @return rasterLayer in mm / degrees C
##'
##' @references
##' Daget, P. (1977) Le bioclimat méditerranéen: analyse des formes climatiques 
##' par le système d’Emberger. \emph{Vegetatio}, \strong{34}, 87–103.
##' 
##' @author Pascal Title
##'
##' @examples
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' env <- stack(rasterFiles)
##'
##' embergerQ(env[['bio_12']], env[['bio_5']], env[['bio_6']], tempScale = 10)
##' @export


## Emberger's pluviometric quotient
# Q = 2000 P / (M + m + 546.4) (M - m)
# P = mean annual precip
# M = mean max temp of warmest month
# m = mean min temp of coldest month
embergerQ <- function(P, M, m, tempScale = 1) {
	
	#switch to 1 deg Celsius
	raster::values(M) <- raster::values(M) / tempScale
	raster::values(m) <- raster::values(m) / tempScale
	res <- 2000 * P / ((M + m + 546.4) * (M - m))
	names(res) <- 'embergerQ'
	return(res)
}
