##' @title Center raster on the Pacific
##'
##' @description Takes a raster that is centered on 0 longitude (default) and recenters it on the Pacific
##'
##' @param r rasterLayer or rasterStack in unprojected geographic coordinates

##' @param crop logical, should raster then be cropped to longitude [100, 300]
##'
##' @details Cropping to [100, 300] is equivalent to [100, -60]
##'
##' @return rasterLayer or rasterStack
##'
##' @author Pascal Title
##'
##' @examples
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' tmin1 <- raster(grep('tmin_1\\.', rasterFiles, value=TRUE))
##' 
##' pacificCentric(tmin1, crop = TRUE)
##' @export


# function to center raster on longitude = 180/-180
# x axis becomes 0 - 360 rather than -180/180
# returns a raster cropped to [100, 300] or [100, -60]
pacificCentric <- function(r, crop = TRUE) {
	
	if (raster::extent(r)@xmin < -180 | raster::extent(r)@xmax > 180) {
		r <- raster::crop(r, extent(-180, 180, -90, 90))
	}

	wHemisphere <- raster::extent(-180, 0, -90, 90)
	eHemisphere <- raster::extent(0, 180, -90, 90)
	leftRas <- raster::crop(r, wHemisphere)
	rightRas <- raster::crop(r, eHemisphere)
	leftRas <- raster::shift(leftRas, 360)
	newRas <- raster::raster(xmn=0, xmx=360,ymn=-90,ymx=90, resolution=rep(raster::res(r), 2))
	origin(rightRas) <- origin(newRas)
	origin(leftRas) <- origin(newRas)
	origin(r) <- origin(newRas)
	newRas <- raster::merge(newRas, rightRas)
	newRas <- raster::merge(newRas, leftRas)
	if (crop) {
		newRas <- raster::crop(newRas, raster::extent(100, 300, -90, 90))
	}
	return(newRas)
}
