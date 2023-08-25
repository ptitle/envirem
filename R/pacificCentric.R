##' @title Center raster on the Pacific
##'
##' @description Takes a raster that is centered on 0 longitude (default) and recenters it on the Pacific
##'
##' @param r rasterLayer or SpatRaster in unprojected geographic coordinates

##' @param crop logical, should raster then be cropped to longitude [100, 300]
##'
##' @details Cropping to [100, 300] is equivalent to [100, -60]
##'
##' @return rasterLayer or SpatRaster
##'
##' @author Pascal Title
##'
##' @examples
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' tmin1 <- rast(grep('tmin_1\\.', rasterFiles, value=TRUE))
##' 
##' pacificCentric(tmin1, crop = TRUE)
##' @export


# function to center raster on longitude = 180/-180
# x axis becomes 0 - 360 rather than -180/180
# returns a raster cropped to [100, 300] or [100, -60]
pacificCentric <- function(r, crop = TRUE) {
	
	if (terra::ext(r)$xmin < -180 | terra::ext(r)$xmax > 180) {
		r <- terra::crop(r, terra::ext(-180, 180, -90, 90))
	}

	wHemisphere <- terra::ext(-180, 0, -90, 90)
	eHemisphere <- terra::ext(0, 180, -90, 90)
	leftRas <- terra::crop(r, wHemisphere)
	rightRas <- terra::crop(r, eHemisphere)
	leftRas <- terra::shift(leftRas, 360)
	newRas <- terra::rast(xmin=0, xmax=360, ymin=-90, ymax = 90, resolution = terra::res(r))
	terra::origin(rightRas) <- terra::origin(newRas)
	terra::origin(leftRas) <- terra::origin(newRas)
	terra::origin(r) <- terra::origin(newRas)
	newRas <- terra::merge(newRas, rightRas)
	newRas <- terra::merge(newRas, leftRas)
	if (crop) {
		newRas <- terra::crop(newRas, terra::ext(100, 300, -90, 90))
	}
	return(newRas)
}
