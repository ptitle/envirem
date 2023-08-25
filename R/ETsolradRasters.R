##' @title Extraterrestrial Solar Radiation
##'
##' @description Generate monthly extraterrestrial solar radiation rasters.
##'
##' @param rasterTemplate any rasterLayer that can be used to extract 
##' 	extent, resolution, projection, etc.
##'
##' @param year The year solar radiation should be calculated for. See details.
##'
##' @param outputDir destination directory for rasters, can be \code{NULL}
##'
##' @param ... additional arguments passed to \code{writeRaster}
##' 
##'
##' @details Given the latitude values of the cells found in the raster template
##' 	and the year, monthly extraterrestrial solar radiation can be calculated,
##'		using the palinsol R package. \code{year = 0} corresponds to 1950. 
##'		Although the year can take on any value, it should match the time period 
##' 	of the other rasters that will be used for generating ENVIREM variables. 
##' 	Suggestions would be \code{year = 40} for the present, \code{year = -6000} 
##' 	for the mid Holocene, and \code{year = -21500} for the LGM.
##' 	
##' 	If you are having problems with this function and the rasterTemplate is not in 
##' 	long/lat, try with an unprojected long/lat raster.
##'
##' @return If \code{outputDir = NULL}, a SpatRaster is returned. Otherwise, rasters
##'		are written to disk in the designated directory, and nothing is returned. 
##'		Naming of the layers uses the tag specified via \code{\link{assignNames}}. 
##'
##' @references
##' J. Laskar et al., A long-term numerical solution for the insolation quantities of the Earth, 
##' \emph{Astron. Astroph.}, \strong{428}, 261-285  2004.
##'
##'
##' @author Pascal Title
##'
##' @examples
##' \donttest{
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' env <- rast(rasterFiles)
##'
##' # set aside a template raster
##' template <- env[[1]]
##'
##' # generate solar radiation for the present
##' solrad <- ETsolradRasters(template, year = 40, outputDir = NULL)
##'
##' }
##' @export

# function to produce monthly rasters of solar radiation, based on a raster template
## current time period: year = 40 (~ mid point in CHELSA range of 1979-2013)
## midHolocene: -6000
## LGM: -21500

ETsolradRasters <- function(rasterTemplate, year, outputDir = NULL, ...) {
		
	if (terra::is.lonlat(rasterTemplate)) {
		isLongLat <- TRUE
	} else {
		isLongLat <- FALSE
	}
	
	if (!isLongLat) {
		
		rasterTemplate2 <- rasterTemplate
		rasterTemplate <- terra::project(rasterTemplate, 'EPSG:4326')
		
		# rasterTemplate2 <- rasterTemplate
		# # get coordinates of cells with data, and transform to long/lat
		# templatePts <- terra::as.points(rasterTemplate)
		# templatePtsLongLat <- terra::project(templatePts, 'EPSG:4326')
		
		# # get an estimate of resolution in long/lat
		# extentLL <- terra::ext(templatePtsLongLat)	
		# resx <- (extentLL$xmax - extentLL$xmin)  / terra::ncol(rasterTemplate)
		# resy <- (extentLL$ymax - extentLL$ymin)  / terra::nrow(rasterTemplate)
		# resx <- floor(resx * 100) / 100
		# resy <- floor(resy * 100) / 100
		
		# rasterTemplate <- terra::rast(extentLL, res = c(resx, resy), crs = 'EPSG:4326')
		
		# terra::values(rasterTemplate) <- terra::extract(rasterTemplate2, terra::project(terra::as.points(rasterTemplate), rasterTemplate2), ID = FALSE)[,1]
		
	}
		
	# extract latitudes from cells
	latvals <- terra::yFromRow(rasterTemplate, row = 1:nrow(rasterTemplate))
	
	# for each month, calculate insolation and fill raster
	solradStack <- terra::rast(rasterTemplate, nlyrs = 12)
	for (i in 1:12) {
		
		message(i, ' ', appendLF = FALSE)	
		RA <- sapply(latvals, function(x) calcSolRad(year = year, lat = x, month = i))
		tmp <- rep(RA, each = terra::ncol(rasterTemplate))
		tmp <- matrix(data = tmp, nrow = terra::nrow(rasterTemplate), ncol = terra::ncol(rasterTemplate), byrow = FALSE)
		ras <- terra::init(rasterTemplate, fun = tmp)
		solradStack[[i]] <- ras
	}
		
	# rename using the naming scheme supplied
	names(solradStack) <- paste0(.var$solrad, sprintf("%02d", 1:12), .var$solrad_post)
	
	if (!isLongLat) {	
		
		# project to input projection
		solradStack <- terra::project(solradStack, rasterTemplate2)
		
		# mask NA regions
		solradStack <- terra::mask(solradStack, rasterTemplate2)	
				
	} else {	
	
		# mask NA regions
		solradStack <- terra::mask(solradStack, rasterTemplate)	
	}
	
	# write to disk, if requested
	if (is.null(outputDir)) {

		return(solradStack)

	} else {

		outputDir <- gsub('/?$', '/', outputDir)
		outputName <- paste0(outputDir, names(solradStack), '.tif')
		terra::writeRaster(solradStack, outputName, datatype = 'FLT4S', NAflag = -9999, ...)
		
	}
}



calcSolRad <- function(year, lat, month) {
	# year is number of years after 1950
	# lat is in degrees
	# month is numeric, will be converted into 15th day of the month to get mid-month mean solar radiation
		
	# define orbit
	orb <- la04(t = year)
	
	# convert date to julian day
	day <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)[month]
	
	# convert julian day to day based on 360-day year
	day <- day * 360/365
	
	# calculate solar longitude for given year
	# t = time after 1950, day is in 360-day calendar
	solarLong <- day2l(orb, day = day)
	
	# returns daily mean insolation in Watts/m2
	solrad <- Insol(orbit = orb, long = solarLong, lat = lat*pi/180)
	
	# convert to MJ/m2 by multiplying by 0.0864, and then by 0.408 to get equivalent evaporation in mm / day
	solrad * 0.0864 * 0.408
}




