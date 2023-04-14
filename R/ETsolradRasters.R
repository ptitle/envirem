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
##' @return If \code{outputDir = NULL}, a RasterStack is returned. Otherwise, rasters
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
##' env <- stack(rasterFiles)
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
	
	solradStack <- vector('list', 12)
	
	if ((grepl('+proj=longlat', raster::projection(rasterTemplate)))) {
		isLongLat <- TRUE
	} else {
		isLongLat <- FALSE
	}
	
	if (!isLongLat) {
		
		rasterTemplate2 <- rasterTemplate
				
		# get coordinates of cells with data, and transform to long/lat
		templatePts <- raster::rasterToPoints(rasterTemplate)
		templatePtsLongLat <- sf::st_as_sf(as.data.frame(templatePts), coords = c('x', 'y'), crs = raster::projection(rasterTemplate))
		templatePtsLongLat <- sf::st_transform(templatePtsLongLat, crs = '+proj=longlat +datum=WGS84')
		
		# get an estimate of resolution in long/lat
		extentLL <- raster::extent(templatePtsLongLat)
		resx <- (extentLL@xmax - extentLL@xmin)  / raster::ncol(rasterTemplate)
		resy <- (extentLL@ymax - extentLL@ymin)  / raster::nrow(rasterTemplate)
		resx <- floor(resx * 100) / 100
		resy <- floor(resy * 100) / 100
		
		rasterTemplate <- raster::raster(ext = extentLL, res = c(resx, resy), crs = '+proj=longlat +datum=WGS84')
		
		rasterTemplate[] <- raster::extract(rasterTemplate2, rgdal::project(coordinates(rasterTemplate), projection(rasterTemplate2)))
	}
		
	# extract latitudes from cells
	latvals <- raster::yFromCell(rasterTemplate, raster::cellFromCol(rasterTemplate, 1))
	
	# for each month, calculate insolation and fill raster
	for (i in 1:12) {
		
		message(i, ' ', appendLF = FALSE)	
		RA <- sapply(latvals, function(x) calcSolRad(year = year, lat = x, month = i))
		ras <- raster::raster(rasterTemplate)
		tmp <- rep(RA, each = raster::ncol(rasterTemplate))
		tmp <- matrix(data = tmp, nrow = raster::nrow(ras), ncol = raster::ncol(ras), byrow = TRUE)
		ras[ ] <- tmp
		solradStack[[i]] <- ras
	}
		
	solradStack <- raster::stack(solradStack)
	raster::projection(solradStack) <- raster::projection(rasterTemplate)
	
	# rename using the naming scheme supplied
	names(solradStack) <- paste0(.var$solrad, sprintf("%02d", 1:12), .var$solrad_post)
	
	if (!isLongLat) {	
		
		# project to input projection
		e <- raster::projectExtent(solradStack, crs = raster::projection(rasterTemplate2))
		newStack <- raster::projectRaster(solradStack, to = e, res = raster::res(rasterTemplate2))
		newStack <- raster::resample(newStack, rasterTemplate2)
		newStack <- raster::mask(newStack, rasterTemplate2)
		solradStack <- newStack
		rm(newStack)
		
	} else {	
	
		# mask NA regions
		solradStack <- raster::mask(solradStack, rasterTemplate)	
	}
	
	# write to disk, if requested
	if (is.null(outputDir)) {

		return(solradStack)

	} else {

		outputDir <- gsub('/?$', '/', outputDir)
		tifOptions <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")
		
		for (i in 1:raster::nlayers(solradStack)) {
			outputName <- paste0(outputDir, names(solradStack)[i], '.tif')
			raster::writeRaster(solradStack[[i]], outputName, datatype = 'FLT4S', NAflag = -9999, options = tifOptions, ...)	
		}
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




