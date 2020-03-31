##' @title Extraterrestrial Solar Radiation
##'
##' @description Generate monthly extraterrestrial solar radiation rasters.
##'
##' @param rasterTemplate any rasterLayer that can be used to extract 
##' 	extent, resolution, etc.
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
	
	if (grepl('+proj=longlat', raster::projection(rasterTemplate))) {
				
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

	} else {

		templatePts <- raster::rasterToPoints(rasterTemplate)
		templatePtsLongLat <- sf::st_as_sf(as.data.frame(templatePts), coords = c('x', 'y'), crs = raster::projection(rasterTemplate))
		templatePtsLongLat <- sf::st_transform(templatePtsLongLat, crs = '+proj=longlat +datum=WGS84')
			
		templatePts <- cbind(templatePts, sf::st_coordinates(templatePtsLongLat))
		colnames(templatePts) <- c('x', 'y', raster::labels(rasterTemplate), 'long', 'lat')
		
		# uniqueLat <- unique(templatePts[,2])
		# uniqueLatTransformed <- templatePts[sapply(uniqueLat, function(x) which(templatePts[,2] == x)[1]), 'lat']
		# names(uniqueLatTransformed) <- NULL

		# for each month, calculate insolation and fill raster
		for (i in 1:12) {
			
			message(i, ' ', appendLF = FALSE)
			RA <- sapply(templatePts[,5], function(x) calcSolRad(year = year, lat = x, month = i))
			ras <- raster::raster(rasterTemplate)
			values(ras)[!is.na(values(rasterTemplate))] <- RA
			solradStack[[i]] <- ras
		}
	}
	
	solradStack <- raster::stack(solradStack)
	raster::projection(solradStack) <- raster::projection(rasterTemplate)
	
	# rename using the naming scheme supplied
	names(solradStack) <- paste0(.var$solrad, sprintf("%02d", 1:12), .var$solrad_post)
	
	# mask NA regions
	solradStack <- raster::mask(solradStack, rasterTemplate)	
	
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




