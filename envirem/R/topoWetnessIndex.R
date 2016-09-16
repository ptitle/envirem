##' @title Topographic Wetness Index
##'
##' @description SAGA-GIS topographic wetness index
##'
##' @param dem elevation rasterLayer, with defined proj4string. 
##'
##' @param sagaEnv list object returned from \code{RSAGA::rsaga.env}, 
##' which supplies appropriate SAGA paths, and parallelization information.
##'
##' @details From a DEM, this function will write an appropriate raster
##' to disk, run an RSAGA function to calculate the topographic wetness index,
##' and will then read it back in and return it. 
##'
##' This function requires that SAGA-GIS be installed on your system.
##' SAGA-GIS can be found at \url{www.saga-gis.org}. 
##'
##' See the documentation for \code{RSAGA::rsaga.env} for specifying appropriate paths
##' and parallelization details.
##'
##' @return rasterLayer, unitless
##' 
##' @references
##' Boehner, J., Koethe, R. Conrad, O., Gross, J., Ringeler, A. & Selige, T. (2002) Soil
##' regionalization by means of terrain analysis and process parameterization. \emph{Soil
##' Classification 2001 European Soil Bureau, Research Report No. 7} (eds Micheli, E., 
##' Nachtergaele, F. & Montanarella, L.), pp. 213-222. Luxembourg.
##'
##' Conrad, O., Bechtel, B., Bock, M., Dietrich, H., Fischer, E., Gerlitz, L., Wehberg, J.,
##' Wichmann, V. & Böhner, J. (2015) System for automated geoscientific analyses (SAGA)
##' v. 2.1.4. \emph{Geoscientific Model Development}, \strong{8}, 1991-2007.
##'
##' @author Pascal Title
##'
##' @examples
##' \dontrun{
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' elev <- raster(grep('elev', rasterFiles, value=TRUE))
##' 
##' # setting up appropriate RSAGA environment
##' sagaEnv <- RSAGA::rsaga.env(modules = '/usr/lib/x86_64-linux-gnu/saga/', cores = 2, 
##' parallel = TRUE, version = "2.2.0")
##' topoWetnessIndex(elev, sagaEnv)
##' }
##' @export



# calculate topographic wetness index with SAGA-GIS


# dem: elevation raster
# sagaEnv: an object returned from rsaga.env, which supplies appropriate SAGA paths, and parallelization information.

topoWetnessIndex <- function(dem, sagaEnv) {
	
	if (is.null(raster::projection(dem))) {
		stop('dem must have a proj4string.')
	}

	# THIS MANY NOT ACTUALLY BE A PROBLEM.
	# # TWI cannot be done with negative values, so increase all elevation to be above zero
	# if (minValue(dem) < 0) {
	# 	if (is.null(minVal)) {
	# 		dem <- dem + abs(minValue(dem)) + 10
	# 	} else {
	# 		dem <- dem + abs(minVal)
	# 	}
	# }
	
	#if raster is not projected, do so
	if (!grepl('units=m', raster::projection(dem))) {
		origProj <- raster::projection(dem)
		behrmann <- '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'
		e <- raster::projectExtent(dem, behrmann)
		dem <- raster::projectRaster(dem, e)
		transformed <- TRUE
	} else {
		transformed <- FALSE
	}

	# write raster to SAGA format	
	raster::writeRaster(dem, filename = 'saga_temp_twi_gtif.tif', format = 'GTiff')
	RSAGA::rsaga.import.gdal(in.grid = 'saga_temp_twi_gtif.tif', out.grid = 'saga_temp_twi_in', env = sagaEnv)

	#temporary outfile
	outfile <- 'saga_temp_twi_out.sgrd'
	call <- RSAGA::rsaga.wetness.index(in.dem = 'saga_temp_twi_in.sgrd', out.wetness.index = outfile, env = sagaEnv)

	#read resulting file back in
	res <- raster::raster('saga_temp_twi_out.sdat')
	
	#project back to input projection
	if (transformed) {
		e <- raster::projectExtent(res, origProj)
		res <- raster::projectRaster(res, e)
	}

	# deleting the temporary files causes the result to lose the file it is referring to.
	# Move values from disk to memory
	vals <- raster::values(res)
	raster::values(res) <- vals

	#delete temporary files
	filesToRemove <- grep('saga_temp_twi', list.files(), value = TRUE)
	file.remove(filesToRemove)

	names(res) <- 'topoWetnessIndex'

	return(res)
}


