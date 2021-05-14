##' @title Split raster into tiles
##'
##' @description Splits a rasterLayer into tiles
##'
##' @param file full path to a raster
##'
##' @param s division applied to each side of the raster (s=2 -> 4 tiles)
##'
##' @param outputDir path and directory name for output
##'
##' @param gdalinfoPath path to gdalinfo binary. Set to \code{NULL} if 
##' default search path is sufficient.
##'
##' @param gdal_translatePath path to gdal_translate binary. 
##' Set to \code{NULL} if default search path is sufficient.
##'
##' @details GDAL must be installed for this function to work.
##' To determine if the default search paths are sufficient,
##' you can type in R \code{Sys.which('gdalinfo')} and \code{Sys.which('gdal_translate')}.
##' If a path is returned, then you can leave those arguments as \code{NULL}.  
##'
##' @return Rasters are written to the output directory. 
##'
##' @references
##' GDAL. 2015. GDAL - Geospatial Data Abstraction Library: Version 1.11.3, 
##' Open Source Geospatial Foundation, \url{https://gdal.org}
##'
##' @author Pascal Title
##'
##' @examples
##' \dontrun{
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' tmin1file <- grep('tmin_1\\.', rasterFiles, value=TRUE)
##' 
##' We will split this raster into 4 tiles, that will be written to disk.
##' split_raster(tmin1file, s = 2, outputDir = '~/temp/', gdalinfoPath = NULL, 
##' 		gdal_translatePath = NULL)
##' }
##' @export

#Function modified from http://thebiobucket.blogspot.com/2014/03/use-gdal-from-r-console-to-split-raster.html
split_raster <- function(file, s = 2, outputDir, gdalinfoPath = NULL, gdal_translatePath = NULL) {
	#file = file name
	#s = division applied to each side of raster (s=2 -> 4 tiles)
     
    outputDir <- gsub('/?$', '/', outputDir)
    filename <- gsub("\\.\\w+$", "", file)
    filename <- basename(filename)
    filename <- paste0(outputDir, filename)

    if (is.null(gdalinfoPath)) {
    	gdalinfo_str <- Sys.which('gdalinfo')
    	if (gdalinfo_str == '') {
    		stop('gdalinfo not found. Please provide complete path.')
    	} 
    } else {
    	gdalinfo_str <- gdalinfoPath
    }

    if (is.null(gdal_translatePath)) {
    	gdal_translate_str <- Sys.which('gdal_translate')
    	if (gdal_translate_str == '') {
    		stop('gdal_translate not found. Please provide complete path.')
    	} 
    } else {
    	gdal_translate_str <- gdal_translatePath
    }
       
    # pick size of each side
    x <- as.numeric(gsub("[^0-9]", "", unlist(strsplit(system2(command = gdalinfo_str, args = file, stdout = TRUE)[3], ", "))))[1]
    y <- as.numeric(gsub("[^0-9]", "", unlist(strsplit(system2(command = gdalinfo_str, args = file, stdout = TRUE)[3], ", "))))[2]
     
    # t is nr. of iterations per side
    t <- s - 1
    counter <- 0
    for (i in 0:t) {
        for (j in 0:t) {
        	counter <- counter + 1
            # [-srcwin xoff yoff xsize ysize] src_dataset dst_dataset
            srcwin_str <- paste("-srcwin", i * x/s, j * y/s, x/s, y/s)
            args <- paste0(srcwin_str, " ", file, " ", filename, "_tile", counter, ".tif")
            log <- system2(command = gdal_translate_str, args = args, stdout = TRUE)
        }
    }
}

