##' @title Execute Layer Creation
##'
##' @description Main function to generate specified ENVIREM layers.
##' If requested, this function will split input rasters into tiles, generate desired variables,
##' and reassemble the results. 
##' For the distinction between this function and \code{\link{layerCreation}}, 
##' see \code{Details}. 
##'
##' @param var a vector of variable names to generate, see Details.
##'
##' @param maindir path to directory of input rasters
##'
##' @param prefix prefix to append to output filename
##'
##' @param outputDir output directory.
##'
##' @param rasterExt the file extension of the input rasters
##'
##' @param nTiles the number of tiles to split the rasters when 
##' tiling is requested, must be a perfect square
##'
##'	@param tempScale integer; scaling factor for the temperature data, see \link{envirem} for 
##' 	additional details. 
##'
##'	@param precipScale integer; scaling factor for the precipitation data, see \link{envirem}
##' 	for additional details. 
##'
##' @param overwriteResults logical, should existing rasters be overwritten
##'
##' @param outputFormat output format for rasters, see \code{\link{writeRaster}} for options
##'
##' @param tempDir temporary directory for raster tiles that will be created and then removed
##'
##' @param gdalinfoPath path to gdalinfo binary, leave as \code{NULL} if it is in the default search path.
##'
##' @param gdal_translatePath path to gdal_translate binary, leave as \code{NULL} if it is in the default search path.
##'
##' @param useCompression logical; should compression options be used to achieve smaller file sizes.
##' 	Only pertains to format GTiff.
##'
##'
##' @details 
##' 
##' The function \code{\link{layerCreation}} will generate envirem rasters from input R 
##' objects (rasterStacks) and will return the result as an R object. In contrast, 
##' the function \code{generateRasters} reads in input rasters from a specified directory, 
##' splits input rasters into tiles if necessary, internally calls 
##' \code{\link{layerCreation}} and writes the result to file. 
##'
##' Possible variables to generate include:\cr
##' \cr
##' annualPET \cr
##' aridityIndexThornthwaite \cr
##' climaticMoistureIndex \cr
##' continentality \cr
##' embergerQ \cr
##' growingDegDays0 \cr
##' growingDegDays5 \cr
##' maxTempColdest \cr
##' minTempWarmest \cr
##' meanTempColdest \cr
##' meanTempWarmest \cr
##' monthCountByTemp10 \cr
##' PETColdestQuarter \cr
##' PETDriestQuarter \cr
##' PETseasonality \cr
##' PETWarmestQuarter \cr
##' PETWettestQuarter \cr
##' thermicityIndex \cr
##' 
##' If \code{var = 'all'}, then all of the variables will be generated.
##' 
##' 
##' Rasters in \code{mainDir} should be named appropriately (see \code{\link{verifyFileStructure}})
##' and with identical resolution, origin and extent. 
##'
##' Output rasters are written with the most appropriate \code{\link{dataType}}, as 
##' inferred with \code{\link{dataTypeCheck}}. This will reduce the file size of these rasters. 
##'
##' If the goal is to use these rasters with the standalone Maxent program, we recommend 
##' \code{outputFormat = 'EHdr'}.
##' 
##' \strong{IMPORTANT}: Temporary files can quickly fill up your hard drive when working with
##' 	large rasters. There are two temporary directories to consider for this function: The
##'		\code{tempDir} directory defined as an argument in this function is used for storing
##' 	intermediate files when splitting rasters into tiles (and is ignored if \code{nTiles = 1}).
##'		The raster package will use another directory for storing temporary rasters. This can be 
##'		can be viewed with \code{rasterOptions()}, and can be set with 
##' 	\code{rasterOptions(tmpdir = 'path-to-dir')}. Be sure that this is pointing to a directory
##'		with plenty of available space. Both temporary directories are automatically cleared.
##'
##' @return The requested set of rasterLayers will be written to \code{outputDir}.
##'
##' @author Pascal Title
##'
##' @seealso Naming of rasters in inputDir will be checked with \code{\link{verifyFileStructure}}.
##'
##' 
##'
##'
##' @export




generateRasters <- function(var, maindir, prefix = '', outputDir = './', rasterExt = '.tif', nTiles = 1, tempScale = 1, precipScale = 1, overwriteResults = TRUE, outputFormat = 'GTiff', tempDir = '~/temp', gdalinfoPath = NULL, gdal_translatePath = NULL, useCompression = TRUE) {

	allvar <- c("annualPET", "aridityIndexThornthwaite", "climaticMoistureIndex", "continentality", "embergerQ", "growingDegDays0", "growingDegDays5", "maxTempColdest", "minTempWarmest", "meanTempColdest", "meanTempWarmest", "monthCountByTemp10", "PETColdestQuarter", "PETDriestQuarter", "PETseasonality", "PETWarmestQuarter", "PETWettestQuarter", "thermicityIndex")

	if (class(var) == 'character') {
		if (length(var) == 1) {
			if (var == 'all') {
				var <- allvar
			}
		}
	}

	varRecognized <- var %in% allvar
	if (!all(varRecognized == TRUE)) {
		badvar <- which(varRecognized == FALSE)
		message('The following variable names were not recognized:')
		for (i in 1:length(badvar)) {
			message('\t', badvar[i])
		}
		stop('\nVariable names must match official set.')
	}
	
	solradVar <- c('annualPET','PETseasonality','aridityIndexThornthwaite','climaticMoistureIndex','PETColdestQuarter','PETWarmestQuarter','PETWettestQuarter','PETDriestQuarter')
	needsSolRad <- ifelse(any(var %in% solradVar), TRUE, FALSE)

	
	# check results directory
	outputDir <- gsub('/?$', '/', outputDir)

	if (!dir.exists(outputDir)) {
		stop('\noutputDir does not exist.')
	}
	
	#check nTiles
	if (nTiles > 1) {
		#number of tiles must be a perfect square
		if (sqrt(nTiles) != round(sqrt(nTiles), 0)) {
			stop('Number of tiles must be a perfect square.')
		}
	}

	# check that temp directory, if needed, does not conflict
	# If it does, append a random number code
	if (nTiles > 1) {
		tempDir <- gsub('/$', '', tempDir)
		while (dir.exists(tempDir)) {
			tempDir <- paste0(tempDir, sample(1:9, 1))
		}
	}

	outputExt <- c(raster = '.grd', ascii = '.asc', SAGA = '.sdat', IDRISI = '.rst', CDF = '.nc', GTiff = '.tif', ENVI = '.envi', EHdr = '.bil', HFA = '.img')
	outputExt <- outputExt[outputFormat]
	outputFiles <- paste0(outputDir, prefix, var, outputExt)
	if (any(basename(outputFiles) %in% list.files(outputDir)) & overwriteResults == FALSE) {
		stop('Found files in outputDir with same names as new files. Either remove them manually, or set overwriteResults to TRUE.')
	}
	
	if (useCompression & outputFormat == 'GTiff') {
		tifOptions <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")
	} else {
		tifOptions <- NULL
	}

	if (nTiles == 1) {
		# no tiling

		#load rasters
		if (needsSolRad) {
			files <- verifyFileStructure(path = maindir, returnFileNames = TRUE, includeSolRad = TRUE, rasterExt = rasterExt)
			if (all(is.null(files))) stop('Something wrong with input files.')
			clim <- raster::stack(files)
	
			#pull out solar radiation rasters and create new stack
			solrad <- clim[[grep(paste0(.var$solrad, '\\d\\d?', .var$solrad_post), names(clim))]]	
			clim <- raster::dropLayer(clim, names(solrad))
		} else {
			files <- verifyFileStructure(path = maindir, returnFileNames = TRUE, includeSolRad = FALSE, rasterExt = rasterExt)
			if (all(is.null(files))) stop('Something wrong with input files.')
			clim <- raster::stack(files)
			solrad <- NULL
		}
		
		res <- layerCreation(masterstack = clim, solradstack = solrad, var = var, tempScale = tempScale, precipScale = precipScale)
		
		# write to disk
		for (i in 1:raster::nlayers(res)) {
			outputName <- paste0(outputDir, prefix, names(res)[i])
			if (outputFormat == 'EHdr') {
				outputName <- paste0(outputName, '.bil')
			}
			
			dtype <- dataTypeCheck(res[[i]])[[2]]
						
			raster::writeRaster(res[[i]], outputName, overwrite = overwriteResults, format = outputFormat, datatype = dtype, NAflag = -9999, options = tifOptions)
		}

		# # write to disk
		# outputName <- paste(timeName, resName, sep = '_')
		# outputName <- paste0(outputDir, outputName)
		# raster::writeRaster(res, outputName, overwrite = overwriteResults, format = outputFormat, bylayer = TRUE, suffix = 'names')

	} else if (nTiles > 1) {

		s <- sqrt(nTiles)

		#create temp directory
		dir.create(tempDir)

		#take each raster and generate tiles, s per raster (ending with tile1, tile2, tile3, tile4)
		if (needsSolRad) {
			files <- verifyFileStructure(path = maindir, returnFileNames = TRUE, includeSolRad = TRUE, rasterExt = rasterExt)
		} else {
			files <- verifyFileStructure(path = maindir, returnFileNames = TRUE, includeSolRad = FALSE, rasterExt = rasterExt)
		}
		if (all(is.null(files))) stop('Something wrong with input files.')
				
		message('\tSplitting rasters into tiles...')
		
		for (i in 1:length(files)) {
			split_raster(files[i], s = s, outputDir = tempDir, gdalinfoPath = gdalinfoPath, gdal_translatePath = gdal_translatePath)
		}

		#create temporary results directory
		dir.create(paste0(tempDir, '/res'))

		for (i in 1:nTiles) {
			tilename <- paste('_tile', i, sep='')
			message('\t', tilename)

			#load rasters
			clim <- raster::stack(list.files(path = tempDir, pattern = paste(tilename, '.tif$', sep=''), full.names = TRUE))
			names(clim) <- gsub(tilename, '', names(clim))

			if (needsSolRad) {
				#pull out solar radiation rasters and create new stack
				solrad <- clim[[grep(.var$solrad, names(clim))]]
				clim <- raster::dropLayer(clim, grep(.var$solrad, names(clim)))
			} else {
				solrad <- NULL
			}
			
			res <- layerCreation(masterstack = clim, solradstack = solrad, var = var, tempScale = tempScale, precipScale = precipScale)
			names(res) <- paste0(names(res), tilename)

			# write to disk
			raster::writeRaster(res, paste0(tempDir, '/res/temp'), overwrite = TRUE, format = 'GTiff', NAflag = -9999, bylayer = TRUE, suffix = 'names', options = tifOptions)
		
			#delete tile-specific temp files
			toRemove <- list.files(path = tempDir, pattern = paste0(tilename, '\\.tif$'), full.names = TRUE)
			file.remove(toRemove)
	
			#delete raster-package generated tmp files
			raster::removeTmpFiles(h = 0)
			gc()
		}

		# Combine tiles
		message('\n\tPutting tiles back together...')
		resRasters <- list.files(path = paste0(tempDir, '/res/'), pattern='\\.tif$')
		resRasters <- unique(gsub("_tile\\d\\d?", "", resRasters))

		for (i in 1:length(resRasters)) {
			message('\t\tTiles being combined for ', gsub('temp_', '', resRasters[i]), '...')
			files <- list.files(path = paste0(tempDir, '/res/'), pattern = '\\.tif$', full.names = TRUE)
			files <- files[grep(gsub('\\.tif', '', resRasters[i]), files)]

			tilelist <- lapply(files, raster::raster)

			# write to disk
			fn <- paste0(outputDir, prefix, gsub('temp_', '', gsub('\\.tif', '', resRasters[i])))
			
			# determine data type
			dtype <- dataTypeCheck(tilelist[[1]])[[2]]
			
			if (outputFormat == 'EHdr') {
				fn <- paste0(fn, '.bil')
			}
			
			tilelist$fun <- mean
			tilelist$filename <- fn
			tilelist$datatype <- dtype
			tilelist$format <- outputFormat
			tilelist$NAflag <- -9999
			tilelist$overwrite <- overwriteResults
			tilelist$options <- tifOptions

			m <- do.call(raster::mosaic, tilelist)
		}

		#cleanup
		unlink(tempDir, recursive = TRUE)
		# system(paste0("rm -rf ", tempDir))
	}	
}
