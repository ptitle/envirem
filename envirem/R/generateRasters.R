##' @title Execute Layer Creation
##'
##' @description Main function to generate specified ENVIREM layers.
##' If requested, this function will split input rasters into tiles, generate desired variables,
##' and reassemble the results. Results are named according to specified resName and timeName. 
##' For the distinction between this function and \code{\link{layerCreation}}, 
##' see \code{Details}. 
##'
##' @param var a vector of variable names to generate, see Details.
##'
##' @param maindir path to directory of input rasters
##'
##' @param resName output nametag for the resolution
##'
##' @param timeName output nametag for the time period
##'
##' @param outputDir output directory. A directory will be generated according 
##' to the resName and timeName, so this is the output location for the 
##' directory that will be generated.
##'
##' @param rasterExt the file extension of the input rasters
##'
##' @param nTiles the number of tiles to split the rasters when 
##' tiling is requested, must be a perfect square
##'
##' @param overwriteResults logical, should existing rasters be overwritten
##'
##' @param outputFormat output format for rasters, see \code{\link{writeRaster}} for options
##'
##' @param tempDir temporary directory that will be created and then removed
##'
##' @param gdalinfoPath path to gdalinfo binary, leave as \code{NULL} if it is in the default search path.
##'
##' @param gdal_translatePath path to gdal_translate binary, leave as \code{NULL} if it is in the default search path.
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
##' \code{resName} and \code{timeName} are only used for naming the output directory.
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




generateRasters <- function(var, maindir, resName, timeName, outputDir, rasterExt = '.tif', nTiles = 1, overwriteResults = TRUE, outputFormat = 'GTiff', tempDir = '~/temp', gdalinfoPath = NULL, gdal_translatePath = NULL) {

	allvar <- c("annualPET", "aridityIndexThornthwaite", "climaticMoistureIndex", "continentality", "embergerQ", "growingDegDays0", "growingDegDays5", "maxTempColdest", "minTempWarmest", "monthCountByTemp10", "PETColdestQuarter", "PETDriestQuarter", "PETseasonality", "PETWarmestQuarter", "PETWettestQuarter", "thermicityIndex")

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
		cat('The following variable names were not recognized:\n')
		for (i in 1:length(badvar)) {
			cat('\t', badvar[i], '\n')
		}
		stop('\nVariable names must match official set.')
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

	# check and create results directory
	outputDir <- gsub('/?$', '/', outputDir)

	if (!dir.exists(outputDir)) {
		dir.create(outputDir)
	}

	outputDir <- paste0(outputDir, timeName)
	if (!dir.exists(outputDir)) {
		dir.create(outputDir)
	}

	outputDir <- paste0(outputDir, '/', resName, '/')
	if (!dir.exists(outputDir)) {
		dir.create(outputDir)
	}

	if (length(list.files(outputDir, pattern='.tif$')) > 0 & overwriteResults == FALSE) {
		stop('Rasters detected in output directory. Either remove them manually, or set overwriteResults to TRUE.')
	}

	cat(toupper(timeName), '--', resName, '\n')

	if (nTiles == 1) {
		# no tiling

		#load rasters
		files <- verifyFileStructure(path = maindir, returnFileNames = TRUE, rasterExt = rasterExt)

		clim <- raster::stack(files)

		#pull out solar radiation rasters and create new stack
		solrad <- clim[[which(grepl('solrad', names(clim)) == TRUE)]]

		clim <- raster::dropLayer(clim, which(grepl('solrad', names(clim)) == TRUE))

		res <- layerCreation(masterstack = clim, solradstack = solrad, var = var)
		
		# write to disk
		for (i in 1:nlayers(res)) {
			outputName <- paste(timeName, resName, sep = '_')
			outputName <- paste0(outputDir, outputName, '_', names(res)[i])
			if (outputFormat == 'EHdr') {
				outputName <- paste0(outputName, '.bil')
			}
			dtype <- dataTypeCheck(res[[i]])[[2]]
			raster::writeRaster(res[[i]], outputName, overwrite = overwriteResults, format = outputFormat, datatype = dtype, NAflag = -9999)	
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
		files <- verifyFileStructure(path = maindir, returnFileNames = TRUE, rasterExt = rasterExt)	

		cat('\tSplitting rasters into tiles...\n')
		for (i in 1:length(files)) {
			split_raster(files[i], s = s, outputDir = tempDir, gdalinfoPath = gdalinfoPath, gdal_translatePath = gdal_translatePath)
		}

		#create temporary results directory
		dir.create(paste0(tempDir, '/res'))

		for (i in 1:nTiles) {
			tilename <- paste('_tile', i, sep='')
			cat('\t', tilename, '\n')

			#load rasters
			clim <- raster::stack(list.files(path = tempDir, pattern = paste(tilename, '.tif$', sep=''), full.names = TRUE))
			names(clim) <- gsub(tilename, '', names(clim))

			#pull out solar radiation rasters and create new stack
			solrad <- clim[[which(grepl('solrad', names(clim)) == TRUE)]]

			clim <- raster::dropLayer(clim, which(grepl('solrad', names(clim)) == TRUE))

			res <- layerCreation(masterstack = clim, solradstack = solrad, var = var)
			names(res) <- paste(names(res), tilename, sep = '')

			# write to disk
			raster::writeRaster(res, paste0(tempDir, '/res/temp'), overwrite = TRUE, format = 'GTiff', NAflag = -9999, bylayer = TRUE, suffix = 'names')
		
			#delete tile-specific temp files
			toRemove <- list.files(path = tempDir, pattern = paste0(tilename, '.tif$'), full.names = TRUE)
			file.remove(toRemove)
	
			#delete raster-package generated tmp files
			raster::removeTmpFiles(h = 0)
		}

		# Combine tiles
		cat('\n\tPutting tiles back together...\n\n')
		resRasters <- list.files(path = paste0(tempDir, '/res/'), pattern='.tif$')
		resRasters <- unique(gsub("_tile\\d\\d?", "", resRasters))

		for (i in 1:length(resRasters)) {
			cat('\tTiles being combined for', resRasters[i], '...\n')
			files <- list.files(path = paste0(tempDir, '/res/'), pattern = '.tif$', full.names = TRUE)
			files <- files[which(grepl(gsub('.tif', '', resRasters[i]), files) == TRUE)]

			tilelist <- lapply(files, raster::raster)

			# write to disk
			outputName <- paste(timeName, resName, sep = '_')
			outputName <- paste0(outputDir, outputName)

			fn <- paste0(outputName, gsub('temp', '', gsub('.tif', '', resRasters[i])))
			
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

			m <- do.call(raster::mosaic, tilelist)
		}

		#cleanup
		system(paste0("rm -rf ", tempDir))
	}	
}
