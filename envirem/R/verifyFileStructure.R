##' @title Verify File Structure
##'
##' @description Ensures that the necessary files are present for other
##' functions to work properly.
##'
##' @param path path to directory of rasters
##'
##' @param returnFileNames logical, should file paths and names be returned
##'
##' @param rasterExt file extension of rasters
##'
##' @details 	
##' This function searches for the following
##' in the directory specified by \code{path}:
##'	
##'	19 bioclimatic variables
##'		
##'	12 precipitation rasters
##'	
##'	12 min temperature rasters
##'	
##'	12 max temperature rasters
##'
##' 12 mean temperature rasters [optional]
##'	
##'	12 solar radiation rasters
##'
##' The naming scheme will be checked against the one 
##' defined via the custom naming environment. See \code{link{?assignNames}}
##' for additional details.
##' 
##' An equivalent function for checking the names of rasters in R can be 
##' found at \code{\link{verifyRasterNames}}. 
##'
##' The naming of the variables is defined by default as shown above. 
##' Changes to the naming scheme can easily be made. See \code{link{?assignNames}}
##' for more information.
##'
##' If mean temperature rasters are not detected, the raster creation functions
##' will create mean temperature by taking the mean of the min and max.
##'
##' @return Prints messages to the console if problems are found. 
##'	If \code{returnFileNames == TRUE}, then a vector of filenames is returned. 
##'
##' @author Pascal Title
##'
##' @examples
##' # As there are no problems with these files, the list of files 
##' # will be returned.
##' verifyFileStructure(system.file('extdata', package='envirem'))
##' @export


# Function to verify proper file structure for main functions
## Expected file structure is:
### 19 bioclim: bio_1.tif
### 12 precip: prec_1.tif
### 12 tmin: tmin_1.tif
### 12 tmax: tmax_1.tif
### 12 solar radiation: et_solrad_1.tif
### tmean not used because may not be available for non-current

verifyFileStructure <- function(path, returnFileNames = TRUE, rasterExt = '.tif') {
	files <- list.files(path = path, pattern = paste0(rasterExt, '$'))
	
	if (!exists('.var', mode = 'environment')) {
		stop('Variable naming environment not found.')
	}
	
	#check bioclim
	xx <- paste0(.var$bio, '\\d\\d?', .var$bio_post)
	bioclimFiles <- grep(xx, files, value = TRUE)
	bioclimFiles <- gsub(paste0('(', xx, ')', '(\\.\\w+$)'), '\\1', bioclimFiles)
	bioclimFiles <- bioclimFiles[order(as.numeric(gsub(paste0('(', .var$bio, ')', '([0-9]+)', .var$bio_post), '\\2', bioclimFiles)))]
	if ((all(paste0(.var$bio, 1:19, .var$bio_post) %in% bioclimFiles) | all(paste0(.var$bio, sprintf("%02d", 1:19), .var$bio_post) %in% bioclimFiles)) & length(bioclimFiles) == 19) {
		bioclimCheck <- TRUE
	} else {
		bioclimCheck <- FALSE
	}
	
	#check precip
	xx <- paste0(.var$precip, '\\d\\d?', .var$precip_post)
	precipFiles <- grep(xx, files, value = TRUE)
	precipFiles <- gsub(paste0('(', xx, ')', '(\\.\\w+$)'), '\\1', precipFiles)
	precipFiles <- precipFiles[order(as.numeric(gsub(paste0('(', .var$precip, ')', '([0-9]+)', .var$precip_post), '\\2', precipFiles)))]
	if ((all(paste0(.var$precip, 1:12, .var$precip_post) %in% precipFiles) | all(paste0(.var$precip, sprintf("%02d", 1:12), .var$precip_post) %in% precipFiles)) & length(precipFiles) == 12) {
		precipCheck <- TRUE
	} else {
		precipCheck <- FALSE
	}

	#check tmin
	xx <- paste0(.var$tmin, '\\d\\d?', .var$tmin_post)
	tminFiles <- grep(xx, files, value = TRUE)
	tminFiles <- gsub(paste0('(', xx, ')', '(\\.\\w+$)'), '\\1', tminFiles)
	tminFiles <- tminFiles[order(as.numeric(gsub(paste0('(', .var$tmin, ')', '([0-9]+)', .var$tmin_post), '\\2', tminFiles)))]
	if ((all(paste0(.var$tmin, 1:12, .var$tmin_post) %in% tminFiles) | all(paste0(.var$tmin, sprintf("%02d", 1:12), .var$tmin_post) %in% tminFiles)) & length(tminFiles) == 12) {
		tminCheck <- TRUE
	} else {
		tminCheck <- FALSE
	}

	#check tmax
	xx <- paste0(.var$tmax, '\\d\\d?', .var$tmax_post)
	tmaxFiles <- grep(xx, files, value = TRUE)
	tmaxFiles <- gsub(paste0('(', xx, ')', '(\\.\\w+$)'), '\\1', tmaxFiles)
	tmaxFiles <- tmaxFiles[order(as.numeric(gsub(paste0('(', .var$tmax, ')', '([0-9]+)', .var$tmax_post), '\\2', tmaxFiles)))]
	if ((all(paste0(.var$tmax, 1:12, .var$tmax_post) %in% tmaxFiles) | all(paste0(.var$tmax, sprintf("%02d", 1:12), .var$tmax_post) %in% tmaxFiles)) & length(tmaxFiles) == 12) {
		tmaxCheck <- TRUE
	} else {
		tmaxCheck <- FALSE
	}

	#check tmean
	xx <- paste0(.var$tmean, '\\d\\d?', .var$tmean_post)
	tmeanFiles <- grep(xx, files, value = TRUE)
	tmeanFiles <- gsub(paste0('(', xx, ')', '(\\.\\w+$)'), '\\1', tmeanFiles)
	tmeanFiles <- tmeanFiles[order(as.numeric(gsub(paste0('(', .var$tmean, ')', '([0-9]+)', .var$tmean_post), '\\2', tmeanFiles)))]
	if ((all(paste0(.var$tmean, 1:12, .var$tmean_post) %in% tmeanFiles) | all(paste0(.var$tmean, sprintf("%02d", 1:12), .var$tmean_post) %in% tmeanFiles)) & length(tmeanFiles) == 12) {
		tmeanCheck <- TRUE
	} else {
		tmeanCheck <- FALSE
	}

	#check solrad
	xx <- paste0(.var$solrad, '\\d\\d?', .var$solrad_post)
	solradFiles <- grep(xx, files, value = TRUE)
	solradFiles <- gsub(paste0('(', xx, ')', '(\\.\\w+$)'), '\\1', solradFiles)
	solradFiles <- solradFiles[order(as.numeric(gsub(paste0('(', .var$solrad, ')', '([0-9]+)', .var$solrad_post), '\\2', solradFiles)))]
	if ((all(paste0(.var$solrad, 1:12, .var$solrad_post) %in% solradFiles) | all(paste0(.var$solrad, sprintf("%02d", 1:12), .var$solrad_post) %in% solradFiles)) & length(solradFiles) == 12) {
		solradCheck <- TRUE
	} else {
		solradCheck <- FALSE
	}
	
	if (!tmeanCheck) {
		cat('\ttmean files are not properly named or missing. Tmean will therefore be calculated.\n')
	}

	if (!all(bioclimCheck, precipCheck, tminCheck, tmaxCheck, solradCheck)) {
		if (!bioclimCheck) {
			cat('\tbioclim files are not properly named.\n')
		}
		if (!precipCheck) {
			cat('\tprecip files are not properly named.\n')
		}
		if (!tminCheck) {
			cat('\ttmin files are not properly named.\n')
		}
		if (!tmaxCheck) {
			cat('\ttmax files are not properly named.\n')
		}
		if (!solradCheck) {
			cat('\tsolrad files are not properly named.\n')
		}
	} else {
		if (returnFileNames) {
			if (tmeanCheck) {
				files <- c(bioclimFiles, precipFiles, tminFiles, tmaxFiles, tmeanFiles, solradFiles)
			} else {
				files <- c(bioclimFiles, precipFiles, tminFiles, tmaxFiles, solradFiles)
			}
			files <- paste0(gsub('/?$', '/', path), files, rasterExt)
			return(files)
		}
	}
}
