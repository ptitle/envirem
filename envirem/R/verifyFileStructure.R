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
##'	19 bioclimatic variables named as bio_1.tif
##'		
##'	12 precipitation rasters named as prec_1.tif
##'	
##'	12 min temperature rasters named as tmin_1.tif
##'	
##'	12 max temperature rasters named as tmax_1.tif
##'
##' 12 mean temperature rasters named as tmean_1.tif [optional]
##'	
##'	12 solar radiation rasters named as et_solrad_1.tif
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
### tmean not used because not available for non-current

verifyFileStructure <- function(path, returnFileNames = TRUE, rasterExt = '.tif') {
	files <- list.files(path = path, pattern = paste0(rasterExt, '$'))
	
	#check bioclim
	bioclimFiles <- grep('bio_\\d\\d?', files, value = TRUE)
	bioclimFiles <- gsub('(bio_\\d\\d?)(\\.\\w+$)', '\\1', bioclimFiles)
	if (all(paste0('bio_', 1:19) %in% bioclimFiles) & length(bioclimFiles) == 19) {
		bioclimCheck <- TRUE
	} else {
		bioclimCheck <- FALSE
	}
	
	#check precip
	precipFiles <- grep('prec_\\d\\d?', files, value = TRUE)
	precipFiles <- gsub('(prec_\\d\\d?)(\\.\\w+$)', '\\1', precipFiles)
	if (all(paste0('prec_', 1:12) %in% precipFiles) & length(precipFiles) == 12) {
		precipCheck <- TRUE
	} else {
		precipCheck <- FALSE
	}

	#check tmin
	tminFiles <- grep('tmin_\\d\\d?', files, value = TRUE)
	tminFiles <- gsub('(tmin_\\d\\d?)(\\.\\w+$)', '\\1', tminFiles)
	if (all(paste0('tmin_', 1:12) %in% tminFiles) & length(tminFiles) == 12) {
		tminCheck <- TRUE
	} else {
		tminCheck <- FALSE
	}

	#check tmax
	tmaxFiles <- grep('tmax_\\d\\d?', files, value = TRUE)
	tmaxFiles <- gsub('(tmax_\\d\\d?)(\\.\\w+$)', '\\1', tmaxFiles)
	if (all(paste0('tmax_', 1:12) %in% tmaxFiles) & length(tmaxFiles) == 12) {
		tmaxCheck <- TRUE
	} else {
		tmaxCheck <- FALSE
	}

	#check tmean
	tmeanFiles <- grep('tmean_\\d\\d?', files, value = TRUE)
	tmeanFiles <- gsub('(tmean_\\d\\d?)(\\.\\w+$)', '\\1', tmeanFiles)
	if (all(paste0('tmean_', 1:12) %in% tmeanFiles) & length(tmeanFiles) == 12) {
		tmeanCheck <- TRUE
	} else {
		tmeanCheck <- FALSE
	}

	#check solrad
	solradFiles <- grep('et_solrad_\\d\\d?', files, value = TRUE)
	solradFiles <- gsub('(et_solrad_\\d\\d?)(\\.\\w+$)', '\\1', solradFiles)
	if (all(paste0('et_solrad_', 1:12) %in% solradFiles) & length(solradFiles) == 12) {
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
