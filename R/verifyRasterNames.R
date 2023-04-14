##' @title Verify Raster Names
##'
##' @description Given a RasterStack, this function will verify the
##' naming scheme and check that all required rasters are present.
##'
##' @param masterstack rasterStack containing all precipitation, 
##' min temperature, max temperature, and (optionally) mean temperature variables. 
##'
##' @param solradstack rasterStack of monthly solar radiation
##'
##' @param returnRasters if \code{FALSE}, the function checks names
##' and reports back. If \code{TRUE}, a RasterStack is returned with
##' standardized names.
##'
##'
##' @details
##' This function checks that the following are present:
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
##' The function can test the temp/precip rasterstack
##' and/or the solar radiation rasterstack separately, or simultaneously.
##'
##'
##' @return Prints messages to the console if \code{returnRasters = FALSE}, 
##'	If \code{returnRasters = TRUE}, then a RasterStack is returned. This RasterStack
##' will not include rasters that were deemed unnecessary. 
##'
##' @author Pascal Title
##'
##' @examples
##'	# Find example rasters
##'	rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##'
##'	# create stack of temperature and precipitation rasters
##'	# and stack of solar radiation rasters
##'	solradFiles <- grep('solrad', rasterFiles, value=TRUE)
##'	worldclim <- stack(setdiff(rasterFiles, solradFiles))
##'	solar <- stack(solradFiles)
##'
##'	# modify naming
##'	names(worldclim) <- gsub('tmin_', 'minTemp', names(worldclim))
##' names(worldclim) <- paste0(names(worldclim), '_v1.0')
##'	names(solar) <- gsub('et_solrad_', 'solar_', names(solar))
##'
##'	# but don't specify this change
##'	varnames()
##'
##'	# Run check
##'	verifyRasterNames(masterstack = worldclim, solradstack = solar, returnRasters = FALSE)
##' 
##'	# But if we specify our naming scheme
##' assignNames(tmin = 'minTemp##_v1.0', tmax = 'tmax_##_v1.0', tmean = 'tmean_##_v1.0', 
##' 	solrad = 'solar_##', precip = 'prec_##_v1.0')
##' varnames()
##' 
##'	verifyRasterNames(masterstack = worldclim, solradstack = solar, returnRasters = FALSE)
##'
##' # set back to defaults
##' assignNames(reset = TRUE)
##'
##' @export


verifyRasterNames <- function(masterstack = NULL, solradstack = NULL, returnRasters = FALSE) {
	
	if (is.null(masterstack) & is.null(solradstack)) {
		stop('Either masterstack or solradstack must be provided.')
	}
	
	problem <- FALSE
	
	if (!is.null(masterstack)) {
		#naming checks	
		if (any(grepl(.var$tmean, names(masterstack)))) {
			newnameNums <- gsub(paste0('(', .var$tmin, '|', .var$tmax, '|', .var$tmean, '|', .var$precip, ')', '([0-9]+)', '(', .var$tmin_post, '|', .var$tmax_post, '|', .var$tmean_post, '|', .var$precip_post, '$)'), "\\2", names(masterstack))
		} else {
			newnameNums <- gsub(paste0('(', .var$tmin, '|', .var$tmax, '|', .var$precip, ')', '([0-9]+)', '(', .var$tmin_post, '|', .var$tmax_post, '|', .var$precip_post, '$)'), "\\2", names(masterstack))
		}
		
		for (i in 1:9) {
			ind <- which(newnameNums == as.character(i))
			if (length(ind) > 0) {
				reg <- paste0('(', .var$tmin, '|', .var$tmax, '|', .var$tmean, '|', .var$precip, ')', '([0-9]+)', '(', .var$tmin_post, '|', .var$tmax_post, '|', .var$tmean_post, '|', .var$precip_post, '$)')
				tag <- gsub(reg, "\\1", names(masterstack)[ind])
				numTag <- gsub(reg, "\\2",names(masterstack)[ind])
				postTag <- gsub(reg, "\\3", names(masterstack)[ind])
				names(masterstack)[ind] <- paste0(tag, gsub(as.character(i), sprintf("%02d", i), numTag), postTag)
			}
		}
		
		# are all variables accounted for?
		expectednames <- list(tmin = paste0(.var$tmin, sprintf("%02d", 1:12), .var$tmin_post), tmax = paste0(.var$tmax, sprintf("%02d", 1:12), .var$tmax_post), precip = paste0(.var$precip, sprintf("%02d", 1:12), .var$precip_post))
		if (any(grepl(.var$tmean, names(masterstack)))) {
			expectednames[[5]] <- paste0(.var$tmean, sprintf("%02d", 1:12), .var$tmean_post)
			names(expectednames)[5] <- 'tmean'
		}		
		
		if (!identical(sort(as.character(unlist(expectednames))), sort(names(masterstack)))) {
			
			if (all(unlist(expectednames) %in% names(masterstack))) {
				extraVar <- setdiff(names(masterstack), unlist(expectednames))
				masterstack <- raster::dropLayer(masterstack, extraVar)
				message('\tIn masterstack, ignoring the following rasters:', paste(extraVar, collapse = ', '))
		
			} else {
				
				missingVar <- c()
				problem <- TRUE
				if (!all(expectednames$tmin %in% names(masterstack))) {
					missingVar <- c(missingVar, setdiff(expectednames$tmin, names(masterstack)))
				}
				
				if (!all(expectednames$tmax %in% names(masterstack))) {
					missingVar <- c(missingVar, setdiff(expectednames$tmax, names(masterstack)))
				}
				
				if (!all(expectednames$precip %in% names(masterstack))) {
					missingVar <- c(missingVar, setdiff(expectednames$precip, names(masterstack)))
				}
				
				if ('tmean' %in% names(expectednames)) {
					if (!all(expectednames$tmean %in% names(masterstack))) {
						missingVar <- c(missingVar, setdiff(expectednames$tmean, names(masterstack)))
					}
				}
				
				if (returnRasters) {
					stop('The following rasters are missing or are not recognized: ', paste(missingVar, collapse = ', '), '\n\tEnsure that you have defined the proper naming scheme. See ?assignNames.\n')
				} else {
					message('\tThe following rasters are missing or are not recognized: ', paste(missingVar, collapse = ', '), '\n\tEnsure that you have defined the proper naming scheme. See ?assignNames.')
				}
			}			
		}		
	}

	if (!is.null(solradstack)) {
		# now solrad
		reg <- paste0('(', .var$solrad, ')', '([0-9]+)', '(', .var$solrad_post, '$)')
		newnameNums <- gsub(reg, "\\2", names(solradstack))
		for (i in 1:9) {
			ind <- which(newnameNums == as.character(i))
			if (length(ind) > 0) {
				numTag <- gsub(reg, "\\2",names(solradstack)[ind])
				names(solradstack)[ind] <- paste0(.var$solrad, gsub(as.character(i), sprintf("%02d", i), numTag), .var$solrad_post)
			}
		}
		
		expectedSolRad <- paste0(.var$solrad, sprintf("%02d", 1:12), .var$solrad_post)
		if (!identical(sort(expectedSolRad), sort(names(solradstack)))) {
			problem <- TRUE
			missingSolRad <- setdiff(expectedSolRad, names(solradstack))
			
			extraVar <- setdiff(names(solradstack), expectedSolRad)
			if (length(extraVar) > 0) {
				solradstack <- raster::dropLayer(solradstack, extraVar)
				message('\tIn solradstack, ignoring the following rasters:', paste(extraVar, collapse = ', '))
			}	
			
			if (returnRasters) {
				stop('solradstack must have 12 monthly variables. Ensure that you have defined the proper naming scheme. \n\tSee ?assignNames.\n')
			 } else {
				message('\tsolradstack must have 12 monthly variables. Ensure that you have defined the proper naming scheme. \n\tSee ?assignNames.')
			}
		}
	}

	if (returnRasters) {
		if (!is.null(masterstack) & !is.null(solradstack)) {
			return(raster::stack(masterstack, solradstack))	
		} else if (!is.null(masterstack) & is.null(solradstack)) {
			return(masterstack)
		} else if (is.null(masterstack) & !is.null(solradstack)) {
			return(solradstack)
		}
	} else {
		if (!problem) {
			message('\t\tNames appear to be correct!')
		}
	}
}





