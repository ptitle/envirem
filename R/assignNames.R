##' @title Defining variable names
##'
##' @description The naming scheme for the different input variables
##' are defined via a custom environment, which only needs to be done once.
##'
##' @param tmin naming scheme for minimum temperature
##' @param tmax naming scheme for maximum temperature
##' @param tmean naming scheme for mean temperature
##' @param precip naming scheme for precipitation
##' @param solrad naming scheme for solar radiation
##' @param pet naming scheme for monthly potential evapotranspiration
##' @param reset if \code{TRUE}, then names are set to default values

##' @details 	
##' The \code{.var} environment contains the naming scheme for the input variables,
##' and this will be queried by the various functions in this R package. The user 
##' should use this function to define the names of the variables, up until the variable number,
##' and after the variable number (prefix and suffix relative to the number). This is done by
##' providing a template of the naming, and placing \code{##} where the numbers would be (1:12). 
##' For example, if your minimum temperature rasters are named as 
##' \code{worldclim_v2_LGM_ccsm4_minTemp_1_land.tif}, then you should define the following:
##' \code{"worldclim_v2_LGM_ccsm4_minTemp_##_land"} for \code{tmin}. 
##' File extensions should not be included at all (not as a suffix).
##'
##' This only needs to be done once during your R session. 
##' For any variable name, if these tags are removed, and the file extension is removed, only the 
##' variable number should remain (the month number). 
##' 
##' When using the \code{assignNames()} function, you can specify as many or as few as needed.
##'
##' Variable numbers can have zero-padding. This is handled automatically. 
##'	Therefore, \code{bio_1} or \code{bio_01} are both fine, and nothing needs to be specified. 
##' 
##' The default values are \code{tmin_}, \code{tmax_}, \code{tmean_}, \code{precip_}, 
##' \code{et_solrad_} and \code{pet_}, with no suffix. 
##' You can use the function \code{namingScheme()} to see the current assigned values.
##'
##' @examples
##' \donttest{
##'	namingScheme()
##' assignNames(precip = 'precip_##_5arcmin')	
##' assignNames(solrad = 'solar_##', tmin = 'minTemp##')
##' namingScheme()
##'
##' # set back to default
##' assignNames(reset = TRUE)
##' }
##' @export

assignNames <- function(tmin, tmax, tmean, precip, solrad, pet, reset) {
	
	# if (methods::hasArg(bio)) {
		# if (!grepl('##', bio)) {
			# stop('The bioclim tag must contain ## to represent the numbers 1:19.')
		# }
		# prefix <- strsplit(bio, split = "##")[[1]][1]
		# suffix <- strsplit(bio, split = "##")[[1]][2]
		# if (is.na(prefix)) prefix <- ''
		# if (is.na(suffix)) suffix <- ''
		# assign('bio', prefix, envir = .var)
		# assign('bio_post', suffix, envir = .var)
	# }

	if (methods::hasArg(tmin)) {
		if (!grepl('##', tmin)) {
			stop('The tmin tag must contain ## to represent the numbers 1:12.')
		}
		prefix <- strsplit(tmin, split = "##")[[1]][1]
		suffix <- strsplit(tmin, split = "##")[[1]][2]
		if (is.na(prefix)) prefix <- ''
		if (is.na(suffix)) suffix <- ''
		assign('tmin', prefix, envir = .var)
		assign('tmin_post', suffix, envir = .var)
	}

	if (methods::hasArg(tmax)) {
		if (!grepl('##', tmax)) {
			stop('The tmax tag must contain ## to represent the numbers 1:12.')
		}
		prefix <- strsplit(tmax, split = "##")[[1]][1]
		suffix <- strsplit(tmax, split = "##")[[1]][2]
		if (is.na(prefix)) prefix <- ''
		if (is.na(suffix)) suffix <- ''
		assign('tmax', prefix, envir = .var)
		assign('tmax_post', suffix, envir = .var)
	}

	if (methods::hasArg(tmean)) {
		if (!grepl('##', tmean)) {
			stop('The tmean tag must contain ## to represent the numbers 1:12.')
		}
		prefix <- strsplit(tmean, split = "##")[[1]][1]
		suffix <- strsplit(tmean, split = "##")[[1]][2]
		if (is.na(prefix)) prefix <- ''
		if (is.na(suffix)) suffix <- ''
		assign('tmean', prefix, envir = .var)
		assign('tmean_post', suffix, envir = .var)
	}

	if (methods::hasArg(precip)) {
		if (!grepl('##', precip)) {
			stop('The precip tag must contain ## to represent the numbers 1:12.')
		}
		prefix <- strsplit(precip, split = "##")[[1]][1]
		suffix <- strsplit(precip, split = "##")[[1]][2]
		if (is.na(prefix)) prefix <- ''
		if (is.na(suffix)) suffix <- ''
		assign('precip', prefix, envir = .var)
		assign('precip_post', suffix, envir = .var)
	}
	
	if (methods::hasArg(solrad)) {
		if (!grepl('##', solrad)) {
			stop('The solrad tag must contain ## to represent the numbers 1:12.')
		}
		prefix <- strsplit(solrad, split = "##")[[1]][1]
		suffix <- strsplit(solrad, split = "##")[[1]][2]
		if (is.na(prefix)) prefix <- ''
		if (is.na(suffix)) suffix <- ''
		assign('solrad', prefix, envir = .var)
		assign('solrad_post', suffix, envir = .var)
	}

	if (methods::hasArg(pet)) {
		if (!grepl('##', pet)) {
			stop('The PET tag must contain ## to represent the numbers 1:12.')
		}
		prefix <- strsplit(pet, split = "##")[[1]][1]
		suffix <- strsplit(pet, split = "##")[[1]][2]
		if (is.na(prefix)) prefix <- ''
		if (is.na(suffix)) suffix <- ''
		assign('pet', prefix, envir = .var)
		assign('pet_post', suffix, envir = .var)
	}
	
	if (methods::hasArg(reset)) {
		assign('tmin', 'tmin_', envir = .var)
		assign('tmax', 'tmax_', envir = .var)
		assign('tmean', 'tmean_', envir = .var)
		assign('precip', 'precip_', envir = .var)
		assign('solrad', 'et_solrad_', envir = .var)
		assign('pet', 'PET_', envir = .var)
		assign('tmin_post', '', envir = .var)
		assign('tmax_post', '', envir = .var)
		assign('tmean_post', '', envir = .var)
		assign('precip_post', '', envir = .var)
		assign('solrad_post', '', envir = .var)
		assign('pet_post', '', envir = .var)
	}
}





