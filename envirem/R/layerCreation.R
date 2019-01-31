##' @title Creates all layers
##'
##' @description Generates all rasterLayers for one particular input dataset.
##' For the distinction between this function and \code{\link{generateRasters}}, 
##' see \code{Details}. 
##'
##' @param masterstack rasterStack containing all precipitation, 
##' min temperature, max temperature, (mean temperature) and 
##' bioclimatic variables
##'
##' @param solradstack rasterStack of monthly solar radiation
##'
##' @param var vector of names of variables to generate, see Details.
##'
##'	@param tempScale integer; scaling factor for the temperature data, see \link{envirem} for 
##' 	additional details. 
##' 
##' @details The function \code{\link{verifyFileStructure}} should be used to 
##' verify that the appropriate rasters are present in \code{masterstack}.
##' 
##' This function is called internally by \code{\link{generateRasters}}. 
##' 
##' The function \code{layerCreation} will generate envirem rasters from input R 
##' objects (rasterStacks) and will return the result as an R object. In contrast, 
##' the function \code{\link{generateRasters}} reads in input rasters from a specified directory, 
##' splits input rasters into tiles if necessary, internally calls 
##' \code{layerCreation} and writes the result to file. 
##' 
##'	Possible variables to generate include:\cr
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
##'	If \code{var = 'all'}, then all of the variables will be generated.
##'
##'
##' @return rasterStack
##'
##' @author Pascal Title
##'
##' @seealso This function is called internally by \code{\link{generateRasters}}.
##'
##' @examples
##' \donttest{
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##'
##' # create stack of temperature and precipitation rasters
##' # and stack of solar radiation rasters
##' solradFiles <- grep('solrad', rasterFiles, value=TRUE)
##' worldclim <- stack(setdiff(rasterFiles, solradFiles))
##' solar <- stack(solradFiles)
##'
##' # set up naming scheme - only precip is different from default
##' assignNames(precip = 'prec_##')
##' 
##' # generate all possible envirem variables
##' layerCreation(worldclim, solar, var='all', tempScale = 10)
##'
##' # set back to defaults
##' assignNames(reset = TRUE)
##' }
##' @export




# Function takes stack of precip, mintemp, maxtemp, bioclim, and a stack of solar radiation, and generates rasterstack of new variables
# var is a vector of variable names that will be generated. 

layerCreation <- function(masterstack, solradstack, var, tempScale = 1) {

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
	
	#naming checks and name standardization
	check <- verifyRasterNames(masterstack, solradstack, returnRasters = TRUE)
	solradstack <- check[[grep(.var$solrad, names(check))]]
	masterstack <- raster::dropLayer(check, names(solradstack))
		
	#receiving list
	reslist <- vector('list', length = length(var))
	names(reslist) <- var

	#create some separate stacks
	cat('\t\t...splitting rasterstack...\n')
	tminstack <- masterstack[[grep(.var$tmin, names(masterstack), value = TRUE)]]
	tmaxstack <- masterstack[[grep(.var$tmax, names(masterstack), value = TRUE)]]
	precipstack <- masterstack[[grep(.var$precip, names(masterstack), value = TRUE)]]
	
	#enforce ordering
	tminstack <- tminstack[[order(as.numeric(gsub(paste0(.var$tmin, '([0-9]+)', .var$tmin_post), "\\1", names(tminstack))))]]
	tmaxstack <- tmaxstack[[order(as.numeric(gsub(paste0(.var$tmax, '([0-9]+)', .var$tmax_post), "\\1", names(tmaxstack))))]]
	precipstack <- precipstack[[order(as.numeric(gsub(paste0(.var$precip, '([0-9]+)', .var$precip_post), "\\1", names(precipstack))))]]
	solradstack <- solradstack[[order(as.numeric(gsub(paste0(.var$solrad, '([0-9]+)', .var$solrad_post), "\\1", names(solradstack))))]]
	
	# adjust temperature rasters to degrees C
	if (tempScale != 1) {
		tminstack <- tminstack / tempScale
		tmaxstack <- tmaxstack / tempScale
	
		# bioclim 1,2,4,5,6,7,8,9,10,11 are affected by tempScale
		for (i in c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11)) {
			masterstack[[grep(paste0(.var$bio, sprintf("%02d", i), .var$bio_post), names(masterstack))]] <- masterstack[[grep(paste0(.var$bio, sprintf("%02d", i), .var$bio_post), names(masterstack))]] / tempScale
		}	
	}
	
	# if tmean not already present in stack, then calculate it from tmin and tmax
	if (!any(grepl(.var$tmean, names(masterstack)))) {
		cat('\t\t...calculating mean temp...\n')
		tmeanstack <- (tmaxstack + tminstack) / 2 #new mean
		names(tmeanstack) <- gsub(.var$tmax, .var$tmean, names(tmaxstack))
	} else {
		tmeanstack <- masterstack[[grep(.var$tmean, names(masterstack), value = TRUE)]]
		tmeanstack <- tmeanstack[[order(as.numeric(gsub(paste0(.var$tmean, '([0-9]+)', .var$tmean_post), "\\1", names(tmeanstack))))]]
		if (tempScale != 1) {
			tmeanstack <- tmeanstack / tempScale
		}
	}
			
	if (any(c('minTempWarmest','maxTempColdest','thermicityIndex','continentality') %in% var)) {
		cat('\t\t...temp extremes...\n')
		tempExtremes <- otherTempExtremes(tmeanstack, tminstack, tmaxstack)
		if ('minTempWarmest' %in% var) {
			reslist[['minTempWarmest']] <- tempExtremes[['minTempWarmest']]
		}
		if ('maxTempColdest' %in% var) {
			reslist[['maxTempColdest']] <- tempExtremes[['maxTempColdest']]
		}
	}

	#growing degree days with temp base of 5 deg C and 0 deg C
	if (any(c('growingDegDays0','growingDegDays5') %in% var)) {
		cat('\t\t...growing degree days...\n')
		if ('growingDegDays0' %in% var) {
			growing0deg <- growingDegDays(tmeanstack, baseTemp = 0)
			reslist[['growingDegDays0']] <- growing0deg
		}
		if ('growingDegDays5' %in% var) {
			growing5deg <- growingDegDays(tmeanstack, baseTemp = 5)
			reslist[['growingDegDays5']] <- growing5deg
		}
	}

	#number of months with mean temp above 10 deg C
	if ('monthCountByTemp10' %in% var) {
		cat('\t\t...month count by deg...\n')
		monthCount10deg <- monthCountByTemp(tmeanstack, minTemp = 10)
		reslist[['monthCountByTemp10']] <- monthCount10deg
	}
	
	#continentality index
	if (any(c('continentality', 'thermicityIndex') %in% var)) {
		cat('\t\t...continentality index...\n')
		ci <- continentality(tmax = tempExtremes[[4]], tmin = tempExtremes[[3]])
		reslist[['continentality']] <- ci
	}
	
	#Compensated Thermicity Index
	if ('thermicityIndex' %in% var) {
		cat('\t\t...thermicity index...\n')
		thermInd <- thermicityIndex(annualTemp=masterstack[[grep(paste0(.var$bio, '01', .var$bio_post), names(masterstack))]], minTemp=masterstack[[grep(paste0(.var$bio, '06', .var$bio_post), names(masterstack))]], maxTemp=tempExtremes[[1]], continentality = ci)
		reslist[['thermicityIndex']] <- thermInd
	}

	#Emberger's pluviothermic quotient
	if ('embergerQ' %in% var) {
		cat("\t\t...emberger's Q...\n")
		emberger <- embergerQ(masterstack[[grep(paste0(.var$bio, '12', .var$bio_post), names(masterstack))]], masterstack[[grep(paste0(.var$bio, '05', .var$bio_post), names(masterstack))]], masterstack[[grep(paste0(.var$bio, '06', .var$bio_post), names(masterstack))]])
		reslist[['embergerQ']] <- emberger
	}

	#annual potential evapotranspiration
	if (any(c('annualPET','PETseasonality','aridityIndexThornthwaite','climaticMoistureIndex','PETColdestQuarter','PETWarmestQuarter','PETWettestQuarter','PETDriestQuarter') %in% var)) {
		monthPET <- monthlyPET(Tmean = tmeanstack, RA = solradstack, TD = abs(tmaxstack - tminstack))
	}

	if (any(c('PETColdestQuarter', 'PETWarmestQuarter', 'PETWettestQuarter', 'PETDriestQuarter') %in% var)) {
		cat('\t\t...PET extremes...\n')
		PETextremes <- petExtremes(monthPET, precipstack, tmeanstack)
		if ('PETColdestQuarter' %in% var) {
			reslist[['PETColdestQuarter']] <- PETextremes[[1]]
		}
		if ('PETWarmestQuarter' %in% var) {
			reslist[['PETWarmestQuarter']] <- PETextremes[[2]]
		}
		if ('PETWettestQuarter' %in% var) {
			reslist[['PETWettestQuarter']] <- PETextremes[[3]]
		}
		if ('PETDriestQuarter' %in% var) {
			reslist[['PETDriestQuarter']] <- PETextremes[[4]]
		}
	}

	#annualPET
	if (any(c('annualPET','climaticMoistureIndex') %in% var)) {
		cat('\t\t...annual PET...\n')
		annualPET <- sum(monthPET)
		reslist[['annualPET']] <- annualPET
	}

	#PET seasonality
	if ('PETseasonality' %in% var) {
		cat('\t\t...PET seasonality...\n')
		seasonalityPET <- PETseasonality(monthPET)
		reslist[['PETseasonality']] <- seasonalityPET
	}

	#climatic moisture index
	if ('climaticMoistureIndex' %in% var) {
		cat('\t\t...climatic moisture index...\n')
		cmi <- climaticMoistureIndex(masterstack[[grep(paste0(.var$bio, '12', .var$bio_post), names(masterstack))]], annualPET)
		reslist[['climaticMoistureIndex']] <- cmi
	}
	
	#Thornthwaite aridity index
	if ('aridityIndexThornthwaite' %in% var) {
		cat('\t\t...Thornthwaite aridity index...\n')
		aridIndThorn <- aridityIndexThornthwaite(precipstack, monthPET)
		reslist[['aridityIndexThornthwaite']] <- aridIndThorn
	}

	# if minTempWarmest or maxTempColdest were requested, put them back on the same
	# scale as the input temperature rasters
	if ('minTempWarmest' %in% var) {
		reslist[['minTempWarmest']] <- reslist[['minTempWarmest']] * tempScale
	}

	if ('maxTempColdest' %in% var) {
		reslist[['maxTempColdest']] <- reslist[['maxTempColdest']] * tempScale
	}


	reslist <- raster::stack(reslist)
	return(reslist)
}
