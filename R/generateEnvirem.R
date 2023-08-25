##' @title Generate ENVIREM rasters
##'
##' @description Generates rasters from an input dataset.
##'
##' @param masterstack rasterStack containing all monthly precipitation, 
##' min temperature, max temperature, and optionally mean temperature rasters.
##'
##' @param solradstack rasterStack of monthly solar radiation, can be \code{NULL} if not needed.
##'
##' @param monthPET rasterStack of monthly potential evapotranspiration. If \code{NULL}, 
##' 	will be calculated internally.
##'
##' @param var vector of names of variables to generate, see Details.
##'
##'	@param tempScale integer; scaling factor for the temperature data, see \link{envirem} for 
##' 	additional details.
##'
##'	@param precipScale integer; scaling factor for the precipitation data, see \link{envirem}
##' 	for additional details. 
##' 
##' @details The function \code{\link{verifyRasterNames}} should be used to 
##' verify that the appropriate rasters are present in \code{masterstack}.
##' 
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
##'	If \code{var = 'all'}, then all of the variables will be generated.
##'
##'
##' @return rasterStack
##'
##' @author Pascal Title
##'
##'
##' @examples
##' \donttest{
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##'
##' # create stack of temperature and precipitation rasters
##' # and stack of solar radiation rasters
##' solradFiles <- grep('solrad', rasterFiles, value=TRUE)
##' worldclim <- rast(setdiff(rasterFiles, solradFiles))
##' solar <- rast(solradFiles)
##'
##' # set up naming scheme - only precip is different from default
##' assignNames(precip = 'prec_##')
##' 
##' # generate all possible envirem variables
##' generateEnvirem(worldclim, solar, var='all', tempScale = 10)
##'
##' # set back to defaults
##' assignNames(reset = TRUE)
##' }
##' @export




# Function takes stack of precip, mintemp, maxtemp, bioclim, and a stack of solar radiation, and generates rasterstack of new variables
# var is a vector of variable names that will be generated. 

generateEnvirem <- function(masterstack, solradstack = NULL, monthPET = NULL, var, tempScale = 1, precipScale = 1) {

	allvar <- c("annualPET", "aridityIndexThornthwaite", "climaticMoistureIndex", "continentality", "embergerQ", "growingDegDays0", "growingDegDays5", "maxTempColdest", "minTempWarmest", "meanTempColdest", "meanTempWarmest", "monthCountByTemp10", "PETColdestQuarter", "PETDriestQuarter", "PETseasonality", "PETWarmestQuarter", "PETWettestQuarter", "thermicityIndex")

	if (inherits(var, 'character')) {
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
	
	#naming checks and name standardization
	if (needsSolRad) {
		check <- verifyRasterNames(masterstack, solradstack, returnRasters = TRUE)
		solradstack <- check[[grep(paste0(.var$solrad, '\\d\\d?', .var$solrad_post), names(check))]]
		masterstack <- terra::subset(check, names(solradstack), negate = TRUE)
	} else {
		check <- verifyRasterNames(masterstack, returnRasters = TRUE)
		masterstack <- check	
	}	
	
	#receiving list
	reslist <- vector('list', length = length(var))
	names(reslist) <- var

	#create some separate stacks
	message('\t\t...splitting rasterstack...')
	tminstack <- masterstack[[grep(paste0(.var$tmin, '\\d\\d?', .var$tmin_post), names(masterstack), value = TRUE)]]
	tmaxstack <- masterstack[[grep(paste0(.var$tmax, '\\d\\d?', .var$tmax_post), names(masterstack), value = TRUE)]]
	precipstack <- masterstack[[grep(paste0(.var$precip, '\\d\\d?', .var$precip_post), names(masterstack), value = TRUE)]]
	
	#enforce ordering
	tminstack <- tminstack[[order(as.numeric(gsub(paste0(.var$tmin, '([0-9]+)', .var$tmin_post), "\\1", names(tminstack))))]]
	tmaxstack <- tmaxstack[[order(as.numeric(gsub(paste0(.var$tmax, '([0-9]+)', .var$tmax_post), "\\1", names(tmaxstack))))]]
	precipstack <- precipstack[[order(as.numeric(gsub(paste0(.var$precip, '([0-9]+)', .var$precip_post), "\\1", names(precipstack))))]]
	
	if (needsSolRad) {
		solradstack <- solradstack[[order(as.numeric(gsub(paste0(.var$solrad, '([0-9]+)', .var$solrad_post), "\\1", names(solradstack))))]]
	}
	
	# adjust temperature rasters to degrees C
	if (tempScale != 1) {
		tminstack <- tminstack / tempScale
		tmaxstack <- tmaxstack / tempScale
	}
	
	# if tmean not already present in stack, then calculate it from tmin and tmax
	if (!any(grepl(paste0(.var$tmean, '\\d\\d?', .var$tmean_post), names(masterstack)))) {
		message('\t\t...calculating mean temp...')
		tmeanstack <- (tmaxstack + tminstack) / 2 #new mean
		names(tmeanstack) <- gsub(paste0('(', .var$tmax, ')(\\d\\d?)(', .var$tmax_post, ')'), paste0(.var$tmean, '\\2', .var$tmean_post), names(tmaxstack))
	} else {
		tmeanstack <- masterstack[[grep(paste0(.var$tmean, '\\d\\d?', .var$tmean_post), names(masterstack), value = TRUE)]]
		tmeanstack <- tmeanstack[[order(as.numeric(gsub(paste0(.var$tmean, '([0-9]+)', .var$tmean_post), "\\1", names(tmeanstack))))]]
		if (tempScale != 1) {
			tmeanstack <- tmeanstack / tempScale
		}
	}
	
	# if monthly PET provided, enforce ordering
	if (!is.null(monthPET)) {
		monthPET <- monthPET[[order(as.numeric(gsub(paste0(.var$pet, '([0-9]+)', .var$pet_post), "\\1", names(monthPET))))]]
	}
	
	# Bioclim variables 1, 5, 6, 12 are used for some of the ENVIREM variables.
	# Calculate those that are needed for the requested variables. 
	bioclimstack <- vector('list', length = 4)
	names(bioclimstack) <- c('bio1', 'bio5', 'bio6', 'bio12')
	message('\t\t...calculating necessary bioclim variables...')
	if ('thermicityIndex' %in% var) {
		# bio1 needed: annual mean temperature
		bioclimstack[['bio1']] <- terra::app(tmeanstack, fun = mean)
		names(bioclimstack[['bio1']]) <- 'bio1'
	}
	if ('embergerQ' %in% var) {
		# bio5 needed: max temp of the warmest month
		bioclimstack[['bio5']] <- terra::app(tmaxstack, fun = max)
		names(bioclimstack[['bio5']]) <- 'bio5'
	}
	if (any(c('thermicityIndex', 'embergerQ') %in% var)) {
		# bio6 needed: min temp of the coldest month
		bioclimstack[['bio6']] <- terra::app(tminstack, fun = min)
		names(bioclimstack[['bio6']]) <- 'bio6'
	}
	if (any(c('embergerQ', 'climaticMoistureIndex') %in% var)) {
		# bio12 needed: annual precipitation
		bioclimstack[['bio12']] <- terra::app(precipstack, fun = sum)
		names(bioclimstack[['bio12']]) <- 'bio12'
	}
	if (any(c('thermicityIndex', 'embergerQ', 'climaticMoistureIndex') %in% var)) {
		bioclimstack <- terra::rast(bioclimstack[!sapply(bioclimstack, is.null)])
	}
				
	if (any(c('minTempWarmest', 'maxTempColdest', 'meanTempWarmest', 'meanTempColdest', 'thermicityIndex', 'continentality') %in% var)) {
		message('\t\t...temp extremes...')
		tempExtremes <- otherTempExtremes(tmeanstack, tminstack, tmaxstack)
		if ('minTempWarmest' %in% var) {
			reslist[['minTempWarmest']] <- tempExtremes[['minTempWarmest']]
		}
		if ('maxTempColdest' %in% var) {
			reslist[['maxTempColdest']] <- tempExtremes[['maxTempColdest']]
		}
		if ('meanTempWarmest' %in% var) {
			reslist[['meanTempWarmest']] <- tempExtremes[['meanTempWarmest']]
		}
		if ('meanTempColdest' %in% var) {
			reslist[['meanTempColdest']] <- tempExtremes[['meanTempColdest']]
		}
	}

	#growing degree days with temp base of 5 deg C and 0 deg C
	if (any(c('growingDegDays0','growingDegDays5') %in% var)) {
		message('\t\t...growing degree days...')
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
		message('\t\t...month count by deg...')
		monthCount10deg <- monthCountByTemp(tmeanstack, minTemp = 10)
		reslist[['monthCountByTemp10']] <- monthCount10deg
	}
	
	#continentality index
	if (any(c('continentality', 'thermicityIndex') %in% var)) {
		message('\t\t...continentality index...')
		ci <- continentality(tmax = tempExtremes[[4]], tmin = tempExtremes[[3]])
		reslist[['continentality']] <- ci
	}
	
	#Compensated Thermicity Index
	if ('thermicityIndex' %in% var) {
		message('\t\t...thermicity index...')
		thermInd <- thermicityIndex(annualTemp = bioclimstack[['bio1']], minTemp = bioclimstack[['bio6']], maxTemp = tempExtremes[[1]], continentality = ci)
		reslist[['thermicityIndex']] <- thermInd
	}

	#Emberger's pluviothermic quotient
	if ('embergerQ' %in% var) {
		message("\t\t...emberger's Q...")
		emberger <- embergerQ(bioclimstack[['bio12']], bioclimstack[['bio5']], bioclimstack[['bio6']])
		reslist[['embergerQ']] <- emberger
	}

	#annual potential evapotranspiration
	if (any(solradVar %in% var) & is.null(monthPET)) {
		monthPET <- monthlyPET(Tmean = tmeanstack, RA = solradstack, TD = abs(tmaxstack - tminstack))
	}

	if (any(c('PETColdestQuarter', 'PETWarmestQuarter', 'PETWettestQuarter', 'PETDriestQuarter') %in% var)) {
		message('\t\t...PET extremes...')
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
		message('\t\t...annual PET...')
		annualPET <- sum(monthPET)
		reslist[['annualPET']] <- annualPET
	}

	#PET seasonality
	if ('PETseasonality' %in% var) {
		message('\t\t...PET seasonality...')
		seasonalityPET <- PETseasonality(monthPET)
		reslist[['PETseasonality']] <- seasonalityPET
	}

	#climatic moisture index
	if ('climaticMoistureIndex' %in% var) {
		message('\t\t...climatic moisture index...')
		cmi <- climaticMoistureIndex(bioclimstack[['bio12']], annualPET)
		reslist[['climaticMoistureIndex']] <- cmi
	}
	
	#Thornthwaite aridity index
	if ('aridityIndexThornthwaite' %in% var) {
		message('\t\t...Thornthwaite aridity index...')
		aridIndThorn <- aridityIndexThornthwaite(precipstack, monthPET)
		reslist[['aridityIndexThornthwaite']] <- aridIndThorn
	}

	# if minTempWarmest, maxTempColdest, meanTempWarmest, meanTempColdest were requested, 
	# put them back on the same scale as the input temperature rasters
	if ('minTempWarmest' %in% var) {
		reslist[['minTempWarmest']] <- reslist[['minTempWarmest']] * tempScale
	}

	if ('maxTempColdest' %in% var) {
		reslist[['maxTempColdest']] <- reslist[['maxTempColdest']] * tempScale
	}

	if ('meanTempWarmest' %in% var) {
		reslist[['meanTempWarmest']] <- reslist[['meanTempWarmest']] * tempScale
	}

	if ('meanTempColdest' %in% var) {
		reslist[['meanTempColdest']] <- reslist[['meanTempColdest']] * tempScale
	}

	reslist <- terra::rast(reslist)
	reslist <- reslist[[var]]
	return(reslist)
}
