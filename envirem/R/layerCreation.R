##' @title Creates all layers
##'
##' @description Generates all rasterLayers for one particular input dataset.
##'
##' @param masterstack rasterStack containing all precipitation, 
##' min temperature, max temperature and bioclimatic variables

##' @param solradstack rasterStack of monthly solar radiation
##'
##' @param var vector of names of variables to generate, see Details.
##' 
##' @details The function \code{\link{verifyFileStructure}} should be used to 
##' verify that the appropriate rasters are present in \code{masterstack}.
##' 
##' This function is called internally by \code{\link{generateRasters}}. 
##' 
##'	Possible variables to generate include:\cr
##'
##'	aridityIndexThornthwaite \cr
##'	airidityIndexUNEP \cr
##'	climaticMoistureIndex \cr
##'	continentality \cr
##'	embergerQ \cr
##'	growingDegDays0 \cr
##'	growingDegDays5 \cr
##'	humidityIndex \cr
##'	monthCountByTemp10 \cr
##'	PETseasonality \cr
##'	thermicityIndex \cr
##'	minSummerPrecip \cr
##'	maxSummerPrecip \cr
##'	minWinterPrecip \cr
##'	maxWinterPrecip \cr
##'	minTempWarmest \cr
##'	maxTempColdest \cr
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
##' \dontrun{
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##'
##' # create stack of temperature and precipitation rasters
##' # and stack of solar radiation rasters
##' solradFiles <- grep('solrad', rasterFiles, value=TRUE)
##' worldclim <- stack(setdiff(rasterFiles, solradFiles))
##' solar <- stack(solradFiles)
##' 
##' # generate all possible envirem variables
##' layerCreation(worldclim, solar, var='all')
##' }
##' @export




# Function takes stack of precip, mintemp, maxtemp, bioclim, and a stack of solar radiation, and generates rasterstack of new variables
# var is a vector of variable names that will be generated. 

layerCreation <- function(masterstack, solradstack, var) {

	allvar <- c('aridityIndexThornthwaite','climaticMoistureIndex','continentality','embergerQ','growingDegDays0','growingDegDays5','monthCountByTemp10','PETseasonality','thermicityIndex','minTempWarmest','maxTempColdest','PETColdestQuarter','PETWarmestQuarter','PETWettestQuarter','PETDriestQuarter')

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
	
	#naming checks
	expectednames <- c(paste('tmin', 1:12, sep='_'), paste('tmax', 1:12, sep='_'), paste('prec', 1:12, sep='_'), paste('bio', 1:12, sep='_'))
	namecheck <- sapply(paste(expectednames, '$', sep=''), function(x) grepl(x, names(masterstack)), simplify=FALSE)
	if (any(unlist(lapply(namecheck, function(x) length(which(x == TRUE)))) != 1)) {
		stop('masterstack should have names ending with prec_1, tmin_1, tmax_1 and bio_1, for 1:12.')
	}
	namecheck <- sapply(paste(1:12, '$', sep=''), function(x) grepl(x, names(solradstack)), simplify=FALSE)
	namecheck <- unlist(lapply(namecheck, function(x) length(which(x == TRUE))))
	if (raster::nlayers(solradstack) != 12 | any(namecheck > 2)) {
		stop('solrad stack must have names ending in 1:12.')
	}
	
	#receiving list
	reslist <- vector('list', length = length(var))
	names(reslist) <- var

	#create some separate stacks
	cat('\t...splitting rasterstack...\n')
	tminstack <- masterstack[[grep('tmin', names(masterstack), value = TRUE)]]
	tmaxstack <- masterstack[[grep('tmax', names(masterstack), value = TRUE)]]
	tmeanstack <- (tmaxstack + tminstack) / 2 #new mean
	names(tmeanstack) <- gsub('tmax', 'tmean', names(tmaxstack))
	precipstack <- masterstack[[grep('prec', names(masterstack))]]
	
	#enforce ordering
	tminstack <- tminstack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(tminstack))))]]
	tmaxstack <- tmaxstack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(tmaxstack))))]]
	tmeanstack <- tmeanstack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(tmeanstack))))]]
	precipstack <- precipstack[[order(as.numeric(gsub("[a-zA-Z]+_([0-9]+)$", "\\1", names(precipstack))))]]
	solradstack <- solradstack[[order(as.numeric(gsub("et_solrad_([0-9]+)$", "\\1", names(solradstack))))]]
	
	# if (any(c('solradColdestQuarter', 'solradWarmestQuarter', 'solradWettestQuarter', 'solradDriestQuarter') %in% var)) {
	# 	cat('\t...solar radiation extremes...\n')
	# 	srExtremes <- solradExtremes(solradstack, precipstack, tmeanstack)
	# 	if ('solradColdestQuarter' %in% var) {
	# 		reslist[['solradColdestQuarter']] <- srExtremes[[1]]
	# 	}
	# 	if ('solradWarmestQuarter' %in% var) {
	# 		reslist[['solradWarmestQuarter']] <- srExtremes[[2]]
	# 	}
	# 	if ('solradWettestQuarter' %in% var) {
	# 		reslist[['solradWettestQuarter']] <- srExtremes[[3]]
	# 	}
	# 	if ('solradDriestQuarter' %in% var) {
	# 		reslist[['solradDriestQuarter']] <- srExtremes[[4]]
	# 	}

	# }

	if (any(c('minTempWarmest','maxTempColdest','thermicityIndex','continentality') %in% var)) {
		cat('\t...temp extremes...\n')
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
		cat('\t...growing degree days...\n')
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
		cat('\t...month count by deg...\n')
		monthCount10deg <- monthCountByTemp(tmeanstack, minTemp = 10)
		reslist[['monthCountByTemp10']] <- monthCount10deg
	}
	
	#continentality index
	if (any(c('continentality', 'thermicityIndex') %in% var)) {
		cat('\t...continentality index...\n')
		ci <- continentality(tmax = tempExtremes[[4]], tmin = tempExtremes[[3]])
		reslist[['continentality']] <- ci
	}
	
	#Compensated Thermicity Index
	if ('thermicityIndex' %in% var) {
		cat('\t...thermicity index...\n')
		thermInd <- thermicityIndex(annualTemp=masterstack[[grep('bio_1$', names(masterstack))]], minTemp=masterstack[[grep('bio_6$', names(masterstack))]], maxTemp=tempExtremes[[1]], continentality = ci)
		reslist[['thermicityIndex']] <- thermInd
	}

	#Emberger's pluviothermic quotient
	if ('embergerQ' %in% var) {
		cat("\t...emberger's Q...\n")
		emberger <- embergerQ(masterstack[[grep('bio_12$', names(masterstack))]], masterstack[[grep('bio_5$', names(masterstack))]], masterstack[[grep('bio_6$', names(masterstack))]])
		reslist[['embergerQ']] <- emberger
	}

	#annual potential evapotranspiration
	if (any(c('annualPET','PETseasonality','aridityIndexThornthwaite','climaticMoistureIndex','PETColdestQuarter','PETWarmestQuarter','PETWettestQuarter','PETDriestQuarter') %in% var)) {
		monthPET <- monthlyPET(Tmean = tmeanstack, RA = solradstack, TD = abs(tmaxstack - tminstack))
	}

	if (any(c('PETColdestQuarter', 'PETWarmestQuarter', 'PETWettestQuarter', 'PETDriestQuarter') %in% var)) {
		cat('\t...PET extremes...\n')
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

	if (any(c('annualPET','climaticMoistureIndex') %in% var)) {
		cat('\t...annual PET...\n')
		annualPET <- sum(monthPET)
		reslist[['annualPET']] <- annualPET
	}

	if ('PETseasonality' %in% var) {
		cat('\t...PET seasonality...\n')
		seasonalityPET <- PETseasonality(monthPET)
		reslist[['PETseasonality']] <- seasonalityPET
	}

	#climatic moisture index
	if ('climaticMoistureIndex' %in% var) {
		cat('\t...climatic moisture index...\n')
		cmi <- climaticMoistureIndex(masterstack[[grep('bio_12$', names(masterstack))]], annualPET)
		reslist[['climaticMoistureIndex']] <- cmi
	}
	
	#Thornthwaite aridity index
	if ('aridityIndexThornthwaite' %in% var) {
		cat('\t...Thornthwaite aridity index...\n')
		aridIndThorn <- aridityIndexThornthwaite(precipstack, monthPET)
		reslist[['aridityIndexThornthwaite']] <- aridIndThorn
	}

	# if ('aridityIndexUNEP' %in% var) {
	# 	cat('\t...UNEP aridity index...\n')
	# 	aridUNEP <- aridityIndexUNEP(masterstack[[grep('bio_12$', names(masterstack))]], annualPET)
	# 	reslist[['aridityIndexUNEP']] <- aridUNEP
	# }

	reslist <- raster::stack(reslist)
	return(reslist)
}
