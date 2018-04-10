##' @title PET seasonality
##'
##' @description Seasonality of potential evapotranspiration
##'
##' @param PETstack rasterStack of monthly PET rasters
##'
##' @details PET seasonality = 100 * standard deviation of monthly PET.
##'
##' @return rasterLayer in mm / month
##' 
##' @references
##' Metzger, M.J., Bunce, R.G.H., Jongman, R.H.G., Sayre, R., Trabucco, A. & Zomer, R. (2013). 
##' A high-resolution bioclimate map of the world: a unifying framework for global 
##' biodiversity research and monitoring. \emph{Global Ecology and Biogeography}, 
##' \strong{22}, 630-638.
##'
##' @author Pascal Title
##'
##' @seealso \link{monthlyPET}
##'
##' @examples
##' \donttest{
##' # Find example rasters
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' env <- stack(rasterFiles)
##'
##' # identify the appropriate layers
##' meantemp <- grep('mean', names(env), value=TRUE)
##' solar <- grep('solrad', names(env), value=TRUE)
##' maxtemp <- grep('tmax', names(env), value=TRUE)
##' mintemp <- grep('tmin', names(env), value=TRUE)
##' 
##' # read them in as rasterStacks
##' meantemp <- stack(env[[meantemp]])
##' solar <- stack(env[[solar]])
##' maxtemp <- stack(env[[maxtemp]])
##' mintemp <- stack(env[[mintemp]])
##' tempRange <- abs(maxtemp - mintemp)
##' 
##' # get monthly PET
##' pet <- monthlyPET(meantemp, solar, tempRange)
##' 
##' PETseasonality(pet)
##' }
##' @export


#var_37: PET seasonality: 100 * standard deviation of monthly PET values. 
PETseasonality <- function(PETstack) {
	res <- 100 * raster::calc(PETstack, fun=sd)
	names(res) <- 'PETseasonality'
	return(res)
}
