##' @title envirem
##'
##' @description Generation of bioclimatic rasters that are complementary to the typical 19 bioclim variables.  
##' 
##' 
##' @author Pascal O. Title, Jordan B. Bemmels
##' 
##' @references \url{http://envirem.github.io}
##'
##' Title, P.O., Bemmels, J.B. 2018. ENVIREM: An expanded set of bioclimatic and topographic variables increases flexibility and improves performance of ecological niche modeling. Ecography 41:291–307.
##' 
##' @details
##' 
##' Package: envirem\cr
##' Type: Package\cr
##' Version: 2.2\cr
##' Date: 2020-06-03\cr
##' License: GPL-2 | GPL-3\cr
##'
##' \strong{NOTE}: Temperature rasters are now assumed by default to be in degrees C and precipitation in mm. 
##" However, different input datasets may have different units. For example, worldclim v1 has temperature 
##' rasters in degrees C * 10. Worldclim v2 uses degrees C. CHELSA has several options, depending 
##' on whether rasters are downloaded as floating point or integer. Therefore, there is an argument 
##' \code{tempScale} to specify the units of temperature, and \code{precipScale} to define precipitation units:
##' 
##' For example:\cr
##' If using worldclim v1 data where temperature is in degrees C * 10, specify 
##' \code{tempScale = 10}.\cr
##' If using worldclim v2 where temperature is in degrees C, specify \code{tempScale = 1}.\cr
##' For CHELSA, read the documentation and carefully examine the rasters. 
##'
##' If a function does not have the \code{tempScale} argument, then the function is not sensitive
##' to the units of the input temperature rasters.
##'
##' 
##' Of course, it is also perfectly acceptable to leave \code{tempScale = 1} and \code{precipScale = 1} 
##' and modify the input rasters yourself. 
##'
##'
##' The main function for generating ENVIREM rasters is \code{\link{generateRasters}}. 
##' A complete tutorial of this R package can be found at \url{http://envirem.github.io}. 
##' 
##' @name envirem
##' @docType package
##' @keywords package
##'
##' @import raster RSAGA palinsol
##' @importFrom stats sd
##' @importFrom knitr kable
##'
NULL
