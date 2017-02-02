##' @title envirem
##'
##' @description Generation of bioclimatic rasters that are complementary to the WorldClim dataset.  
##' 
##' 
##' @author Pascal O. Title, Jordan B. Bemmels
##' 
##' @references \url{http://envirem.github.io}
##'
##' Title, P.O., Bemmels, J.B. 2017. ENVIREM: An expanded set of bioclimatic and topographic variables increases flexibility and improves performance of ecological niche modeling. Ecography doi: 10.1111/ecog.02880.
##' 
##' @details
##' 
##' Package: envirem\cr
##' Type: Package\cr
##' Version: 1.1\cr
##' Date: 2017-02-02\cr
##' License: GPL-2 | GPL-3\cr
##' 
##' For input rasters, temperature rasters are assumed to be in degrees C * 10 
##' and precipitation rasters are assumed to be in mm, as is the case in WorldClim. Units of
##' output rasters are specified in the \code{Value} field of the documentation.
##'
##' The main function for generating ENVIREM rasters is \code{\link{generateRasters}}. 
##' A complete tutorial of this R package can be found at \url{http://envirem.github.io}. 
##' 
##' @name envirem
##' @docType package
##' @keywords package
##'
##' @import raster RSAGA
##' @importFrom stats sd
##'
NULL
