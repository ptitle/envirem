##' @title envirem
##'
##' @description Generation of bioclimatic rasters that are complementary to the WorldClim dataset.  
##' 
##' 
##' @author Pascal O. Title, Jordan B. Bemmels
##' 
##' @references \url{http://envirem.gituhub.io}
##'
##' Title, P.O. and J.B. Bemmels. \emph{Submitted}. ENVIREM: An expanded set of bioclimatic variables improves ecological niche modeling performance. \emph{Methods in Ecology and Evolution}.
##' 
##' @details
##' 
##' Package: envirem\cr
##' Type: Package\cr
##' Version: 1.0\cr
##' Date: 2016-08-01\cr
##' License: GPL-2 | GPL-3\cr
##' 
##' For input rasters, temperature rasters are assumed to be in degrees C * 10 
##' and precipitation rasters are assumed to be in mm, as is the case in WorldClim. Units of
##' output rasters are specified in the \code{Value} field of the documentation.
##'
##' The main function for generating ENVIREM rasters is \code{\link{generateRasters}}. 
##' A complete tutorial of this R package can be found at \url{http://envirem.gituhub.io}. 
##' 
##' @name envirem
##' @docType package
##' @keywords package
##'
##' @import raster RSAGA
##' @importFrom stats sd
##'
NULL
