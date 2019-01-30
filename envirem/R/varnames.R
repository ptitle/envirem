##' @title List Naming Scheme
##' @description Lists the naming scheme, either as default, or 
##' as modified by the user.
##' @details See \code{\link{assignNames}}.
##' @examples
##' varnames()
##' @export

varnames <- function() {
	
	cat('\n')
	cat('bio:\t', .var$bio, '\n')
	cat('tmin:\t', .var$tmin, '\n')
	cat('tmax:\t', .var$tmax, '\n')
	cat('tmean:\t', .var$tmean, '\n')
	cat('precip:\t', .var$precip, '\n')
	cat('solrad:\t', .var$solrad, '\n')
	cat('\nto change these values, see ?assignNames.')
	
}

