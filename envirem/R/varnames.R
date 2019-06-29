##' @title List Naming Scheme
##' @description Lists the naming scheme, either as default, or 
##' as modified by the user.
##' @details See \code{\link{assignNames}}.
##' @examples
##' varnames()
##' @export

varnames <- function() {
	
	# cat('\n')
	# cat('bio:\t', .var$bio, '\t\t\t##\t', .var$bio_post, '\n')
	# cat('tmin:\t', .var$tmin, '\t\t\t##\t', .var$tmin_post, '\n')
	# cat('tmax:\t', .var$tmax, '\t\t\t##\t', .var$tmax_post, '\n')
	# cat('tmean:\t', .var$tmean, '\t\t\t##\t', .var$tmean_post, '\n')
	# cat('precip:\t', .var$precip, '\t\t\t##\t', .var$precip_post, '\n')
	# cat('solrad:\t', .var$solrad, '\t\t\t##\t', .var$solrad_post, '\n')
	# cat('\nto change these values, see ?assignNames.')

	# print(sprintf("%-7s %-11s %-3s %s", 'bio:', .var$bio, '##', .var$bio_post))
	# print(sprintf("%-7s %-11s %-3s %s", 'tmin:', .var$tmin, '##', .var$tmin_post))
	# print(sprintf("%-7s %-11s %-3s %s", 'tmax:', .var$tmax, '##', .var$tmax_post))
	# print(sprintf("%-7s %-11s %-3s %s", 'tmean:', .var$tmean, '##', .var$tmean_post))
	# print(sprintf("%-7s %-11s %-3s %s", 'precip:', .var$precip, '##', .var$precip_post))
	# print(sprintf("%-7s %-11s %-3s %s", 'solrad:', .var$solrad, '##', .var$solrad_post))
	# cat('\nto change these values, see ?assignNames.')


	vartable <- huxtable::hux(c('tmin:','tmax:','tmean:','precip:','solrad:'), 
		c(.var$tmin, .var$tmax, .var$tmean, .var$precip, .var$solrad),
		rep('##', 5),
		c(.var$tmin_post, .var$tmax_post, .var$tmean_post, .var$precip_post, .var$solrad_post))

	message('')
	huxtable::print_screen(vartable)
	message('\n\n\tTo change these values, see ?assignNames.')
	
}