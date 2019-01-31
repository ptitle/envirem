##' @export

# define a custom environment for defining the naming of variables

# .var <- new.env(parent = emptyenv())
.var <- new.env()

# default
assign('bio', 'bio_', envir = .var)
assign('tmin', 'tmin_', envir = .var)
assign('tmax', 'tmax_', envir = .var)
assign('tmean', 'tmean_', envir = .var)
assign('precip', 'precip_', envir = .var)
assign('solrad', 'et_solrad_', envir = .var)

assign('bio_post', '', envir = .var)
assign('tmin_post', '', envir = .var)
assign('tmax_post', '', envir = .var)
assign('tmean_post', '', envir = .var)
assign('precip_post', '', envir = .var)
assign('solrad_post', '', envir = .var)


# ls(envir = .var)

