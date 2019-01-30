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


# ls(envir = .var)

