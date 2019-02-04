##' @title Data Type Check
##'
##' @description Determines the best data type to implement when writing the raster to file
##'
##' @param r raster object
##'
##' @details 	
##'
##' Function to determine the most memory efficient data type given whether
##' or not the raster contains integer or non-integer values,
##' and the range of those values, based on the definitions described
##' in \code{\link[raster]{dataType}}. 
##'
##' @author Pascal Title
##'
##' @examples
##' rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
##' r <- raster(rasterFiles[1])
##' dataTypeCheck(r)
##' @export

dataTypeCheck <- function(r) {
	
	minVal <- minValue(r)
	maxVal <- maxValue(r)
	
	#check if values are integer
	isInteger <- isTRUE(all.equal(values(r), as.integer(values(r))))
	
	res <- vector('list', length = 2)
	names(res) <- c('isInteger', 'dataType')
	res[[1]] <- isInteger
	
	if (isInteger) {
		
		# if values fall within +/- 32767, then INT2S
		# if values fall within +/- 2147483647, then INT4S
		if (minVal > -32767 & maxVal < 32767) {
			res[[2]] <- 'INT2S'
		} else if (minVal > -2147483647 & maxVal < 2147483647) {
			res[[2]] <- 'INT4S'
		} else {
			stop('DataType problem!')
		}
	} else {
		# if values fall within +/- 3.4e+38, then FLT4S
		# if values fall within +/- 1.7e+308, then FLT8S
		if (minVal > -3.4e+38 & maxVal < 3.4e+38) {
			res[[2]] <- 'FLT4S'
		} else if (minVal > -1.7e+308 & maxVal < 1.7e+308) {
			res[[2]] <- 'FLT8S'
		} else {
			stop('DataType problem!')
		}
	}
	return(res)
}
