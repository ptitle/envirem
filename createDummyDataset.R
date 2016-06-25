# create testing dataset

require(raster)

outputfolder <- '~/Dropbox/bioclimExtension/rpackage/envirem/inst/extdata/'

setwd('~/Documents/worldclim/extraBioclimPaper/current/10arcmin/processed/')

files <- list.files(pattern='.tif$')

dat <- stack(files)
dat <- aggregate(dat, 25)

for (i in 1:nlayers(dat)) {
	writeRaster(dat[[i]], paste0(outputfolder, names(dat)[i], '.tif'), format='GTiff', overwrite=TRUE)
}

# elevation
elev <- raster('~/Documents/worldclim/extraBioclimPaper/elev/topo30Rotated.tif')

elev <- resample(elev, dat[[1]])

writeRaster(elev, paste0(outputfolder, 'elev.tif'), format='GTiff', overwrite=TRUE)

