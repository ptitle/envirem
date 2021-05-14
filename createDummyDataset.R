# create testing dataset

require(raster)

outputfolder <- '~/envirem/envirem/inst/extdata/'


setwd('~/Documents/worldclim/extraBioclimPaper/current/10arcmin/processed/')


# setwd('~/envirem/processed/')

files <- list.files(pattern='.tif$')

dat <- stack(files)
dat <- aggregate(dat, 70)

for (i in 1:nlayers(dat)) {
	writeRaster(dat[[i]], paste0(outputfolder, names(dat)[i], '.tif'), datatype='FLT4S', format='GTiff', NAflag=-9999, overwrite=TRUE)
}

# elevation
elev <- raster('~/Documents/worldclim/extraBioclimPaper/elev/topo30Rotated.tif')
# elev <- raster('~/envirem/topo30Rotated.tif')

elev <- resample(elev, dat[[1]])

writeRaster(elev, paste0(outputfolder, 'elev.tif'), datatype='FLT4S', NAflag=-9999, format='GTiff', overwrite=TRUE)

### update (to fix projection warnings)

rasterFiles <- list.files(system.file('extdata', package='envirem'), full.names=TRUE)
env <- stack(rasterFiles)

#projection(env) <- sf::st_crs(4326)$input
# projection(env) <- "+init=epsg:4326"
projection(env) <- CRS(SRS_string='EPSG:4326')


for (i in 1:nlayers(env)) {
	message('\t', i)
	writeRaster(env[[i]], paste0(outputfolder, names(env)[i], '.tif'), datatype='FLT4S', format='GTiff', NAflag=-9999, overwrite=TRUE)
}

