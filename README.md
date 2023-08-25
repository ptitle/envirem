  <!-- badges: start -->
  [![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/envirem)](https://github.com/r-hub/cranlogs.app)
  [![cran version](https://www.r-pkg.org/badges/version/envirem)](https://cran.r-project.org/package=envirem)  
  <!-- badges: end -->
# envirem
An R package for generating additional bioclimatic variables


## Updates
**August 2023 -- envirem v3.0**: The envirem package has been substantially updated such that previously written scripts that use the envirem package will no longer run and will need to be updated. Under the hood, this package no longer depends on raster and rgdal and now uses SpatRaster objects with the [terra](https://cran.r-project.org/package=terra) package. From the user's perspective, some functions have been renamed. 

### Function name updates
- `generateRasters()`/`layerCreation()` replaced by `generateEnvirem()`
- `varnames()` replaced by `namingScheme()`
- `verifyFileStructure()` removed
- `topoWetnessIndex()` removed

Previously, the envirem package provided the capability to split input rasters into tiles, process those, and merge the resulting rasters back together. We have removed this feature, as the R package terra's ability to manage large rasters and memory requirements is superior to anything we can do here. 

## If using, please cite: 

Title, P. O., & Bemmels, J. B. (2018). ENVIREM: An expanded set of bioclimatic and topographic variables increases flexibility and improves performance of ecological niche modeling. Ecography, 41, 291–307. [https://doi.org/10.1111/ecog.02880](https://doi.org/10.1111/ecog.02880)
