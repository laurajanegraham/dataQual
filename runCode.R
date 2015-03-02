require(rgdal) 
# this package is required to load shape data. If not installed, run
# install.packages("rgdal")

setwd(xxx) 
# update to point to the folder downloaded from github

source("R/calcUncertainty.R")
source("R/gridConvert.R")

recs <- read.csv("data/species_recs.csv") 
# update to name and location of the .csv file - this inputs the shape file make
# sure that you have filtered the records so that there are no records with a
# grid cell size greater than the size you want to run the analysis at

recs.out <- gridConvert(recs, "orig_grid", "grid_2km", "2km") 
# 1st argument: species data;
# 2nd argument: input grid_ref column - named as is on the file; 
# 3rd argument: output grid ref column - named as desired; 
# 4th argument: output grain size - one of 10km, 2km, 1km
# gridConvert() adds a column onto your data containing the grid references at
# the desired grain.

grid.shp <- readOGR("data", "grids") 
# this loads the shape file containing the grid data update the first bit to the
# location and the second bit to correct name of grid file (without the .shp
# part)

names(grid.shp) <- "grid_2km" 
# make this the same as the output grid ref column from the gridConvert step

uncertainty <- calcUncertainty(recs.out, grid.shp, "species", "grid_2km")
# argument 1: output from gridConvert; 
# argument 2: shape file; 
# argument 3: name of the column containing the species names; 
# argument 4: name of the column containing the grid references (must be the
# same on both the shape file and the species data file)
# output of calcUncertainty() is a list containing 3 items: 
# (1) data frame containing the "slope" which gives an idea of the completeness
# of the data for each grid, as well as the number of records and species
# richness for each grid;
# (2) the collectors curves for each grid cell; 
# (3) a shapefile where the # results from (1) are put onto the attribute table
# for the grid file

# for GiGL - most likely interested in the resulting shapefile
grid.shp <- uncertainty[[3]]

writeOGR(grid.shp, ".", "grids_with_data_quality", driver="ESRI Shapefile")
# export the shapefile - update "grids_with_data_quality" to name as required.
# Replacing the . with a location will write the file to that location
