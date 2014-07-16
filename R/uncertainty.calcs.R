# Laura Graham 21/02/2014


################################################################################
# Maps of uncertainty: calculate the collectors curve for each grid cell; 
# fits to a Clench function; calculates slope of curve at last record; 
# slope is the uncertainty of that cell; output slope value, clench parameters
# (a and b), species richness and record numbers.

################################################################################

uncertainty.calc <- function(species.group, grid.file) {
  require(rgdal)
  # load species data and select necessary columns
  species.dat <- get(load(paste0("data/all_",species.group,"_species.rda")))
  species.dat <- species.dat[,c("grid_ref", "species")]
  no.species <- length(unique(species.dat$species))
  
  
  # load shapefiles
  grid.poly <- readOGR("data/shapes", grid.file, verbose=FALSE)
  
  # get rid of spaces and put to lower case to avoid duplicate species names 
  species.dat$Species <- gsub(" ", ".", tolower(species.dat$species), fixed=T)
  
  # merge with grids to get only those in study area
  species.dat <- merge(species.dat, grid.poly@data)
  
  # create a list of the grid cells for looping
  grid.list <- unique(species.dat$grid_ref)
  
  res <- list()
  
  for(grid in grid.list) {
    grid.dat <- species.dat[species.dat$grid_ref==grid,2]
    grid.res <- data.frame(NA, nrow=length(grid.dat), ncol=100)
    if(length(grid.dat) >= no.species/2 && length(unique(grid.dat)) > 1) { # only calculate the slope for those with a sampling effort of records >= 2*species
      for(i in 1:100) {
        # create 100 randomisations of the records list
        grid.dat <- sample(grid.dat, size=length(grid.dat))
        
        # create number of species at 1, 2, 3, ....j records
        for(j in 1:length(grid.dat)) {
          grid.res[j, i] <- length(unique(grid.dat[1:j]))
        }
      }
      
      res[[grid]] <- data.frame(records = 1:length(grid.dat), mean.species = rowMeans(grid.res))
    }
  }
  
  save(res, file=paste0("results/uncertainty/slopes.", species.group, ".rda"))
  
  ### create the results for the output table ###
  uncertainty.results <- data.frame(grid.ref = NA, clench.slope = NA, clench.a = NA, 
                                    clench.b = NA, no.records = NA, sp.richness = NA)
  i <- 1
  
  for(grid in names(res)){
    clench <- nls(mean.species~(a*records)/(1 + b*records), data=res[[grid]], 
                  start = list(a=1, b=1))
    
    clench.slope <- function(x) {
      coef(clench)[1]/(coef(clench)[2]^2*x^2 + 2*coef(clench)[2]*x + 1)
    }  
    uncertainty.results[i, 1] <- grid
    uncertainty.results[i, 2] <- clench.slope(nrow(res[[grid]]))
    uncertainty.results[i, 3] <- coef(clench)[1]
    uncertainty.results[i, 4] <- coef(clench)[2]
    uncertainty.results[i, 5] <- nrow(res[[grid]])
    uncertainty.results[i, 6] <- max(res[[grid]][,2])
    i <- i + 1
  }
  
  save(uncertainty.results, file=paste0("results/uncertainty/", species.group, ".uncertainty.rda"))
}