# Laura Graham 21/02/2014


################################################################################
# Maps of uncertainty: calculate the collectors curve for each grid cell; 
# fits to a Clench function; calculates slope of curve at last record; 
# slope is the uncertainty of that cell; output slope value, clench parameters
# (a and b), species richness and record numbers.

################################################################################

calcUncertainty <- function(species.dat, grid.poly, species.col, grid.col, n.rand=100) {
  # error checking
  if(!grid.col %in% colnames(species.dat)) stop("Please specify the column containing the grid references")
  if(!species.col %in% colnames(species.dat)) stop("Please specify the column containing the species names")
  if(!grid.col %in% names(grid.poly)) stop("The grid reference for the shapefile must match that of the species data")
  if(class(grid.poly)!="SpatialPolygonsDataFrame") stop("Grid file must be class SpatialPolygonsDataFrame")
  
  # select necessary columns
  species.dat <- species.dat[,c(grid.col, species.col)]
  no.species <- length(unique(species.dat[,species.col]))
  
  # merge with grids to get only those in study area
  species.dat <- merge(species.dat, grid.poly@data)
  
  # create a list of the grid cells for looping
  grid.list <- unique(species.dat[,grid.col])
  
  cc.list <- list()
  
  for(grid in grid.list) {
    grid.dat <- species.dat[species.dat[,grid.col]==grid, species.col] 
    if(length(grid.dat) != length(unique(grid.dat)) && length(unique(grid.dat)) > 1) { # only calculate the slope for those with a sampling effort of records >= 2*species
      grid.iters <- rep(data.frame(species=grid.dat), 100) 
      grid.rand <- lapply(grid.iters, function(x) { 
        data.frame(species=sample(x, size=length(x)))
      })
      
      grid.res <- lapply(grid.rand, function(x) {
        mutate(x, first.appearance = !duplicated(x$species)
               ,cumsum = cumsum(first.appearance)
        ) %>%
          select(cumsum)
      })
      
      grid.res <- do.call("cbind", grid.res)
      cc.list[[grid]] <- data.frame(records = 1:nrow(grid.res), mean.species = rowMeans(grid.res))
    }
  }
  
  ### create the results for the output table ###
  results <- data.frame(grid_ref = NA, slope = NA, clench_a = NA, 
                                    clench_b = NA, no_recs = NA, sp_rich = NA, confidence = NA)
  i <- 1
  
  for(grid in grid.list){
    if(grid %in% names(cc.list)){
      control <- nls.control(minFactor = 1/1000000)
      clench <- nls(mean.species~(a*records)/(1 + b*records), data=cc.list[[grid]], 
                    start = list(a=1, b=1), control)
      
      clench.slope <- function(x) {
        coef(clench)[1]/(coef(clench)[2]^2*x^2 + 2*coef(clench)[2]*x + 1)
      }
      
      results[i, 1] <- grid
      results[i, 2] <- clench.slope(nrow(cc.list[[grid]]))
      results[i, 3] <- coef(clench)[1]
      results[i, 4] <- coef(clench)[2]
      results[i, 5] <- nrow(cc.list[[grid]])
      results[i, 6] <- max(cc.list[[grid]][,2])
      results[i, 7] <- 1 - clench.slope(nrow(cc.list[[grid]]))
    } else{
      grid.dat <- species.dat[species.dat[,grid.col]==grid,species.col] 
      results[i, 1] <- grid
      results[i, 2] <- NA
      results[i, 3] <- NA
      results[i, 4] <- NA
      results[i, 5] <- length(grid.dat)
      results[i, 6] <- length(unique(grid.dat))
      results[i, 7] <- NA
    }

    i <- i + 1
  }
  
  grid.shp@data <- data.frame(grid.shp@data, results[match(grid.shp@data[,grid.col], 
                                                           results[,1]),])
  
  output <- list(results=results, cc.list=cc.list, grid.shp=grid.shp)
  return(output)
}