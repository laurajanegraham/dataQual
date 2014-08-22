gridConvert <- function(input.file, grid.column="location", output.column="converted_grid", output.grain) {
  # only allow user to enter output grains the function caters for
  if(!output.grain %in% c("10km", "2km", "1km", "100m")){ 
     stop("Output grain size must be one of 10km, 2km, 1km or 100m")
  }
  if(!grid.column %in% colnames(input.file)) stop ("Please specify the column containing the grid references")
  
  load("data/dinty.letters.rda")
  input.file[,grid.column] <- as.character(input.file[,grid.column])
  grids <- input.file[,grid.column]
  
  # get grids into the correct grain
  grid.100m <- input.file[which(nchar(grids)==8),]
  grid.1km <- input.file[which(nchar(grids)==6),]
  grid.2km <- input.file[which(nchar(grids)==5),]
  grid.10km <- input.file[which(nchar(grids)==4),]
  
  
  switch(output.grain, # depending on the output grain required, different approach is taken
         "10km"={
           grid.check <- grids[which(nchar(grids) < 4 | nchar(grids) > 8)]
           if(length(grid.check) > 0){
             stop("File contains grid cells larger than 10km or smaller than 100m, please update manually and try again")
           }
           grid.100m[,output.column] <- paste0(substr(grid.100m[,grid.column], 1, 3), substr(grid.100m[,grid.column], 6, 6))
           grid.1km[,output.column] <- paste0(substr(grid.1km[,grid.column], 1, 3), substr(grid.1km[,grid.column], 5, 5))
           grid.2km[,output.column] <- substr(grid.2km[,grid.column],1,4)
           grid.10km[,output.column] <- grid.10km[,grid.column]
           output.file <- rbind(grid.100m, grid.1km, grid.2km, grid.10km)
           
         },
         "2km"={
           grid.check <- grids[which(nchar(grids) < 5 | nchar(grids) > 8)]
           if(length(grid.check) > 0){
             stop("File contains grid cells larger than 2km or smaller than 100m, please update manually and try again")
           }
           # section for 100m grids
           main <- paste0(substr(grid.100m[,grid.column], 1, 3), substr(grid.100m[,grid.column], 6, 6))
           X <- substr(grid.100m[,grid.column], 4, 4)
           Y <- substr(grid.100m[,grid.column], 7, 7)
           temp <- data.frame(main, X, Y, stringsAsFactors=FALSE)
           temp$id <- 1:nrow(temp)
           temp <- merge(temp, dinty.letters)
           temp <- temp[order(temp$id),]
           grid.100m[,output.column] <- paste0(temp$main, temp$letter)
           rm(temp)
           
           # section for 1km grids
           main <- paste0(substr(grid.1km[,grid.column], 1, 3), substr(grid.1km[,grid.column], 5, 5))
           X <- substr(grid.1km[,grid.column], 4, 4)
           Y <- substr(grid.1km[,grid.column], 6, 6)
           temp <- data.frame(main, X, Y, stringsAsFactors=FALSE)
           temp$id <- 1:nrow(temp)
           temp <- merge(temp, dinty.letters)
           temp <- temp[order(temp$id),]
           grid.1km[,output.column] <- paste0(temp$main, temp$letter)
                      
           # section for 2km grids
           grid.2km[,output.column] <- grid.2km[,grid.column]
           
           output.file <- rbind(grid.100m, grid.1km, grid.2km)
         },
         "1km"={
           grid.check <- grids[which(nchar(grids) < 6 | nchar(grids) > 8)]
           if(length(grid.check) > 0){
             stop("File contains grid cells larger than 1km or smaller than 100m, please update manually and try again")
           } 
           # 100m grids section
           grid.100m[,output.column] <- paste0(substr(grid.100m[,grid.column], 1, 4), 
                                               substr(grid.100m[,grid.column], 6, 7))
           # 1km grids section
           grid.1km[,output.column] <- grid.1km[,grid.column]
           
           output.file <- rbind(grid.100m, grid.1km, grid.2km)
         }
         )
  return(output.file)
}