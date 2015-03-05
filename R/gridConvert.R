gridConvert <- function(input.file, grid.column="location", output.column="converted_grid", output.grain) {
  # only allow user to enter output grains the function caters for
  if(!output.grain %in% c("10km", "2km", "1km")){ 
     stop("Output grain size must be one of 10km, 2km or 1km")
  }
  if(!grid.column %in% colnames(input.file)) stop ("Please specify the column containing the grid references")
  
  load("data/dinty.letters.rda")
  input.file[,grid.column] <- as.character(input.file[,grid.column])
  grids <- input.file[,grid.column]
  
  # get grids into the correct grain
  grid.1m <- input.file[which(nchar(grids)==12),]
  grid.10m <- input.file[which(nchar(grids)==10),]
  grid.100m <- input.file[which(nchar(grids)==8),]
  grid.1km <- input.file[which(nchar(grids)==6),]
  grid.2km <- input.file[which(nchar(grids)==5),]
  grid.10km <- input.file[which(nchar(grids)==4),]
  
  
  switch(output.grain, # depending on the output grain required, different approach is taken
         "10km"={
           grid.check <- grids[which(nchar(grids) < 4 | nchar(grids) > 12)]
           if(length(grid.check) > 0){
             stop("File contains grid cells larger than 10km or smaller than 1m, please update manually and try again")
           }
           grid.1m[,output.column] <- paste0(substr(grid.1m[,grid.column], 1, 3), substr(grid.1m[,grid.column], 8, 8))
           grid.10m[,output.column] <- paste0(substr(grid.10m[,grid.column], 1, 3), substr(grid.10m[,grid.column], 7, 7))
           grid.100m[,output.column] <- paste0(substr(grid.100m[,grid.column], 1, 3), substr(grid.100m[,grid.column], 6, 6))
           grid.1km[,output.column] <- paste0(substr(grid.1km[,grid.column], 1, 3), substr(grid.1km[,grid.column], 5, 5))
           grid.2km[,output.column] <- substr(grid.2km[,grid.column],1,4)
           grid.10km[,output.column] <- grid.10km[,grid.column]
           output.file <- rbind(grid.1m, grid.10m, grid.100m, grid.1km, grid.2km, grid.10km)
           
         },
         "2km"={
           grid.check <- grids[which(nchar(grids) < 5 | nchar(grids) > 12)]
           if(length(grid.check) > 0){
             stop("File contains grid cells larger than 2km or smaller than 1m, please update manually and try again")
           }

           # section for 1m grids
           if(nrow(grid.1m) > 0){
             main <- paste0(substr(grid.1m[,grid.column], 1, 3), substr(grid.1m[,grid.column], 8, 8))
             X <- substr(grid.1m[,grid.column], 4, 4)
             Y <- substr(grid.1m[,grid.column], 9, 9)
             temp <- data.frame(main, X, Y, stringsAsFactors=FALSE)
             temp$id <- 1:nrow(temp)
             temp <- merge(temp, dinty.letters)
             temp <- temp[order(temp$id),]
             grid.1m[,output.column] <- paste0(temp$main, temp$letter)
             rm(temp)
           }
           # section for 10m grids
           if(nrow(grid.10m) > 0){
             main <- paste0(substr(grid.10m[,grid.column], 1, 3), substr(grid.10m[,grid.column], 7, 7))
             X <- substr(grid.10m[,grid.column], 4, 4)
             Y <- substr(grid.10m[,grid.column], 8, 8)
             temp <- data.frame(main, X, Y, stringsAsFactors=FALSE)
             temp$id <- 1:nrow(temp)
             temp <- merge(temp, dinty.letters)
             temp <- temp[order(temp$id),]
             grid.10m[,output.column] <- paste0(temp$main, temp$letter)
             rm(temp)
           }
           
           # section for 100m grids
           if(nrow(grid.100m) > 0){
             main <- paste0(substr(grid.100m[,grid.column], 1, 3), substr(grid.100m[,grid.column], 6, 6))
             X <- substr(grid.100m[,grid.column], 4, 4)
             Y <- substr(grid.100m[,grid.column], 7, 7)
             temp <- data.frame(main, X, Y, stringsAsFactors=FALSE)
             temp$id <- 1:nrow(temp)
             temp <- merge(temp, dinty.letters)
             temp <- temp[order(temp$id),]
             grid.100m[,output.column] <- paste0(temp$main, temp$letter)
             rm(temp)
           }
           # section for 1km grids
           if(nrow(grid.1km) > 0){
             main <- paste0(substr(grid.1km[,grid.column], 1, 3), substr(grid.1km[,grid.column], 5, 5))
             X <- substr(grid.1km[,grid.column], 4, 4)
             Y <- substr(grid.1km[,grid.column], 6, 6)
             temp <- data.frame(main, X, Y, stringsAsFactors=FALSE)
             temp$id <- 1:nrow(temp)
             temp <- merge(temp, dinty.letters)
             temp <- temp[order(temp$id),]
             grid.1km[,output.column] <- paste0(temp$main, temp$letter)
             rm(temp)
           }          
           # section for 2km grids
           grid.2km[,output.column] <- grid.2km[,grid.column]
           
           output.file <- rbind(grid.1m, grid.10m, grid.100m, grid.1km, grid.2km)
         },
         "1km"={
           grid.check <- grids[which(nchar(grids) < 6 | nchar(grids) > 12)]
           if(length(grid.check) > 0){
             stop("File contains grid cells larger than 1km or smaller than 1m, please update manually and try again")
           } 
           # 1m grids section
           grid.1m[,output.column] <- paste0(substr(grid.1m[,grid.column], 1, 4), 
                                              substr(grid.1m[,grid.column], 8, 9))
           # 10m grids section
           grid.10m[,output.column] <- paste0(substr(grid.10m[,grid.column], 1, 4), 
                                               substr(grid.10m[,grid.column], 7, 8))
           # 100m grids section
           grid.100m[,output.column] <- paste0(substr(grid.100m[,grid.column], 1, 4), 
                                               substr(grid.100m[,grid.column], 6, 7))
           # 1km grids section
           grid.1km[,output.column] <- grid.1km[,grid.column]
           
           output.file <- rbind(grid.1m, grid.10m, grid.100m, grid.1km)
         }
         )
  return(output.file)
}