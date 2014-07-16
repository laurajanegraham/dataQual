### create a plot showing the species-accumulation curve for each grid cell - perhaps have 
### those cells whose slope is below 0.05 in a different colour? Panel of 3 plots, one for 
### each species group
plot.sacurves <- function(species.group, xaxis="", yaxis="", plot.title) {

  load(paste0("../results/uncertainty/slopes.", species.group, ".rda"))
  load(paste0("../results/uncertainty/", species.group, ".uncertainty.rda"))
  x.limit <- max(uncertainty.results$no.records)
  y.limit <- max(uncertainty.results$sp.richness)
  species.group <- paste0(toupper(substr(species.group, 1, 1)),
                          substr(species.group, 2, nchar(species.group)), "s")
  plot(res[[1]][,2]~res[[1]][,1], type="n", cex.axis=2, cex.lab=3, xlab=xaxis, ylab=yaxis
       , xlim=c(1, x.limit), ylim=c(1, y.limit), las=1, cex.main=3, main=species.group)
  
  text(1, y.limit, plot.title, cex=2, pos=4)
  grids <- names(res)
  for(i in 1:length(grids)){
    if(subset(uncertainty.results, grid.ref==grids[i], select="clench.slope")>0.05) {
      lines(res[[i]][,2]~res[[i]][,1], col="grey")  
    } else
      lines(res[[i]][,2]~res[[i]][,1], col="black", lwd="2")  
  }
}

plot.spdist <- function(species.group, grid.file, stat, plot.title, row.title="") {
  load(paste0("../results/uncertainty/", species.group, ".uncertainty.rda"))
  studysite <- readOGR("../data/shapes", "ncc_2km_buffer", verbose=FALSE)
  grid.poly <- readOGR("../data/shapes", grid.file, verbose=FALSE)
  load("../data/param.subset.rda")
  grid.poly@data <- subset(grid.poly@data, select="grid_ref")
  
  grid.poly@data <- data.frame(grid.poly@data, uncertainty.results[match(grid.poly@data$grid_ref, 
                                                                         uncertainty.results$grid.ref),])

  grids <- param.subset[param.subset$sp.group==species.group,1]
  
  subset_list <- strsplit(grids, ", ", fixed=TRUE)
  subset_list <- do.call("rbind", subset_list)
  if(!is.null(subset_list)){
    grid.subset <- grid.poly[grid.poly$grid_ref %in% subset_list,]
    grid.subset <- gUnionCascaded(grid.subset)
  }
  
  if(stat == "sp.rich") {
    # PLOT SPECIES RICHNESS
    cols <- brewer.pal(5, "Greys")
    x <- round(max(uncertainty.results$sp.richness, na.rm=TRUE)/3, 0)
    y <- c(x*0, x*1, x*2, max(uncertainty.results$sp.richness, na.rm=TRUE))

    brks <- classIntervals(na.omit(grid.poly$sp.richness), n=3, style="fixed", fixedBreaks = y)
    plot(grid.poly, col=cols[findInterval(grid.poly$sp.richness, brks$brks, all.inside=TRUE)])
    plot(studysite, add=TRUE)
    text(446000, 350000, plot.title, cex=2, pos=4)
    title(main=row.title, adj=0, cex.main=2)
    legend(x=465000, y=348000, legend=leglabs(brks$brks), fill=cols, bty="n", cex=2, title="No. Species")
  }
  
  if(stat == "no.recs") {
    # PLOT NUMBER OF RECORDS
    cols <- brewer.pal(5, "Greys")
    q <- as.integer(quantile(na.omit(grid.poly$no.records)))
    brks <- classIntervals(na.omit(grid.poly$no.records), n=4, style="fixed", fixedBreaks = q)
    plot(grid.poly, col=cols[findInterval(grid.poly$no.records, brks$brks, all.inside=TRUE)])
    plot(studysite, add=TRUE)
    text(446000, 350000, plot.title, cex=2, pos=4)
    legend(x=465000, y=348000, legend=leglabs(brks$brks), fill=cols, bty="n", cex=2, title="No. Records")
  }
  
  if(stat == "w.samp") {
    # PLOT WELL SURVEYED SITES
    w.samp <- subset(grid.poly, clench.slope <= 0.05)
    plot(grid.poly)
    plot(w.samp, col="grey", add=TRUE)
    plot(studysite, add=TRUE)
    text(446000, 350000, plot.title, cex=2, pos=4)
    legend(x=465000, y=348000, legend=expression("Slope" <=0.05), fill="grey", bty="n", cex=2)
    if(!is.null(subset_list)) plot(grid.subset, lwd=6, add=T)
  }
}





# ### create plots of species richness, record number and well sampled grids - perhaps a
# ### 3x3 panel with these across, and species groups on rows.
# sp.dist.mapping <- function(species.group, grid.file) {
# 
#   load(paste0("../results/uncertainty/", species.group, ".uncertainty.rda"))
#   studysite <- readOGR("../data/shapes", "ncc_2km_buffer", verbose=FALSE)
#   grid.poly <- readOGR("../data/shapes", grid.file, verbose=FALSE)
#   grid.poly.gg <- fortify(grid.poly, region="grid_ref")
#   studysite.gg <- fortify(studysite, region="NAME") 
#   # create a function based on ggplot with formats as required (ie. no background etc.) 
#   ggmap <- function(...) {
#    ggplot2::ggplot(...) + theme_classic() + theme(axis.line=element_blank(),
#                                                   axis.text=element_blank(),
#                                                   axis.title=element_blank(),
#                                                   axis.ticks=element_blank(),
#                                                   panel.grid.major = element_blank(), 
#                                                   panel.grid.minor = element_blank())
#   }
#   
#   
#   # map species richness
#   sp.rich <- ggmap() 
#   sp.rich <- sp.rich + geom_polygon(data=studysite.gg, colour="black", fill=NA, aes(x=long, y=lat, group=group))
#   sp.rich <- sp.rich + geom_map(data=uncertainty.results, aes(map_id= grid.ref, fill = as.numeric(as.character(sp.richness))), 
#                           map=grid.poly.gg) 
#   #sp.rich <- sp.rich + scale_colour_brewer(palette="Set1")
#   sp.rich <- sp.rich + expand_limits(x=grid.poly.gg$long, y=grid.poly.gg$lat) 
#   sp.rich <- sp.rich + scale_fill_continuous(name="Number of Species") 
#   sp.rich <- sp.rich + geom_polygon(data=grid.poly.gg, colour="black", fill=NA, aes(x=long, y=lat, group=group))
#   sp.rich <- sp.rich + coord_fixed()
#   sp.rich
#   
#   # map number of records
#   recs <- ggmap() 
#   recs <- recs + geom_polygon(data=studysite.gg, colour="black", fill=NA, aes(x=long, y=lat, group=group))
#   recs <- recs + geom_map(data=uncertainty.results, aes(map_id= grid.ref, fill = as.numeric(as.character(no.records))), 
#                     map=grid.poly.gg) 
#   recs <- recs + expand_limits(x=grid.poly.gg$long, y=grid.poly.gg$lat) 
#   recs <- recs + scale_fill_continuous(name="Number of Records") 
#   recs <- recs + geom_polygon(data=grid.poly.gg, colour="black", fill=NA, aes(x=long, y=lat, group=group))
#   recs <- recs + coord_fixed()
#   recs
#   
#   # map well sampled grid cells
#   w.samp <- ggmap() 
#   w.samp <- w.samp + geom_polygon(data=studysite.gg, colour="black", fill=NA, aes(x=long, y=lat, group=group))
#   w.samp <- w.samp + geom_map(data=subset(uncertainty.results, clench.slope<=0.05), 
#                               aes(map_id= grid.ref, fill="test"),
#                               map=grid.poly.gg) 
#   w.samp <- w.samp + expand_limits(x=grid.poly.gg$long, y=grid.poly.gg$lat)
#   w.samp <- w.samp + scale_fill_discrete(name="", labels=paste0("  Slope ", expression("<=0.05")))
#   w.samp <- w.samp + geom_polygon(data=grid.poly.gg, colour="black", fill=NA, aes(x=long, y=lat, group=group))
#   w.samp <- w.samp + coord_fixed()
#   w.samp
#   
#   grid.arrange(sp.rich, recs, w.samp, nrow=1, ncol=3)
# }