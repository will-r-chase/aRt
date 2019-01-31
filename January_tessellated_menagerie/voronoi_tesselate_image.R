library(tidyverse)
library(colourlovers)
library(mgcv)
library(deldir)
library(sp)
library(rgeos)

#load sparse border
giraffe <- readRDS("sparse_giraffe_points.rds")


#fill w/ some random points
#check triangulate_image script for more details on this process
x <- runif(8000, min = min(giraffe$x), max = max(giraffe$x))
y <- runif(8000, min = min(giraffe$y), max = max(giraffe$y))

fill_df <- data.frame(x = x, y = y)

## Function to buffer points in XY space:
## from https://davidrroberts.wordpress.com/2015/09/25/spatial-buffering-of-points-in-r-while-retaining-maximum-sample-size/
## Returns the original data table with buffered points removed.
# foo - a data.frame to select from with columns x, y
# buffer - the minimum distance between output points
# reps - the number of repetitions for the points selection
buffer.f <- function(foo, buffer, reps){
  # Make list of suitable vectors
  suitable <- list()
  for(k in 1:reps){
    # Make the output vector
    outvec <- as.numeric(c())
    # Make the vector of dropped (buffered out) points
    dropvec <- c()
    for(i in 1:nrow(foo)){
      # Stop running when all points exhausted
      if(length(dropvec)<nrow(foo)){
        # Set the rows to sample from
        if(i>1){
          rowsleft <- (1:nrow(foo))[-c(dropvec)]
        } else {
          rowsleft <- 1:nrow(foo)
        }
        # Randomly select point
        outpoint <- as.numeric(sample(as.character(rowsleft),1))
        outvec[i] <- outpoint
        # Remove points within buffer
        outcoord <- foo[outpoint,c("x","y")]
        dropvec <- c(dropvec, which(sqrt((foo$x-outcoord$x)^2 + (foo$y-outcoord$y)^2)<buffer))
        # Remove unnecessary duplicates in the buffered points
        dropvec <- dropvec[!duplicated(dropvec)]
      } 
    } 
    # Populate the suitable points list
    suitable[[k]] <- outvec
  }
  # Go through the iterations and pick a list with the most data
  best <- unlist(suitable[which.max(lapply(suitable,length))])
  foo[best,]
}

fill_df_buffer <- buffer.f(fill_df, 70, 1000)

x <- fill_df_buffer$x
y <- fill_df_buffer$y

is_inside_buff <- inSide(giraffe, x, y)

fill_buff <- tibble(x, y, is_inside_buff) %>%
  filter(is_inside_buff)

#check plot
ggplot() +
  geom_path(data = giraffe, aes(x, y)) +
  geom_point(data = fill_buff, aes(x, y)) +
  scale_y_reverse() +
  theme_void()

#to constrain voronoi tesselation to border
#convert border to SpatialPolygons
#then use deldir to get voronoi polygons
#get intersection of the two w/ gIntersect
#from https://stackoverflow.com/a/24237509/7712222
giraffe_sp <- SpatialPolygons(list(Polygons(list(Polygon(giraffe)), ID=1)))

voronoipolygons <- function(x, poly) {
  crds <- x
  bb = bbox(poly)
  rw = as.numeric(t(bbox(poly)))
  z <- deldir(crds$x, crds$y ,rw=rw)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  
  SpatialPolygonsDataFrame(
    SP, data.frame(x=crds[,1], y=crds[,2], 
                   row.names=sapply(slot(SP, 'polygons'), 
                                    function(x) slot(x, 'ID'))))  
}

giraffe_voronoi <- voronoipolygons(fill_buff, giraffe_sp)

final <- gIntersection(giraffe_sp, giraffe_voronoi, byid=TRUE)

plot(final)

#get polygons from spatialpolygons
poly_list <- final@polygons

get_polys <- function(x) {
  pts_x <- x@Polygons[[1]]@coords[, 1]
  pts_y <- x@Polygons[[1]]@coords[, 2]
  area <- rep(x@Polygons[[1]]@area, length(pts_x))
  data.frame(x = pts_x, y = pts_y, area = area)            
}

poly_df <- map_dfr(poly_list, ~get_polys(.x), .id = "id")

ggplot(poly_df, aes(x = x, y = y, group = id)) + geom_polygon()

#save polygons if nice
saveRDS(poly_df, "giraffe_polys.rds")

#color
palette <- 
  sample(clpalettes('top'), 1)[[1]] %>% 
  swatch %>% 
  .[[1]]

#plot
ggplot(poly_df) +
  geom_polygon(aes(x = x, y = y, group = id, fill = area), size = 0.35, color = "#383131", show.legend = FALSE) +
  scale_fill_gradientn(colors=sample(palette, length(palette))) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#ff6600")) +
  scale_y_reverse()

ggsave("draonfly_voronoi9.png", device = "png", type = "cairo")
