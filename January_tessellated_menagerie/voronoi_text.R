library(tidyverse)
library(colourlovers)
library(deldir)
library(sp)
library(rgeos)
library(imager)
library(mgcv)

#load sparse border
giraffe <- readRDS("text_polys.rds")

giraffe$x <- giraffe$x_new
giraffe$y <- giraffe$y_new

#remove dots to make it easier on ourselves

giraffe_clean <- giraffe %>% filter(pathGroup != "i.1") %>% filter(pos != 8) %>% filter(pos != 9)
dots <- giraffe %>% filter(pathGroup == "i.1" | pos == 8 | pos == 9)

ggplot(dots2, aes(x = x, y = y, group = id)) + geom_polygon()

letters_list <- split(giraffe_clean, giraffe_clean$pos)

#func for all
voronate <- function(data) {
  x <- runif(1200, min = min(data$x), max = max(data$x))
  y <- runif(1200, min = min(data$y), max = max(data$y))
  
  fill_df <- data.frame(x = x, y = y)
  
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
  
  fill_df_buffer <- buffer.f(fill_df, 0.07, 500)
  print("buffered complete!")
  x <- fill_df_buffer$x
  y <- fill_df_buffer$y
  
  is_inside_buff <- inSide(data, x, y)
  
  fill_buff <- tibble(x, y, is_inside_buff) %>%
    filter(is_inside_buff)
  
  r2 <- data %>% select(x, y) %>% distinct()
  
  r_sp <- SpatialPolygons(list(Polygons(list(Polygon(r2)), ID=1)))
  print("spatial polygons conversion complete!")
  voronoipolygons <- function(x, poly) {
    require(deldir)
    crds <- x
    bb = sp::bbox(poly)
    rw = as.numeric(t(sp::bbox(poly)))
    z <- deldir(crds$x, crds$y,rw=rw)
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
  
  r_voronoi <- voronoipolygons(fill_buff, r_sp)
  print("voronoi polygons complete!")
  final <- gIntersection(r_sp, r_voronoi, byid=TRUE)
  print("intersection complete!")
  poly_list <- final@polygons
  
  get_polys <- function(x) {
    pts_x <- x@Polygons[[1]]@coords[, 1]
    pts_y <- x@Polygons[[1]]@coords[, 2]
    area <- rep(x@Polygons[[1]]@area, length(pts_x))
    data.frame(x = pts_x, y = pts_y, area = area)            
  }
  
  poly_df <- map_dfr(poly_list, ~get_polys(.x), .id = "id")
  return(poly_df)
}

voronoi_list <- map(letters_list, ~voronate(.x))

#reinsert dots
dots$area <- 0.0066
dots2 <- dots %>% 
  select(pathGroup, x, y, pos, area) %>%
  group_by(pathGroup) %>%
  mutate(ID = case_when(
    pathGroup == "i.1" ~ 1,
    pos == "8" & pathGroup == ":.1" ~ 2,
    pos == "8" & pathGroup == ":.2" ~ 3,
    pos == "9" & pathGroup == ":.1" ~ 4,
    pos == "9" & pathGroup == ":.2" ~ 5,
  )) %>%
  ungroup() %>%
  select(-c("pos", "pathGroup")) %>%
  mutate(id = "1") %>%
  as.data.frame()

dots2_list <- split(dots2, dots2$ID)
dots2_list <- map(dots2_list, ~select(., x, y, area, id))
names(dots2_list) <- c("18", "19", "20", "21", "22")

text_list <- c(voronoi_list, dots2_list)

text_df <- bind_rows(text_list, .id = "group")

text_list2 <- map(text_list, ~modify_if(., is.character, as.numeric))
for(i in 2:length(text_list2)){
  text_list2[[i]]$id <- text_list2[[i]]$id + max(text_list2[[i-1]]$id)
}
text_df2 <- bind_rows(text_list2, .id = "group")

palette <- 
  sample(clpalettes('top'), 1)[[1]] %>% 
  swatch %>% 
  .[[1]]

r_pal <- c("#8fb9ff", "#3f6896", "#6a97c1", "#8aaddd", "#204374")

ggplot(text_df2) +
  geom_polygon(aes(x = x, y = y, group = id, fill = area), size = 0, show.legend = FALSE) +
  #scale_fill_gradientn(colors=giraffe_col) +
  scale_fill_gradientn(colors=r_pal) +
  theme_void()

###for one
r <- giraffe %>% filter(pos == 1)

x <- runif(1200, min = min(r$x), max = max(r$x))
y <- runif(1200, min = min(r$y), max = max(r$y))

fill_df <- data.frame(x = x, y = y)

fill_df_buffer <- buffer.f(fill_df, 0.07, 500)

x <- fill_df_buffer$x
y <- fill_df_buffer$y

is_inside_buff <- inSide(r, x, y)

fill_buff <- tibble(x, y, is_inside_buff) %>%
  filter(is_inside_buff)

r2 <- r %>% select(x, y) %>% distinct()

r_sp <- SpatialPolygons(list(Polygons(list(Polygon(r2)), ID=1)))

voronoipolygons <- function(x, poly) {
  require(deldir)
  crds <- x
  bb = sp::bbox(poly)
  rw = as.numeric(t(sp::bbox(poly)))
  z <- deldir(crds$x, crds$y,rw=rw)
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

r_voronoi <- voronoipolygons(fill_buff, r_sp)

final <- gIntersection(r_sp, r_voronoi, byid=TRUE)

poly_list <- final@polygons

get_polys <- function(x) {
  pts_x <- x@Polygons[[1]]@coords[, 1]
  pts_y <- x@Polygons[[1]]@coords[, 2]
  area <- rep(x@Polygons[[1]]@area, length(pts_x))
  data.frame(x = pts_x, y = pts_y, area = area)            
}

poly_df <- map_dfr(poly_list, ~get_polys(.x), .id = "id")

###
palette <- 
  sample(clpalettes('top'), 1)[[1]] %>% 
  swatch %>% 
  .[[1]]

ggplot(poly_df) +
  geom_polygon(aes(x = x, y = y, group = id, fill = area), size = 0, show.legend = FALSE) +
  #scale_fill_gradientn(colors=giraffe_col) +
  scale_fill_gradientn(colors=sample(palette, length(palette))) +
  theme_void()

im <- load.image("inputs/rstudio_conf_poly.png") %>% grayscale()

# Filter image to convert it to bw
# sample points within image
im %>%
  #threshold("30%") %>% 
  as.cimg() %>% 
  plot() %>%
  as.data.frame() %>%
  # filter(x > 10 & x < 490) %>%
  # filter(y > 10 & y < 340) %>%
  sample_n(7000, weight = (1 - value)) %>% 
  select(x, y) -> df

#funct to space out points
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

df_buffer <- buffer.f(df, 35, 500)

#check plot
ggplot(df_buffer, aes(x=x, y=y)) + geom_point() + scale_y_reverse() + theme_void()

#to constrain voronoi tesselation to border
#convert border to SpatialPolygons
#then use deldir to get voronoi polygons
#get intersection of the two w/ gIntersect
#from https://stackoverflow.com/a/24237509/7712222
giraffe2 <- giraffe %>% select(x, y, pos) %>% distinct()
giraffe3 <- giraffe2 %>% select(x, y)

giraffe_list <- split(giraffe3, giraffe2$pos) 

ps <- lapply(giraffe_list, Polygon)

p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), 
                                                 ID = names(giraffe_list)[i]  ))

# create SpatialPolygons object
my_spatial_polys <- SpatialPolygons(p1)

voronoipolygons <- function(x, poly) {
  crds <- x
  bb = sp::bbox(poly)
  rw = as.numeric(t(sp::bbox(poly)))
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

df_fill <- data.frame(x = rescale(df_buffer$x, c(0, 10)), y = rescale(df_buffer$y, c(-1, 1)))

giraffe_voronoi <- voronoipolygons(df_fill, my_spatial_polys)

final <- gIntersection(giraffe_sp, giraffe_voronoi, byid=TRUE)

plot(final)
anyDuplicated(df_fill)
#get polygons from spatialpolygons
poly_list <- final@polygons

get_polys <- function(x) {
  pts_x <- x@Polygons[[1]]@coords[, 1]
  pts_y <- x@Polygons[[1]]@coords[, 2]
  area <- rep(x@Polygons[[1]]@area, length(pts_x))
  data.frame(x = pts_x, y = pts_y, area = area)            
}

poly_df <- map_dfr(poly_list, ~get_polys(.x), .id = "id")

#color
line_col <- c("#F9EECD", "#FEFCD8")
fill_col <- c("#562402", "#ca7f4f")
giraffe_col <- c("#683C0F", "#683C0F", "#683C0F", "#D15F02", "#D15F02", "#FEAE03", "#F7AD00")

set.seed(NULL)
palette <- 
  sample(clpalettes('top'), 1)[[1]] %>% 
  swatch %>% 
  .[[1]]

#found good pal for insects
insect_wings <- palette
insect <- c("#E8DDCB", "#CDB380", "#036564", "#033649", "#031634")


set.seed(4339)
#plot
ggplot(poly_df) +
  geom_polygon(aes(x = x, y = y, group = id, fill = area), size = 1.5, color = "#FFE2B7", show.legend = FALSE) +
  #scale_fill_gradientn(colors=giraffe_col) +
  scale_fill_gradientn(colors=sample(palette, length(palette))) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#F7E5D1")) +
  scale_y_reverse()

ggsave("giraffe_voronoi_wine.png", device = "png", type = "cairo")