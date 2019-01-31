library(RTriangle)
library(tidyverse)
library(colourlovers)
library(mgcv)

#######################################################################
##triangulate an image starting with points from select_points app
#######################################################################
#give your points here
giraffe <- readRDS("rds_files/sparse_bear_points.rds")

#border points
x <- giraffe$x
y <- giraffe$y
bounds <- cbind(x, y)

#S tells the order of how the points are connected, required for concavity
##to do: function to make S automatically
s1 <- 1:nrow(giraffe)
s2 <- 2:(nrow(giraffe)+1)
s2[nrow(giraffe)] <- 1

S <- as.matrix(cbind(s1, s2))

#plot the outside points and check it
ps <- pslg(P = bounds, S = S)
plot(ps)
rtriang <- triangulate(ps)
plot(rtriang)

#generate some points inside boundary
#more points for more regular polygons
#less points for more irregular polygons
x <- runif(5000, min = min(giraffe$x), max = max(giraffe$x))
y <- runif(5000, min = min(giraffe$y), max = max(giraffe$y))

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

#second param is min dist between points
#smaller dist means smaller polygons, and vice versa
fill_df_buffer <- buffer.f(fill_df, 10, 1000)

x <- fill_df_buffer$x
y <- fill_df_buffer$y

#remove points not within border
is_inside <- inSide(giraffe, x, y)

fill <- tibble(x, y, is_inside) %>%
  filter(is_inside)

inners <- cbind(fill$x, fill$y)

#PB defines which poins are borders, which are not (1 is border, 0 not)
df <- giraffe
df2 <- fill[, 1:2]
df$PB <- 1
df2$PB <- 0

#setting up rtriangle data
x_comb <- c(df$x, df2$x)
y_comb <- c(df$y, df2$y)
PB <- c(df$PB, df2$PB)
xy_comb <- cbind(x_comb, y_comb)

#triangulate
ps <- pslg(P = xy_comb, PB = PB, S = S)
plot(ps)
rtriang <- triangulate(ps)
plot(rtriang)

################################################################
##getting polygons from triangles
## T variable gives vertex indicies of triangles, P gives points
################################################################

#extract points
pts_df <- tibble(id = 1:(length(rtriang$P)/2), x = rtriang$P[, 1], y = rtriang$P[, 2])

#get triangle vertex indices
triangles <- as.data.frame(rtriang$T)
tri_df <- tibble(id_1 = triangles$V1, id_2 = triangles$V2, id_3 = triangles$V3, group = 1:nrow(triangles))

#join triangle vertex indices with vertex points
tri_list <- split(tri_df, tri_df$group) %>%
  map( ~select(., 1:3)) %>%
  map( ~as.tibble(t(.))) %>%
  map( ~inner_join(., pts_df, by = c("V1" = "id"))) %>%
  map( ~select(., x, y))

#function to get area of triangle from 3 points
triang_area <- function(data) {
  x <- data$x
  y <- data$y
  mat <- matrix(data = c(1,1,1,x[1],x[2],x[3],y[1],y[2],y[3]), nrow = 3, ncol = 3, byrow = TRUE)
  area <- 0.5*det(mat)
  return(area)
}

#add area to each triangle and do some reshuffling
tri_list %>%
  map( ~mutate(.x, area = triang_area(.x))) %>%
  bind_rows(.id = "id") %>%
  select(id, x, y, area) -> triang_df

#here save polygons for use later in animation or whatever
saveRDS(triang_df, "bear_polys.rds")


################################
#plotting
################################

#random colors
palette <- 
  sample(clpalettes('top'), 1)[[1]] %>% 
  swatch %>% 
  .[[1]]

#plot w/ random color palette
ggplot(triang_df, aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, color=area, group = id), 
               show.legend = FALSE, size=0)+
  scale_fill_gradientn(colors=sample(good_pal, length(good_pal))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() +
  scale_y_reverse()

ggsave("stag_1.png", device = "png", type = "cairo")


#a different color palette
mid_mod <- c("#ECA069", "#CC6135", "#E9184C", "#9A151A", 
             "#F9C802", "#95AC80", "#71BC91", "#9CDAB5", 
             "#3D73D3", "#FAF1E0", "#FCE2D5", "#D8D4D5", 
             "#445DAD", "#747900")

ggplot(triang_df, aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, color=area, group = id), 
               show.legend = FALSE, size=0)+
  scale_fill_gradientn(colors = sample(mid_mod, length(mid_mod))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() +
  scale_y_reverse() +
  theme(panel.background = element_rect(fill = "#F7E5D1"))

ggsave("stag_5.png", device = "png", type = "cairo")


#messing with look of plot, add border to polygons
ggplot(triang_df, aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, group = id), 
               color = "white", show.legend = FALSE, size=1)+
  scale_fill_gradientn(colors=sample(palette, length(palette))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() +
  scale_y_reverse()

ggsave("longhorn_triangle3.png", device = "png", type = "cairo", width = 7, height = 5, units = "in")