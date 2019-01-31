library(RTriangle)
library(tidyverse)
library(colourlovers)
library(gganimate)

#######################################################################
##attempt at making my own figure
#######################################################################

#define some points to be the borders
x <- c(0, 0, 1, 2, 2, 1, 0, 1, 2, 2, 1, 0)
y <- c(0, 2, 1.5, 2, 0, 0.5, 3, 3.5, 3, 5, 4.5, 5)
bounds <- cbind(x, y)

#S tells the order of how the points are connected, required for concavity
##to do: function to make S automatically
S <- rbind(c(1, 2), c(2, 3), c(3, 4), c(4, 5), c(5, 6), c(6,1), c(7, 8), c(8, 9), c(9, 10), c(10, 11), c(11, 12), c(12, 7))
ps <- pslg(P = bounds, S = S)
plot(ps)
rtriang <- triangulate(ps)
plot(rtriang)

#add some random points inside of boundary
x1 <- runif(6, min = 0.1, max = 1.9)
x2 <- runif(6, min = 0.1, max = 1.9)
y1 <- runif(6, min = 0.6, max = 1.4)
y2 <- runif(6, min = 3.6, max = 4.4)
x_fill <- c(x1, x2)
y_fill <- c(y1, y2)
inners <- cbind(x_fill, y_fill)

#PB defines which poins are borders, which are not (1 is border, 0 not)
df <- data.frame(bound_x = x, bound_y = y)
df2 <- data.frame(inner_x = x_fill, inner_y = y_fill)
df$PB <- 1
df2$PB <- 0

x_comb <- c(df$bound_x, df2$inner_x)
y_comb <- c(df$bound_y, df2$inner_y)
PB <- c(df$PB, df2$PB)
xy_comb <- cbind(x_comb, y_comb)

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

#colors
palette <- 
  sample(clpalettes('top'), 1)[[1]] %>% 
  swatch %>% 
  .[[1]]

good_pal <- c("#ECD078", "#D95B43", "#C02942", "#542437", "#53777A")

#plot w/ geom_polygon
ggplot(triang_df, aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, color=area, group = id), 
               show.legend = FALSE, size=0)+
  scale_fill_gradientn(colors=sample(good_pal, length(good_pal))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() 

#messing with look of plot, I like this, it's cool
ggplot(triang_df, aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, group = id), 
               color = "white", show.legend = FALSE, size=2)+
  scale_fill_gradientn(colors=sample(good_pal, length(good_pal))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() 

ggsave("constrained.png", device = "png", type = "cairo")

#############################################################
##testing animate############################################
#############################################################

#make top figure state 1, bottom figure state 2
triang_state_df <-
  triang_df %>%
  mutate(state = ifelse(y > 2, as.integer(1), as.integer(2)), 
         id = as.numeric(.$id)) %>%
  arrange(id)

#make each state have the same group indices for triangles
anim_df <- split(triang_state_df, triang_state_df$state) %>%
  map( ~mutate(., group = group_indices(., id))) %>%
  map( ~select(., x, y, area, state, group)) %>%
  bind_rows()

#animate between two states with transition_states
bowtie <- 
  anim_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, color=area, group = group), 
               show.legend = FALSE, size=0)+
  scale_fill_gradientn(colors=sample(palette, length(palette))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() + 
  ease_aes('cubic-in-out') +
  transition_states(state, transition_length = 2, state_length = 1, wrap = TRUE)

animate(bowtie, nframes = 100, fps = 10, detail = 2, type = "cairo")
anim_save("bowtie3.gif")

#try with white borders, kinda interesting
#looks kinda weird when tweening? maybe better for static plot
bowtie <- 
  anim_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, group = group), 
               color = "white", show.legend = FALSE, size=2)+
  scale_fill_gradientn(colors=sample(palette, length(palette))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() + 
  ease_aes('cubic-in-out') +
  transition_states(state, transition_length = 2, state_length = 1, wrap = TRUE)

animate(bowtie, nframes = 100, fps = 10, detail = 2, type = "cairo")