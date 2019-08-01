library(tidyverse)
library(poissoned)
library(mgcv)
library(deldir)

#we'll use these later for texture
#put them up here so we can rerun later without rerunning this part
salt <- poisson_disc(300, 800, 1, k = 10, verbose = TRUE)
pepper <- poisson_disc(300, 800, 1, k = 10, verbose = TRUE)

#generate random seed points and calculate the delaunay triangulation
rand_pts <- data.frame(x = runif(50, 0, 300), y = runif(50, 0, 800))
tess <- deldir(rand_pts)
triang <- triang.list(tess)

triang_area <- function(data) {
  x <- data$x
  y <- data$y
  mat <- matrix(data = c(1,1,1,x[1],x[2],x[3],y[1],y[2],y[3]), nrow = 3, ncol = 3, byrow = TRUE)
  area <- 0.5*det(mat)
  return(area)
}

#add area to each triangle and do some reshuffling
triang %>%
  map( ~mutate(.x, area = triang_area(.x))) %>%
  bind_rows(.id = "id") %>%
  select(id, x, y, area) -> triang_df

#filter out smaller triangles
big_triang <- 
  triang_df %>%
  group_by(id) %>%
  filter(area > 3000) %>%
  ungroup()

#set up the boundary for each triangle
boundary <- 
  split(big_triang, big_triang$id) %>%
  map( ~add_row(.x, id = NA, x = NA, y = NA, area = NA)) %>%
  bind_rows() %>%
  select(x, y)

#find the points inside and outside of the boundary
salt_inout <- in.out(as.matrix(boundary), as.matrix(salt))
salt_inside <- cbind(salt, salt_inout) %>%
  filter(salt_inout == TRUE)

pepper_inout <- in.out(as.matrix(boundary), as.matrix(pepper))
pepper_outside <- cbind(pepper, pepper_inout) %>%
  filter(pepper_inout == FALSE)

#color triangles
colors <- c("#E1BABA", "#FFDFE2", "#AAD8A8", "#8B9DC3", "#5C8492", "#B20937", "#E9FA77", "#D7EAAE", "#667788", 
            "#761409", "#FFDD4D", "#aebab7", "#a3a3a3")

big_triang <- 
  big_triang %>%
  group_by(id) %>%
  mutate(color = sample(colors, 1, replace = TRUE))

#plot
ggplot() +
  geom_polygon(data = big_triang, aes(x = x, y  = y, group = id, fill = color), color = "white", size = 1) +
  geom_point(data = salt_inside, aes(x = x, y = y), size = 0.001, color = "white", alpha = 0.15) +
  geom_point(data = pepper, aes(x = x, y = y), size = 0.1, color = "black", alpha = 1, shape = "*") +
  scale_fill_identity() +
  lims(x = c(0, 300), y = c(0, 800)) +
  theme_void()

ggsave("triangles_textured3.png", width = 8, height = 10)
