library(tidyverse)
library(rlang)
library(poissoned)
library(deldir)
library(rlist)
library(magrittr)
library(polyclip)
library(mgcv)


####################################
###Some general purpose functions###
####################################

#takes a dataframe of polygons (with id column) and dataframe of points (x, y)
#and returns a dataframe of points that are inside the polygons and classified
#by the id of the original polygon. Format (x, y, id)
texturize <- function(polys, pts) {
  polys %>%
    select(x, y) %>%
    split(., polys$id) %>%
    map(., ~in.out(as.matrix(.x), as.matrix(pts))) %>%
    map(., ~cbind(.x, pts)) %>%
    map(., ~rename(.x, "inout" = ".x")) %>%
    map(., ~filter(.x, inout == TRUE)) %>%
    bind_rows(.id = "id") %>%
    select(-inout)
}

#functions for reformatting deldir outputs
cleanup <- function(x) x[ !names(x) %in% c("pt", "ptNum", "area", "id")]
cleanup2 <- function(x) x[ !names(x) %in% c("x", "y", "ptNum", "area", "id", "bp")]

#generates a set of voronoi polygons filtered by area
#number of seed points is num_points
#canvas size is x_max and y_max
#max_size and min_size is the max and min area of the resulting voronoi polygons
#polygons outside of the min/max will be removed
medium_voronois <- function(num_points = 50, x_max = 300, y_max = 800, max_size = 8000, min_size = 3000) {
  rand_pts <- data.frame(x = runif(num_points, 0, x_max), y = runif(num_points, 0, y_max))
  tess <- deldir(rand_pts)
  vor_list <- tile.list(tess) 
  
  vor_list_small <-
    vor_list %>%
    keep( ~ .x$area < max_size) %>%
    keep( ~ .x$area > min_size)
  
  vor_list_small %>%
    map( ~cleanup(.x)) %>%
    bind_rows(.id = "id") %>%
    select(id, x, y) %>%
    mutate(id = as.numeric(id))
}

#generates a set of voronoi polygons with specified num of seed pts and canvas size
#then randomly selects a certain number (num_shapes) to keep, and discards the rest
rand_voronois <- function(num_points = 50, x_max = 300, y_max = 800, num_shapes = 10) {
  rand_pts <- data.frame(x = runif(num_points, 0, x_max), y = runif(num_points, 0, y_max))
  tess <- deldir(rand_pts)
  vor_list <- tile.list(tess) 
  
  vor_list_sample <- list.sample(vor_list, num_shapes)
  
  vor_list_sample %>%
    map( ~cleanup(.x)) %>%
    bind_rows(.id = "id") %>%
    select(id, x, y) %>%
    mutate(id = as.numeric(id))
}

#takes an input dataframe of seed points (x, y) and returns the voronoi tessellation as 
#a dataframe with (x, y, id) ready for ggplotting
voronize <- function(data) {
  tess <- deldir(as.data.frame(data))
  vor_list <- tile.list(tess) 
  
  vor_list %>%
    map( ~cleanup(.x)) %>%
    bind_rows(.id = "id") %>%
    select(id, x, y) %>%
    mutate(id = as.numeric(id))
}

#takes a set of points, and returns the points with their associated voronoi polygon IDs
#only reason this exists is to correlate polygon ID with seed point ID
get_seeds <- function(data) {
  tess <- deldir(as.data.frame(data))
  vor_list <- tile.list(tess) 
  
  vor_list %>%
    map( ~as.data.frame(t(unlist(cleanup2(.x))))) %>%
    bind_rows(.id = "id") %>%
    select(id, pt.x, pt.y) %>%
    mutate(id = as.numeric(id))
}

#takes a dataframe of polygons and applies a shape manipulation 
#polyclip::polyoffset has details on the parameters
shapify <- function(data, delta, jointype, miterlim = 2) {
  x_new <- split(data$x, data$id)
  y_new <- split(data$y, data$id)
  polygons <- Map(list, x = x_new, y = y_new)
  
  polygons2 <- lapply(polygons, polyoffset, delta = delta,
                      jointype = jointype, miterlim = miterlim)
  
  polygons2 %>%
    map(~as.data.frame(.x)) %>%
    bind_rows(.id = "id")
}

#list of color palettes
pals <- list(
  pal1 = colorRampPalette(colors = c("#daeed8", "#A4C990")),
  pal2 = colorRampPalette(colors = c("#FAEBD7", "#FF7373")),
  pal3 = colorRampPalette(colors = c("#B0E0E6", "#323232")),
  pal6 = colorRampPalette(colors = c("#efe1ff", "#bd3037"))
)

#takes an input dataframe of points with (x, y, id)
#and adds a color column that will make each set of points (each id group)
#a random gradient palette from the list of `pals`
colorize <- function(data) {
  dir <- sample(exprs(x, y, desc(x), desc(y)))[[1]]
  pal <- sample(pals)[[1]]
  
  data %>% 
    arrange(!!dir) %>%
    mutate(color = pal(nrow(data)))
}

#draws num_points in a circle, badly
#min_r and max_r are the min and max radius possible
#jitter is the x y randomness
bad_circle <- function(num_pts = 10, min_r, max_r, jitter) {
  tibble(angle = seq(0, 2*pi, length.out = num_pts), r = sample(seq(min_r, max_r, length.out = 100), num_pts, replace = TRUE)) %>%
    mutate(x_jitter = sample(seq(-jitter, jitter, length.out = 100), num_pts, replace = TRUE), 
           y_jitter = sample(seq(-jitter, jitter, length.out = 100), num_pts, replace = TRUE),
           x = r*cos(angle) + x_jitter, 
           y = r*sin(angle) + y_jitter) %>%
    select(x, y) 
}

#draw concentric circles of pebbles then select some to keep
circle_pebbles <- function(num_pts = 10, min_r1 = 50, max_r1 = 100, jitter_1 = 100, min_r2 = 200, max_r2 = 300, jitter_2 = 100,
                           min_r3 = 400, max_r3 = 500, jitter_3 = 100, expand = -30, round = 20, num_keepers = 8, probs = NULL) {
  circle1 <- bad_circle(num_pts, min_r1, max_r1, jitter_1)
  circle2 <- bad_circle(num_pts, min_r2, max_r2, jitter_2)
  circle3 <- bad_circle(num_pts, min_r3, max_r3, jitter_3)
  
  all_circles <- rbind(circle1, circle2, circle3)
  
  circular_layer <- voronize(all_circles) %>%
    shapify(delta = expand, jointype = "miter", miterlim = 1000) %>%
    shapify(delta = round, jointype = "round")
  
  keepers <- sample(1:30, num_keepers, prob = probs)
  
  seeds <- get_seeds(all_circles) %>%
    filter(id %in% keepers)
  
  pebbles <- 
    circular_layer %>%
    filter(id %in% keepers)
  
  list(seeds = seeds, pebbles = pebbles)
}

##############################################
##############################################
##############################################

##make some gradient colored pebbles
salt <- poisson_disc(ncols = 800, nrows = 2000, cell_size = 0.5, verbose = TRUE)
salt$x <- salt$x - 50
salt$y <- salt$y - 60

shapes <- medium_voronois(max_size = 11000) %>%
  shapify(delta = -30, jointype = "miter", miterlim = 1000) %>%
  shapify(delta = 20, jointype = "round") %>%
  texturize(salt)

tex_colored <- 
  split(shapes, shapes$id) %>%
  map( ~colorize(.x)) %>%
  bind_rows()

ggplot() +
  geom_point(data = tex_colored, aes(x = x, y = y, color = color), alpha = 0.8, size = 2) +
  scale_color_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = "#EEE0E5"))

ggsave("gradient_rocks.png", height = 11, width = 6)


##try a new layout
probs1 <- c(0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03)

circ_layer1 <- circle_pebbles(probs = probs1, num_keepers = 3)
circ_layer2 <- circle_pebbles(probs = probs1, num_keepers = 3)
circ_layer3 <- circle_pebbles(probs = probs1, num_keepers = 3)
circ_layer4 <- circle_pebbles(probs = probs1, num_keepers = 2)
circ_layer5 <- circle_pebbles(probs = probs1, num_keepers = 2)

ggplot() +
  geom_segment(data = circ_layer1[["seeds"]], aes(x = pt.x, y = pt.y, xend = 0, yend = 0), color = "black", linetype = "dotted", alpha = 0.8) +
  geom_segment(data = circ_layer2[["seeds"]], aes(x = pt.x, y = pt.y, xend = 0, yend = 0), color = "black", linetype = "dotted", alpha = 0.8) +
  geom_segment(data = circ_layer3[["seeds"]], aes(x = pt.x, y = pt.y, xend = 0, yend = 0), color = "black", linetype = "dotted", alpha = 0.8) +
  geom_segment(data = circ_layer4[["seeds"]], aes(x = pt.x, y = pt.y, xend = 0, yend = 0), color = "black", linetype = "dotted", alpha = 0.8) +
  geom_segment(data = circ_layer5[["seeds"]], aes(x = pt.x, y = pt.y, xend = 0, yend = 0), color = "black", linetype = "dotted", alpha = 0.8) +
  geom_polygon(data = circ_layer1[["pebbles"]], aes(x = x, y = y, group = id), fill = "#10628E", alpha = 0.85) +
  geom_polygon(data = circ_layer2[["pebbles"]], aes(x = x, y = y, group = id), fill = "#D42A20", alpha = 0.85) +
  geom_polygon(data = circ_layer3[["pebbles"]], aes(x = x, y = y, group = id), fill = "#FAC12C", alpha = 0.85) +
  geom_polygon(data = circ_layer4[["pebbles"]], aes(x = x, y = y, group = id), fill = "black", alpha = 0.85) +
  geom_polygon(data = circ_layer5[["pebbles"]], aes(x = x, y = y, group = id), fill = "white", alpha = 0.85) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#FCF3D9"))


ggsave("byrne_pebbles_8.png", height = 8, width = 8)


##make some textured pebbles
##try a new layout
probs2 <- c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)
pebble_colors <- c('#20342a', '#f74713', '#686d2c', '#e9b4a6')
tex_colors <- c('#4d3d9a', '#f76975', '#ffffff', '#eff0dd')
pts <- poisson_disc(ncols = 400, nrows = 400, cell_size = 5.5, k = 10, verbose = TRUE)
pts$x <- pts$x - 1000
pts$y <- pts$y - 1000

circ_layer6 <- circle_pebbles(probs = probs2, num_keepers = 20)[["pebbles"]] %>%
  group_by(id) %>%
  group_map( ~mutate(.x, color = sample(pebble_colors, 1))) %>%
  ungroup()

tex_colored <- 
  texturize(circ_layer6, pts)  %>%
  mutate(size = sample(seq(0.03, 0.6, length.out = 100), size = nrow(.), replace = TRUE)) %>%
  group_by(id) %>%
  group_map( ~mutate(.x, color = sample(tex_colors, 1))) %>%
  ungroup()

tex_random <- 
  tex_colored %>%
  sample_n(nrow(tex_colored) / 10) %>%
  mutate(color = sample(tex_colors, nrow(.), replace = TRUE))

tex_final <- left_join(tex_colored, tex_random, by = c("x", "y")) %>%
  mutate(color.x = ifelse(is.na(color.y), color.x, color.y)) %>%
  select(id = id.x, x, y, size = size.x, color = color.x)

ggplot() +
  geom_polygon(data = circ_layer6, aes(x = x, y = y, group = id, fill = color)) +
  geom_point(data = tex_final, aes(x = x, y = y, size = size, color = color)) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_size_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = "#e5ded8"))

ggsave("textured_pebbles3.png", height = 10, width = 10)



