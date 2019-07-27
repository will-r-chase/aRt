library(tidyverse)
library(gganimate)
library(scales)

#read polygon dataframes
stag <- readRDS("January_tessellated_menagerie/rds_files/stag_polys.rds")
dragonfly <- readRDS("January_tessellated_menagerie/rds_files/dragonfly_polys.rds")
bear <- readRDS("January_tessellated_menagerie/rds_files/bear_polys.rds")
giraffe <- readRDS("January_tessellated_menagerie/rds_files/giraffe_polys.rds")

#colors
mondrian <- c("#F7F4EF", "#F7F4EF", "#F7F4EF", "#ffc214", "#d0191f", "#3D487E", "#070A03")
#add random colors (could do by area too)
bear$col <- sample(mondrian, nrow(bear), replace = TRUE)
dragonfly$col <- sample(mondrian, nrow(dragonfly), replace = TRUE)
giraffe$col <- sample(mondrian, nrow(giraffe), replace = TRUE)
stag$col <- sample(mondrian, nrow(stag), replace = TRUE)

#add states 
stag$state <- 1
giraffe$state <- 4
bear$state <- 3
dragonfly$state <- 2

#scale all to be roughly the same
bear$x_new <- rescale(bear$x, c(0,1.25))
bear$y_new <- rescale(bear$y, c(0,1))
ggplot(bear, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=col), size = 0.3, color = "black") +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

dragonfly$x_new <- rescale(dragonfly$x, c(0,1.2))
dragonfly$y_new <- rescale(dragonfly$y, c(0,1))
ggplot(dragonfly, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=col), size = 0.3, color = "black") +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

stag$x_new <- rescale(stag$x, c(0,1.2))
stag$y_new <- rescale(stag$y, c(0,1.2))
ggplot(stag, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=col), size = 0.3, color = "black") +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

giraffe$x_new <- rescale(giraffe$x, c(0,1.1))
giraffe$y_new <- rescale(giraffe$y, c(0,1))
ggplot(giraffe, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=col), size = 0.3, color = "black") +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

#arrange stag>dragonfly>bear>giraffe top to bot
dragonfly$y_new <- dragonfly$y_new + 1.2
bear$y_new <- bear$y_new + 2.2
giraffe$y_new <- giraffe$y_new + 3.2

#randomize ids to make animation look better
#also add dummy polygons to make all equal
#define x is the largest of your dataframes (most polygons)
x <- 505

stag_split <- split(stag, stag$id)
dummy_stag <- sample(stag_split, x-length(stag_split))
stag_split <- c(stag_split, dummy_stag)
new_id1 <- sample(1:length(stag_split), length(stag_split))
y1 <- 1:length(stag_split)
stag_randomized <- map2_dfr(stag_split, y1, ~mutate(.x, new_id = new_id1[.y]))

dragonfly_split <- split(dragonfly, dragonfly$id)
dummy_dragonfly <- sample(dragonfly_split, x-length(dragonfly_split))
dragonfly_split <- c(dragonfly_split, dummy_dragonfly)
new_id2 <- sample(1:length(dragonfly_split), length(dragonfly_split))
y2 <- 1:length(dragonfly_split)
dragonfly_randomized <- map2_dfr(dragonfly_split, y2, ~mutate(.x, new_id = new_id2[.y]))

bear_split <- split(bear, bear$id)
dummy_bear <- sample(bear_split, x-length(bear_split))
bear_split <- c(bear_split, dummy_bear)
new_id3 <- sample(1:length(bear_split), length(bear_split))
y3 <- 1:length(bear_split)
bear_randomized <- map2_dfr(bear_split, y3, ~mutate(.x, new_id = new_id3[.y]))

giraffe_split <- split(giraffe, giraffe$id)
dummy_giraffe <- sample(giraffe_split, x-length(giraffe_split))
giraffe_split <- c(giraffe_split, dummy_giraffe)
new_id4 <- sample(1:length(giraffe_split), length(giraffe_split))
y4 <- 1:length(giraffe_split)
giraffe_randomized <- map2_dfr(giraffe_split, y4, ~mutate(.x, new_id = new_id4[.y]))

#manual scale y reverse for animate
#view_follow doesn't work with scale_y_reverse
stag_randomized$y_new = -stag_randomized$y_new
dragonfly_randomized$y_new = -dragonfly_randomized$y_new
bear_randomized$y_new = -bear_randomized$y_new
giraffe_randomized$y_new = -giraffe_randomized$y_new

#combine randomized dfs and add transitions
combined_rand <- rbind(stag_randomized, dragonfly_randomized, bear_randomized, giraffe_randomized)

anim1 <-
  ggplot(combined_rand, aes(x=x_new,y=y_new,group=new_id)) +
  geom_polygon(aes(fill=col), size = 0.3, color = "black") +  
  scale_fill_identity() + 
  coord_equal() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#FAF1E0")) +
  transition_states(
    states = state,
    transition_length = 2,
    state_length = 1,
    wrap = FALSE
  ) +
  ease_aes('exponential-in')+
  view_step(pause_length = 2, step_length = 1, nsteps = 4, wrap = FALSE, ease = "exponential-in")

#animate and save
animate(anim1, nframes = 150, fps = 10, detail = 2)
anim_save("shattered_menagerie_improved.gif", device = "png", type = "cairo", height = 7, width = 7, units = "in")
