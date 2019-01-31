library(tidyverse)
library(gganimate)
library(colourlovers)
library(scales)

##sorry, this code is a dumpster fire and I don't have time to clean it up
##DM me on twitter @W_R_Chase if you need to know something

armadillo <- readRDS("rds_files/armadillo_points.rds")
longhorn <- readRDS("rds_files/longhorn_points.rds")
rstudio <- readRDS("rds_files/rstudio_points.rds")
austin <- readRDS("rds_files/austin_points.rds")

longhorn_pal <- c("#BC4E29", "#7F563C", "#B3805A", "#F6F7BD", "#B4B676", "#88BB9D", "#A29C7B",
                  "#BD5832", "#A2B888", "#ECBB4B", "#E4AD2D", "#81563C", "#AA906D", "#D4B04C",
                  "#B4502C", "#95AB8E", "#76563F", "#925437", "#9D5333", "#BFB469", "#B86F48", 
                  "#AA512F", "#B47C56", "#B54F2C", "#8B5539", "#B74F2B", "#965436")
austin_pal <- c("#8fb9ff", "#204374", "#B22234", "#b21616", "#7C0F0F", "#1833B5", "#011f4b", "#03396c", "#0000ff", "#0e68ce", "#0c457d", "#13066d", "#060a47", "#05acc8")

longhorn$hex <- sample(longhorn_pal, nrow(longhorn), replace = TRUE)

austin2 <- austin %>% filter(hex == "#FEFEFE")
austin2$hex <- sample(austin_pal, nrow(austin2), replace = TRUE)

whites <- c("#FEFEFE", "#FFFFFF", "#FDFDFD", "#FCFCFC", "#F5F5F5", "#EBEBEB", 
            "#F4F4F4", "#FAFAFA", "#F7F7F7", "#F9F9F9", "#F8F8F8", "#F6F6F6", 
            "#FBFBFB", "#F1F1F1", "#E8E8E8", "#E9E9E9", "#F3F3F3", "#EAEAEA",
            "#E6E6E6", "#EDEDED", "#F2F2F2", "#DADADA", "#E5E5E5", "#F0F0F0",
            "#EFEFEF", "#E7E7E7", "#E4E4E4", "#ECECEC")

austin2 <- austin %>% filter(!(hex %in% whites))
austin2$hex <- sample(austin_pal, nrow(austin2), replace = TRUE)

armadillo2 <- armadillo %>% filter(!(hex %in% whites))

rstudio$hex <- "#005b96"

rstudio$y <- -rstudio$y

ggplot(rstudio, aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

ggplot(austin2, aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 

ggplot(longhorn, aes(x = x, y = y)) +
  geom_polygon(aes(group = id, fill = hex), 
               show.legend = FALSE, size=0)+
  scale_fill_identity() +
  coord_equal() +
  theme_void() +
  scale_y_reverse()

ggplot(armadillo2, aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 


maxes_x <- c(max(rstudio$x), max(armadillo$x), max(austin$x), max(longhorn$x))
maxes_y <- c(max(rstudio$y), max(armadillo$y), max(austin$y), max(longhorn$y))

mins_x <- c(min(rstudio$x), min(armadillo$x), min(austin$x), min(longhorn$x))
mins_y <- c(min(rstudio$y), min(armadillo$y), min(austin$y), min(longhorn$y))

names <- c("rstudio", "armadillo", "austin", "longhorn")

dims <- data.frame(names, maxes_x, maxes_y, mins_x, mins_y)

#try rescaling a couple
rstudio$x_new <- rescale(rstudio$x, c(0,1.5))
rstudio$y_new <- rescale(rstudio$y, c(0,1))
ggplot(rstudio, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=hex, size = size), color = "black") +  
  scale_fill_identity() + 
  scale_size_identity() +
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

armadillo2$x_new <- rescale(armadillo2$x, c(0,1.2))
armadillo2$y_new <- rescale(armadillo2$y, c(0,1))
ggplot(armadillo2, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

austin2$x_new <- rescale(austin2$x, c(0,1.5))
austin2$y_new <- rescale(austin2$y, c(0,1))
ggplot(austin2, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

longhorn$x_new <- rescale(longhorn$x, c(0,1.5))
longhorn$y_new <- rescale(longhorn$y, c(0,1))
ggplot(longhorn, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 


# #adding border 
# rstudio$size <- 0.5
# austin2$size <- 0
# armadillo2$size <- 0
# longhorn$size <- 0

#add states 
rstudio$state <- 1
armadillo2$state <- 1
austin2$state <- 2
longhorn$state <- 2

#add ids
rstudio$pic <- "rstudio"
armadillo2$pic <- "armadillo"
austin2$pic <- "austin"
longhorn$pic <- "longhorn"

# #adding color
# rstudio$col <- "#666666"
# austin2$col <- austin2$hex
# armadillo2$col <- armadillo2$hex
# longhorn$col <- longhorn$hex

ggplot(longhorn, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=hex), color = "black") +  
  scale_fill_identity() + 
  scale_size_identity() +
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

#arrange one below the other
armadillo2$y_new <- armadillo2$y_new + max(rstudio$y_new) + 0.2
armadillo2$x_new <- armadillo2$x_new + 0.2

armadillo3 <- armadillo2 %>% select(x_new, y_new, id, hex, state, pic)
rstudio2 <- rstudio %>% select(x_new, y_new, id, hex, state, pic)

armadillo_split <- split(armadillo3, armadillo3$id)
newid1 <- 1:length(armadillo_split)
y1 <- 1:length(armadillo_split)
armadillo4 <- map2_dfr(armadillo_split, y1, ~mutate(.x, new_id = newid1[.y]))

rstudio_split <- split(rstudio2, rstudio2$id)
newid2 <- 1:length(rstudio_split)
y2 <- 1:length(rstudio_split)
rstudio3 <- map2_dfr(rstudio_split, y2, ~mutate(.x, new_id = newid2[.y]))
rstudio3$id <- rstudio3$new_id
rstudio3 <- rstudio3 %>% select(-c("new_id"))

armadillo4$id <- armadillo4$new_id + max(rstudio3$id)
armadillo4 <- armadillo4 %>% select(-c("new_id"))

state1 <- rbind(rstudio3, armadillo4)

ggplot(state1, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

#arrange one below the other
longhorn$y_new <- longhorn$y_new + max(austin2$y_new) + 0.3
longhorn$x_new <- longhorn$x_new + 0.1

longhorn2 <- longhorn %>% select(x_new, y_new, id, hex, state, pic)
longhorn2$id <- as.numeric(longhorn2$id)
austin3 <- austin2 %>% select(x_new, y_new, id, hex, state, pic)

austin_split <- split(austin3, austin3$id)
newid3 <- 1:length(austin_split)
y3 <- 1:length(austin_split)
austin4 <- map2_dfr(austin_split, y3, ~mutate(.x, new_id = newid3[.y]))
austin4$id <- austin4$new_id
austin4 <- austin4 %>% select(-c("new_id"))

longhorn_split <- split(longhorn2, longhorn2$id)
newid4 <- 1:length(longhorn_split)
y4 <- 1:length(longhorn_split)
longhorn3 <- map2_dfr(longhorn_split, y4, ~mutate(.x, new_id = newid4[.y]))
longhorn3$id <- longhorn3$new_id
longhorn3 <- longhorn3 %>% select(-c("new_id"))

longhorn3$id <- longhorn3$id + max(austin4$id)

state2 <- rbind(austin4, longhorn3)

ggplot(state2, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

state2$x_new <- state2$x_new + max(state1$x_new)

all_states <- rbind(state1, state2)

state1_ids <- state1 %>% distinct(id) %>% pull(id)
state1_ids <- as.integer(state1_ids)
sanity <- 1:1642
identical(state1_ids, sanity)

state2_ids <- state2 %>% distinct(id) %>% pull(id)
state2_ids <- as.integer(state2_ids)
sanity <- 1:1741
identical(state2_ids, sanity)

state1_in_state2 <- state1_ids[state1_ids %in% state2_ids]
state2_not_state1 <- state2_ids[!(state2_ids %in% state1_ids)]

dummys <- data.frame(x_new = rep(c(0.5, 0.5000002, 0.5000001)), y_new = rep(c(1, 1, 1.000002)), id = rep(state2_not_state1, each = 3), hex = "#FFFFFF", state = 1, pic = "rstudio")

ggplot(dummys, aes(x = x_new, y = y_new, group = id)) +geom_polygon()

all_plus_dummys <- rbind(all_states, dummys)

anim <-
  ggplot(all_plus_dummys, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() +
  transition_states(state, transition_length = 2, state_length = 2) +
  ease_aes('cubic-in-out')

animate(anim, nframes = 100, fps = 10, detail = 2, type = "cairo", height = 1600, width = 1900)

anim_save("logo_test.gif")
#testing
#trying
state1_split <- split(state1, state1$id)
newid5 <- sample(1:length(state1_split), length(state1_split))
y5 <- 1:length(state1_split)
state1_randomized <- map2_dfr(state1_split, y5, ~mutate(.x, new_id = newid5[.y]))

state2_split <- split(state2, state2$id)
newid6 <- sample(1:length(state2_split), length(state2_split))
y6 <- 1:length(state2_split)
state2_randomized <- map2_dfr(state2_split, y6, ~mutate(.x, new_id = newid6[.y]))

all_states_randomized <- rbind(state1_randomized, state2_randomized)

ggplot(all_states_randomized, aes(x=x_new,y=y_new,group=new_id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

state1_split <- split(state1, state1$id)
newid1 <- 1:1642
y1 <- 1:length(state1_split)
state1_randomized <- map2_dfr(state1_split, y1, ~mutate(.x, new_id = newid1[.y]))

state2_split <- split(state2, state2$id)
newid2 <- 1:1741
y2 <- 1:length(state2_split)
state2_randomized <- map2_dfr(state2_split, y2, ~mutate(.x, new_id = newid2[.y]))

all_states_randomized <- rbind(state1_randomized, state2_randomized)

ggplot(all_states, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

#does this work? sort of, but doesn't look great
anim_random <-
  ggplot(all_states_randomized, aes(x=x_new,y=y_new,group=new_id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() +
  transition_states(state, transition_length = 2, state_length = 2) +
  ease_aes('cubic-in-out') +
  enter_grow() +
  enter_fade() +
  exit_shrink() +
  exit_fade()

animate(anim_random, nframes = 100, fps = 10, type = "cairo")

#checking stuff
rstudio_ids <- all_states %>% filter(pic == "rstudio") %>% distinct(id) %>% pull(id)
rstudio_ids <- as.integer(rstudio_ids)
sanity <- 1:338
identical(rstudio_ids, sanity)

state1_ids <- state1 %>% distinct(id) %>% pull(id)
state1_ids <- as.integer(state1_ids)
sanity <- 1:1642
identical(state1_ids, sanity)

state2_ids <- state2 %>% distinct(id) %>% pull(id)
state2_ids <- as.integer(state2_ids)
sanity <- 1:1741
identical(state2_ids, sanity)

state2_ids[!(state2_ids%in%sanity)]
state2_ids

austin_ids <- austin4 %>% distinct(id) %>% pull(id)
austin_ids <- as.integer(austin_ids)
sanity <- 1:1567
identical(austin_ids, sanity)
max(austin_ids)
length(austin_ids)

longhorn_ids <- longhorn3 %>% distinct(id) %>% pull(id)
longhorn_ids <- as.integer(longhorn_ids)
sanity <- 1:174
identical(longhorn_ids, sanity)
max(longhorn_ids)
length(longhorn_ids)

state1_in_state2 <- state1_ids[state1_ids %in% state2_ids]
state2_not_state1 <- state2_ids[!(state2_ids %in% state1_ids)]

#figured it out, some not in state 1 that are in state 2
all_states <- rbind(state1, state2) %>% filter(!(id %in% state2_not_state1))

#try adding dummy points to make equal # of polys in both states
dummys <- data.frame(x_new = rep(c(0.5, 0.5000002, 0.5000001)), y_new = rep(c(1, 1, 1.000002)), id = rep(state2_not_state1, each = 3), hex = "#FFFFFF", state = 1, pic = "rstudio")

ggplot(dummys, aes(x = x_new, y = y_new, group = id)) +geom_polygon()

all_plus_dummys <- rbind(all_states, dummys)

anim <-
  ggplot(all_plus_dummys, aes(x=x_new,y=y_new,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() +
  transition_states(state, transition_length = 2, state_length = 2) +
  ease_aes('cubic-in-out') +
  enter_grow() +
  enter_fade() +
  exit_shrink() +
  exit_fade()

animate(anim, nframes = 100, fps = 10, detail = 2, type = "cairo", height = 10, width = 12, units = "in")

anim_save("logo_test.gif")
