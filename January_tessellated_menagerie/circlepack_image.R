## barely modified from 
## https://chichacha.netlify.com/2018/12/22/bubble-packed-chart-with-r-using-packcircles-package/

library(tidyverse) ## I need tidyverse for everything :) 
library(imager) ## to create data frame from image
library(scales) ## rescale function is so handy!
library(packcircles) ## making circle packing easy! 

## Step 1
im <- load.image("inputs/austin.jpg")
plot(im)

## Convert Image into Data Frame
im.df.colour <- im %>%
  as.data.frame(wide="c") %>% ## so that rgb value is in separate column.
  rename(im_x=x,im_y=y) %>%
  mutate(hex=rgb(c.1,c.2,c.3))

## Step 2 using circleProgressiveLayout function.
## Generate circle packing layout using rbeta distribution as size of circles
pack_layout <- circleProgressiveLayout(rbeta(5000,1,2), sizetype='area') %>% 
  ## Step 3 - I want to figure out what colour to use, so I want layout & image df to have same scaling. 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour$im_y))),
         ## also generate id, so i can join the data frame easily later!
         id=row_number()) %>% 
  inner_join(im.df.colour %>% select(im_x,im_y,hex), by=c("im_x","im_y"))

#for smaller circles inside
pack_layout2 <- 
  pack_layout %>%
  mutate(r = sqrt(x^2 + y^2), ## calculate distance from 0,0 coordinate 
       angle_t = atan2(y,x),  ## The arc-tangent of two arguments atan2(y, x) returns the angle between the x-axis and the vector from the origin to (x, y)
       angle = rescale(angle_t, from=c(-pi,pi))) ## convert theta value to value betwwen 0 and 1
    

## Step 4 
## Using the layout above create data frame using circleLayoutVertices function so that you can plot circle using ggplot2
data_gg <- circleLayoutVertices(pack_layout) %>% 
  inner_join(pack_layout %>% select(id,hex), by=c("id"))

#for smaller circles inside
data_gg2 <- circleLayoutVertices(pack_layout2) %>% 
  inner_join(pack_layout2 %>% select(id,hex), by=c("id"))

data_gg2_1 <- circleLayoutVertices(pack_layout2 %>% 
                                     sample_n(800, weight=radius) %>%
                                     mutate(radius=0.7*radius), npoints = 25) ## I want to draw smaller circle, so shrink the radius
data_gg2_2 <- circleLayoutVertices(pack_layout2 %>% 
                                     sample_n(700,weight=radius) %>%
                                     mutate(radius=0.5*radius), npoints = 25) 
data_gg2_3 <- circleLayoutVertices(pack_layout2 %>% 
                                     sample_n(900,weight=radius) %>%
                                     mutate(radius=0.3*radius), npoints = 25)

## plot bubblles
data_gg %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 

ggsave("armadillo_bubbles.png", device = "png", type = "cairo")

#with smalller circles inside
data_gg2 %>% 
  ggplot(aes(x=x,y=y, group=id)) + 
  geom_polygon(aes(fill=hex)) +  
  geom_path(data=data_gg2, size=0.5, color="#ffffff90") +
  geom_path(data=data_gg2_1,size=1, color="#ffffff90") +
  geom_path(data=data_gg2_2,size=0.5, color="#ffffff90") +
  geom_path(data=data_gg2_3,size=0.5, color="#ffffff90") +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  coord_fixed() +
  scale_y_reverse()

ggsave("armadillo_circles.png", device = "png", type = "cairo")

saveRDS(data_gg, "austin_points.rds")
