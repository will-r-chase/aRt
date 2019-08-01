library(tidyverse)
library(poissoned)
library(gganimate)

#generate a bunch of points around a center point with close distance and order of discovery kept
pts <- poisson_disc(ncols = 150, nrows = 400, cell_size = 2, xinit = 150, yinit = 750, keep_idx = TRUE) %>%
  arrange(idx)

#plot giving a color gradient based on order of discovery
ggplot(pts) +
  geom_point(aes(x = x, y = y, color = idx), size = 4, alpha = 0.9) +
  scale_color_gradientn(colors = c("#F37374", "#F48181", "#F58D8D","#FF9999", "#FFA3A3","#FFA699", "#FFB399", "#FFB399","#FFC099", "#FFC099","#FFCC99", "#FFCC99"), guide = "none") +
  theme_void() 

#static image
ggsave("sunrise.png", width = 5, height = 10)

#animate
anim <- 
  ggplot(pts) +
  geom_point(aes(x = x, y = y, color = idx, group = idx), size = 4, alpha = 0.9) +
  scale_color_gradientn(colors = c("#F37374", "#F48181", "#F58D8D","#FF9999", "#FFA3A3","#FFA699", "#FFB399", "#FFB399","#FFC099", "#FFC099","#FFCC99", "#FFCC99"), guide = "none") +
  theme_void()  +
  scale_y_reverse() +
  transition_reveal(along = idx) +
  ease_aes("cubic-in") +
  enter_grow() +
  enter_fade(alpha = 0.9)

animate(anim, nframes = 100, fps = 20)
anim_save("sunrise.gif")

#one more
#generate a bunch of points around a center point with close distance and order of discovery kept
pts <- poisson_disc(ncols = 150, nrows = 400, cell_size = 2, xinit = 150, yinit = 750, keep_idx = TRUE) %>%
  arrange(idx)

#plot giving a color gradient based on order of discovery
ggplot(pts) +
  geom_point(aes(x = x, y = y, color = idx), size = 4, alpha = 0.9) +
  scale_color_gradientn(colors = c("#2C396C", "#33437e", "#4256a2","#0a8cb4","#0a8cb4", "#0aaac8","#0aaac8","#50b4cd","#50b4cd","#8ccde1", "#d1ebf3"), guide = "none") +
  theme_void() 
#"#6ebeaf"
#static image
ggsave("waterfall.png", width = 5, height = 10)

#animate
anim <- 
  ggplot(pts) +
  geom_point(aes(x = x, y = y, color = idx, group = idx), size = 4, alpha = 0.9) +
  scale_color_gradientn(colors =c("#2C396C", "#33437e", "#4256a2","#0a8cb4","#0a8cb4", "#0aaac8","#0aaac8","#50b4cd","#50b4cd","#8ccde1", "#d1ebf3"), guide = "none") +
  theme_void()  +
  transition_reveal(along = idx) +
  ease_aes("cubic-in") +
  enter_grow() +
  enter_fade(alpha = 0.9)

animate(anim, nframes = 100, fps = 20)
anim_save("waterfall.gif")
