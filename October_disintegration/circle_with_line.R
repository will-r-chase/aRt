library(tidyverse)
library(EnvStats)
library(zoo)

circle_seed <- gen_seed_cir(n_grains = 100000, wind_angle = 270, r = 50, split_mod = 2500)

circles <- list(a = circle(100000, r = 50),
                b = circle(100000, r = 50.25),
                c = circle(100000, r = 50.5),
                d = circle(100000, r = 50.75),
                e = circle(100000, r = 51)
)

circle1 <- 
  circles %>%
  map( ~ splitter2(., seed = circle_seed, wind_angle = 270)) %>%
  map_dfr( ~ fray(., min_fray = 100, max_fray = 200, num_fray = 5))

circle_dust <- gust(circle1, angle = 270, force = 4, diff_mod = 0.006, inertia_mod = 0.0009, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4)

line_seed <- gen_seed(data.frame(x = 0, xend = 0, y = -60, yend = 60), n_grains = 100000, split_mod = 3000)

lines <- list(a = data.frame(x = -0.5, xend = -0.5, y = -60, yend = 60),
              b = data.frame(x = -0.25, xend = -0.25, y = -60, yend = 60),
              c = data.frame(x = 0, xend = 0, y = -60, yend = 60),
              d = data.frame(x = 0.25, xend = 0.25, y = -60, yend = 60),
              e = data.frame(x = 0.5, xend = 0.5, y = -60, yend = 60))

line_pts <- map(lines, ~ paint(., 100000)) %>%
  map( ~ splitter2(., seed = line_seed, wind_angle = 270)) %>%
  map_dfr( ~ fray(., min_fray = 100, max_fray = 200, num_fray = 5))

line_dust <- gust(line_pts, angle = 270, force = 4, diff_mod = 0.006, inertia_mod = 0.0009, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4)

shapes <- rbind(circle_dust, line_dust)

ggplot(shapes) +
  geom_point(aes(x = x, y = y), alpha = 0.04, size = 0.1, color = "#1e1e1e", shape = 46) +
  scale_color_identity() +  
  theme_void() +
  coord_equal()

# ggplot(square1) +
#   geom_point(aes(x = x, y = y), alpha = 0.03, size = 0.1, color = "#1e1e1e", shape = 46) +
#   scale_color_identity() +  
#   theme_void() +
#   coord_equal()


ggsave("circle_with_line_1.png", height = 10, width = 10)
