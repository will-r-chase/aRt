library(tidyverse)
library(EnvStats)
library(zoo)

regon_seed1 <- gen_seed_regon(n_grains = 100000, edges = 3, start_angle = 70, wind_angle = 225, r = 50, split_mod = 2000)
regons1 <- list(a = regon_pts(100000, start_angle = 70, edges = 3, r = 50),
                b = regon_pts(100000, start_angle = 70, edges = 3, r = 50.2),
                c = regon_pts(100000, start_angle = 70, edges = 3, r = 50.4),
                d = regon_pts(100000, start_angle = 70, edges = 3, r = 50.6),
                e = regon_pts(100000, start_angle = 70, edges = 3, r = 50.8)
)

regons_1 <- 
  regons1 %>%
  map( ~ splitter2(., seed = regon_seed1, wind_angle = 225)) %>%
  map_dfr( ~ fray(., min_fray = 100, max_fray = 200, num_fray = 5))

tri1 <- gust(regons_1, angle = 225, force = 6, diff_mod = -0.025, inertia_mod = 0.003, jitter_min = 2, jitter_max = 30, jitter_mod = 0.6)

regon_seed2 <- gen_seed_regon(n_grains = 100000, edges = 4, start_angle = 40, wind_angle = 0, cx = -10, cy = 35, r = 50, split_mod = 2000)
regons2 <- list(a = regon_pts(100000, start_angle = 40, edges = 4, cx = -10, cy = 35, r = 50),
                b = regon_pts(100000, start_angle = 40, edges = 4, cx = -10, cy = 35, r = 50.2),
                c = regon_pts(100000, start_angle = 40, edges = 4, cx = -10, cy = 35, r = 50.4),
                d = regon_pts(100000, start_angle = 40, edges = 4, cx = -10, cy = 35, r = 50.6),
                e = regon_pts(100000, start_angle = 40, edges = 4, cx = -10, cy = 35, r = 50.8)
)

regons_2 <- 
  regons2 %>%
  map( ~ splitter2(., seed = regon_seed2, wind_angle = 0)) %>%
  map_dfr( ~ fray(., min_fray = 100, max_fray = 200, num_fray = 5))

square2 <- gust(regons_2, angle = 0, force = 4, diff_mod = 0.008, inertia_mod = 0.001, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4)

squares <- rbind(square1, square2)

ggplot(tri1) +
  geom_point(aes(x = x, y = y), alpha = 0.03, size = 0.1, color = "#1e1e1e", shape = 46) +
  scale_color_identity() +  
  xlim(c(-150, 150)) +
  ylim(c(-150, 100))+
  theme_void() +
  coord_equal()

# ggplot(square1) +
#   geom_point(aes(x = x, y = y), alpha = 0.03, size = 0.1, color = "#1e1e1e", shape = 46) +
#   scale_color_identity() +  
#   theme_void() +
#   coord_equal()


ggsave("tri_1.png", height = 10, width = 10)
