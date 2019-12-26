library(tidyverse)
library(EnvStats)
library(zoo)

regon_seed1 <- gen_seed_regon(n_grains = 100000, edges = 6, start_angle = 100, wind_angle = 0, r = 50, split_mod = 2000)
regons1 <- list(a = regon_pts(100000, start_angle = 100, edges = 6, r = 50),
                b = regon_pts(100000, start_angle = 100, edges = 6, r = 50.2),
                c = regon_pts(100000, start_angle = 100, edges = 6, r = 50.4),
                d = regon_pts(100000, start_angle = 100, edges = 6, r = 50.6),
                e = regon_pts(100000, start_angle = 100, edges = 6, r = 50.8)
)

regons_1 <- 
  regons1 %>%
  map( ~ splitter2(., seed = regon_seed1, wind_angle = 0)) %>%
  map_dfr( ~ fray(., min_fray = 100, max_fray = 200, num_fray = 5))

square1 <- gust(regons_1, angle = 0, force = 7, diff_mod = 0.003, inertia_mod = 0.0004, jitter_min = 2, jitter_max = 20, jitter_mod = 0.5)

regon_seed2 <- gen_seed_regon(n_grains = 100000, edges = 5, start_angle = 40, wind_angle = 0, cx = -25, cy = 65, r = 50, split_mod = 2000)
regons2 <- list(a = regon_pts(100000, start_angle = 40, edges = 5, cx = -25, cy = 65, r = 50),
                b = regon_pts(100000, start_angle = 40, edges = 5, cx = -25, cy = 65, r = 50.2),
                c = regon_pts(100000, start_angle = 40, edges = 5, cx = -25, cy = 65, r = 50.4),
                d = regon_pts(100000, start_angle = 40, edges = 5, cx = -25, cy = 65, r = 50.6),
                e = regon_pts(100000, start_angle = 40, edges = 5, cx = -25, cy = 65, r = 50.8)
)

regons_2 <- 
  regons2 %>%
  map( ~ splitter2(., seed = regon_seed2, wind_angle = 0)) %>%
  map_dfr( ~ fray(., min_fray = 100, max_fray = 200, num_fray = 5))

square2 <- gust(regons_2, angle = 0, force = 7, diff_mod = 0.003, inertia_mod = 0.0004, jitter_min = 2, jitter_max = 20, jitter_mod = 0.5)


regon_seed3 <- gen_seed_regon(n_grains = 100000, edges = 8, start_angle = 40, wind_angle = 0, cx = 10, cy = 30, r = 50, split_mod = 2000)
regons3 <- list(a = regon_pts(100000, start_angle = 40, edges = 8, cx = 15, cy = 30, r = 50),
                b = regon_pts(100000, start_angle = 40, edges = 8, cx = 15, cy = 30, r = 50.2),
                c = regon_pts(100000, start_angle = 40, edges = 8, cx = 15, cy = 30, r = 50.4),
                d = regon_pts(100000, start_angle = 40, edges = 8, cx = 15, cy = 30, r = 50.6),
                e = regon_pts(100000, start_angle = 40, edges = 8, cx = 15, cy = 30, r = 50.8)
)

regons_3 <- 
  regons3 %>%
  map( ~ splitter2(., seed = regon_seed2, wind_angle = 0)) %>%
  map_dfr( ~ fray(., min_fray = 100, max_fray = 200, num_fray = 5))

square3 <- gust(regons_3, angle = 0, force = 7, diff_mod = 0.003, inertia_mod = 0.0004, jitter_min = 2, jitter_max = 20, jitter_mod = 0.5)

squares <- rbind(square1, square2, square3)

ggplot(squares) +
  geom_point(aes(x = x, y = y), alpha = 0.03, size = 0.1, color = "#1e1e1e", shape = 46) +
  scale_color_identity() +  
  theme_void() +
  coord_equal()

# ggplot(square1) +
#   geom_point(aes(x = x, y = y), alpha = 0.03, size = 0.1, color = "#1e1e1e", shape = 46) +
#   scale_color_identity() +  
#   theme_void() +
#   coord_equal()


ggsave("regons_3.png", height = 10, width = 10)
