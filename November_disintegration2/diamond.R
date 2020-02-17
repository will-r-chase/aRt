library(tidyverse)
library(EnvStats)
library(zoo)
library(ggforce)

my_pal <- dots4

seed1 <- gen_seed_regon(n_grains = 5000, r = 50, edges = 4, start_angle = 90, wind_angle = 45, gaps = list(c(3751, 5000)))
seed2 <- gen_seed_regon(n_grains = 5000, r = 50, edges = 4, start_angle = 90, wind_angle = 135, gaps = list(c(1, 1250)))
seed3 <- gen_seed_regon(n_grains = 5000, r = 50, edges = 4, start_angle = 90, wind_angle = 225, gaps = list(c(1251, 2500)))
seed4 <- gen_seed_regon(n_grains = 5000, r = 50, edges = 4, start_angle = 90, wind_angle = 315, gaps = list(c(2501, 3750)))

diamond1 <- 
  regon_pts(points = 5000, edges = 4, start_angle = 90, r = 50, wind_angle = 45) %>%
  splitter(seed = seed1, wind_angle = 45) %>%
  gust(angle = 45, force = 6, diff_mod = -0.01, inertia_mod = 0.017, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4) %>%
  mutate(scale = ifelse(inertia <= 2, 0, 0.1 * inertia)) %>%
  unfold_tempest(iterations = 100, type = "curl") %>%
  regonize(min_r = 0.1, max_r = 3, pal = my_pal$colors)
diamond2 <- 
  regon_pts(points = 5000, edges = 4, start_angle = 90, r = 50, wind_angle = 135) %>%
  splitter(seed = seed2, wind_angle = 135) %>%
  gust(angle = 135, force = 6, diff_mod = -0.01, inertia_mod = 0.017, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4) %>%
  mutate(scale = ifelse(inertia <= 2, 0, 0.1 * inertia)) %>%
  unfold_tempest(iterations = 100, type = "curl") %>%
  regonize(min_r = 0.1, max_r = 3, pal = my_pal$colors)
diamond3 <- 
  regon_pts(points = 5000, edges = 4, start_angle = 90, r = 50, wind_angle = 225) %>%
  splitter(seed = seed3, wind_angle = 225) %>%
  gust(angle = 225, force = 6, diff_mod = -0.01, inertia_mod = 0.017, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4) %>%
  mutate(scale = ifelse(inertia <= 2, 0, 0.1 * inertia)) %>%
  unfold_tempest(iterations = 100, type = "curl") %>%
  regonize(min_r = 0.1, max_r = 3, pal = my_pal$colors)
diamond4 <- 
  regon_pts(points = 5000, edges = 4, start_angle = 90, r = 50, wind_angle = 315) %>%
  splitter(seed = seed4, wind_angle = 315) %>%
  gust(angle = 315, force = 6, diff_mod = -0.01, inertia_mod = 0.017, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4) %>%
  mutate(scale = ifelse(inertia <= 2, 0, 0.1 * inertia)) %>%
  unfold_tempest(iterations = 100, type = "curl") %>%
  regonize(min_r = 0.1, max_r = 3, pal = my_pal$colors)

diamonds <- rbind(diamond1$regons, diamond2$regons, diamond3$regons, diamond4$regons)

plot0 <- 
ggplot() +
  #geom_point(data = diamond$static, aes(x = x, y = y), alpha = 0.1, size = 0.1, color = "#1e1e1e") +
  geom_regon(data = diamonds, aes(x0 = x, y0 = y, sides = 4, r = r, fill = color, angle = angle), alpha = 0.7, color = NA) +
  scale_fill_identity() +  
  scale_size_identity() +
  theme_void() +
  theme(panel.background = element_rect(fill = my_pal$bg, color = NA)) +
  coord_equal() 

ggsave("diamond_dots3.png", height = 10, width = 10)


library(patchwork)
wrap_plots(plot1, plot2, plot3,
           plot4, plot5, plot6, plot7,
           plot8, plot9, plot10, plot11, 
           plot12, plot0)

ggsave("plots_wrapped.png", width = 12, height = 10)
