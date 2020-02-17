library(tidyverse)
library(EnvStats)
library(zoo)
library(ggforce)

seed_rand <- sample(seq(0, 5000, by = 1), 1)
set.seed(seed_rand)

angle1 <- sample(seq(0, 360, by = 1), 1)
angle2 <- sample(seq(0, 360, by = 1), 1)
x0_1 <- sample(seq(-50, 50, by = 1), 1)
x0_2 <- sample(seq(-50, 50, by = 1), 1)
y0_1 <- sample(seq(-50, 50, by = 1), 1)
y0_2 <- sample(seq(-50, 50, by = 1), 1)

seed1 <- gen_seed_cir(n_grains = 10000, r = 50, wind_angle = angle1, split_mod = 500)
seed2 <- gen_seed_cir(n_grains = 10000, r = 50, wind_angle = angle2, split_mod = 500)

circle1 <- circle(points = 10000, r = 50, x0 = x0_1, y0 = y0_1) %>%
  splitter(seed = seed1, wind_angle = angle1) %>%
  gust(angle = angle1, force = 6, diff_mod = -0.01, inertia_mod = 0.017, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4) %>%
  mutate(scale = ifelse(inertia <= 2, 0, 0.1 * inertia)) %>%
  unfold_tempest(iterations = 70, type = "curl") %>%
  bubbleize(min_r = 0.5, max_r = 3, base_color = "#FEFEFE", pal = no_expectations$colors)

circle2 <- circle(points = 10000, r = 50, x0 = x0_2, y0 = y0_2) %>%
  splitter(seed = seed2, wind_angle = angle2) %>%
  gust(angle = angle2, force = 6, diff_mod = -0.01, inertia_mod = 0.017, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4) %>%
  mutate(scale = ifelse(inertia <= 2, 0, 0.1 * inertia)) %>%
  unfold_tempest(iterations = 70, type = "curl") %>%
  bubbleize(min_r = 0.5, max_r = 3, base_color = "#FEFEFE", pal = no_expectations$colors)

circles <- rbind(circle1, circle2)

ggplot() +
  geom_point(data = circles, aes(x = x, y = y, size = size, color = color), alpha = 0.7) +
  #geom_regon(data = regons, aes(x0 = x, y0 = y, sides = 6, r = r, fill = color, angle = angle), alpha = 0.7, color = NA) +
  scale_color_identity() +  
  scale_size_identity() +
  theme_void() +
  theme(panel.background = element_rect(fill = no_expectations$bg, color = NA)) +
  coord_equal()

ggsave("two_circles_1956.png", width = 10, height = 10)
