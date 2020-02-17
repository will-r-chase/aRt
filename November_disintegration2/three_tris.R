library(tidyverse)
library(EnvStats)
library(zoo)
library(ggforce)

seed_rand <- sample(seq(0, 5000, by = 1), 1)
set.seed(seed_rand)

angle1 <- sample(seq(0, 360, by = 1), 1)
angle2 <- sample(seq(0, 360, by = 1), 1)
angle3 <- sample(seq(0, 360, by = 1), 1)
x0_1 <- sample(-50:50, 1)
x0_2 <- sample(-50:50, 1)
y0_1 <- sample(-100:-30, 1)
y0_2 <- sample(40:100, 1)
x0_3 <- sample(20:50, 1)
y0_3 <- sample(100:100, 1)
r1 <- sample(50:70, 1)
r2 <- sample(50:80, 1)
r3 <- sample(40:80, 1)

seed1 <- gen_seed_regon(n_grains = 5000, wind_angle = angle1, split_mod = 1000)
seed2 <- gen_seed_regon(n_grains = 5000, wind_angle = angle1, split_mod = 1000)
seed3 <- gen_seed_regon(n_grains = 5000, wind_angle = angle1, split_mod = 1000)

tri1 <- 
  regon_pts(points = 5000, edges = 3, start_angle = angle2, r = r1, wind_angle = angle1, cx = x0_1, cy = y0_1) %>%
  splitter(seed = seed1, wind_angle = angle1) %>%
  gust(angle = angle1, force = 6, diff_mod = -0.01, inertia_mod = 0.017, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4) %>%
  mutate(scale = ifelse(inertia <= 2, 0, 0.1 * inertia)) %>%
  unfold_tempest(iterations = 70, type = "curl") %>%
  regonize(min_r = 0.1, max_r = 3, pal = boom$colors)
tri2 <- 
  regon_pts(points = 5000, edges = 3, start_angle = angle3, r = r2, wind_angle = angle1, cx = x0_2, cy = y0_2) %>%
  splitter(seed = seed2, wind_angle = angle1) %>%
  gust(angle = angle1, force = 6, diff_mod = -0.01, inertia_mod = 0.017, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4) %>%
  mutate(scale = ifelse(inertia <= 2, 0, 0.1 * inertia)) %>%
  unfold_tempest(iterations = 70, type = "curl") %>%
  regonize(min_r = 0.1, max_r = 3, pal = boom$colors)
tri3 <- 
  regon_pts(points = 5000, edges = 3, start_angle = angle1, r = r3, wind_angle = angle1, cx = x0_3, cy = y0_3) %>%
  splitter(seed = seed3, wind_angle = angle1) %>%
  gust(angle = angle1, force = 6, diff_mod = -0.01, inertia_mod = 0.017, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4) %>%
  mutate(scale = ifelse(inertia <= 2, 0, 0.1 * inertia)) %>%
  unfold_tempest(iterations = 70, type = "curl") %>%
  regonize(min_r = 0.1, max_r = 3, pal = boom$colors)

static <- rbind(tri1$static, tri2$static, tri3$static)  
triangles <- rbind(tri1$regons, tri2$regons, tri3$regons)

ggplot() +
  geom_point(data = static, aes(x = x, y = y), alpha = 0.1, size = 0.1, color = "#1e1e1e") +
  geom_regon(data = triangles, aes(x0 = x, y0 = y, sides = 3, r = r, fill = color, angle = angle), alpha = 0.7, color = NA) +
  scale_fill_identity() +  
  scale_size_identity() +
  theme_void() +
  theme(panel.background = element_rect(fill = boom$bg, color = NA)) +
  coord_equal()

ggsave("3_tris_581.png", height = 10, width = 10)
