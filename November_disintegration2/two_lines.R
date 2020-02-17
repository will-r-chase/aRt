library(tidyverse)
library(EnvStats)
library(zoo)
library(ggforce)

seed1 <- gen_seed_line(data.frame(x = 0, xend = 50, y = 50, yend = 0),
                       n_grains = 10000, split_mod = 3000,
                       gaps = list(c(4500, 5500)))

seed2 <- gen_seed_line(data.frame(x = 15, xend = 65, y = 65, yend = 15),
                       n_grains = 10000,
                       gaps = list(c(4500, 5500)))

line1 <- 
  paint(data.frame(x = 0, xend = 100, y = 100, yend = 0), 10000) %>%
  splitter(seed = seed1, wind_angle = 225) %>%
  gust(angle = 225, force = 6, diff_mod = -0.01, inertia_mod = 0.017, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4) %>%
  mutate(scale = ifelse(inertia <= 2, 0, 0.1 * inertia)) %>%
  unfold_tempest(iterations = 70, type = "curl") %>%
  regonize(min_r = 0.1, max_r = 2, pal = nyc2$colors)

line2 <- 
  paint(data.frame(x = 20, xend = 120, y = 120, yend = 20), 10000) %>%
  splitter(seed = seed2, wind_angle = 45) %>%
  gust(angle = 45, force = 6, diff_mod = -0.01, inertia_mod = 0.017, jitter_min = 2, jitter_max = 20, jitter_mod = 0.4) %>%
  mutate(scale = ifelse(inertia <= 2, 0, 0.1 * inertia)) %>%
  unfold_tempest(iterations = 70, type = "curl") %>%
  regonize(min_r = 0.1, max_r = 2, pal = nyc2$colors)

regons <- rbind(line1$regons, line2$regons)
static <- rbind(line1$static, line2$static)

ggplot() +
  geom_point(data = static, aes(x = x, y = y), alpha = 0.1, size = 0.1, color = "#FEFEFE") +
  geom_regon(data = regons, aes(x0 = x, y0 = y, sides = 6, r = r, fill = color, angle = angle), alpha = 0.7, color = NA) +
  scale_fill_identity() +  
  scale_size_identity() +
  theme_void() +
  theme(panel.background = element_rect(fill = nyc2$bg, color = NA)) +
  coord_equal()

ggsave("two_lines_nyc1.png", width = 10, height = 9.606)
