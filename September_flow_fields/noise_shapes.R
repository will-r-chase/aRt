library(ambient)
library(tidyverse)
library(ggforce)

seed <- 111

grid <- long_grid(seq(1, 10, length.out = 50), seq(1, 10, length.out = 50)) %>%
  mutate(noise = gen_perlin(x, y, seed = seed))

curl <- curl_noise(gen_perlin, seed = seed, x = grid$x, y = grid$y)

grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)

ggplot(grid) +
  geom_regon(aes(x0 = x, y0 = y, r = noise/3, angle = angle, sides = 3, fill = noise), alpha = 0.75) +
  scale_fill_gradientn(colors = c("#FFC1C1", "#C6B6E0", "#C6B6E0", "#326983"), guide = "none") +
  theme_void()

ggsave("perlin_triangles_1.png", height = 10, width = 10)

#trying some more random fields
grid <-
  long_grid(x = seq(0, 10, length.out = 70), 
            y = seq(0, 10, length.out = 70)) %>% 
  mutate(
    x1 = x + gen_perlin(x = x, y = y, frequency = 1), 
    y1 = y + gen_perlin(x = x, y = y, frequency = 2),
    x2 = x1 + gen_simplex(x = x1, y = y1, frequency = 1),
    y2 = y1 + gen_simplex(x = x1, y = y1, frequency = 3),
    simplex_warp = gen_simplex(x = x2, y = y2)
  )

ggplot(grid) +
  geom_regon(aes(x0 = x2, y0 = y2, r = simplex_warp/4, sides = 6, fill = simplex_warp, angle = 1), alpha = 0.8) +
  scale_fill_gradientn(colors = c("#009A72", "#2AA57E", "#6DB089", "#B9BA94", "#FFC4A0", "#FFAA94", "#FFA198", "#FFA9A8", "#FAC4C6"), guide = "none") +
  theme_void()

#a worley field
seed <- 424

grid <-
  long_grid(x = seq(0, 10, length.out = 60), 
            y = seq(0, 10, length.out = 60)) %>% 
  mutate(noise = gen_worley(x, y, seed = seed))

curl <- curl_noise(gen_worley, seed = seed, x = grid$x, y = grid$y)

grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)

ggplot(grid) +
  geom_regon(aes(x0 = x, y0 = y, r = noise/4, sides = 4, fill = noise, angle = angle), alpha = 0.8) +
  scale_fill_gradientn(colors = c("#009A72", "#2AA57E", "#6DB089", "#B9BA94", "#FFC4A0", "#FFAA94", "#FFA198", "#FFA9A8", "#FAC4C6"), guide = "none") +
  theme_void()

ggsave("worley_squares_1.png", width = 10, height = 10)

#another worley field, but with a new angle 
seed <- 381

grid <-
  long_grid(x = seq(0, 10, length.out = 100), 
            y = seq(0, 10, length.out = 100)) %>% 
  mutate(noise = gen_worley(x, y, seed = seed))

curl <- curl_noise(gen_worley, seed = seed, x = grid$x, y = grid$y)

grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)

ggplot(grid) +
  geom_regon(aes(x0 = x, y0 = y, r = 0.08, sides = 4, fill = noise, angle = noise * 2*pi), alpha = 0.8) +
  scale_fill_gradientn(colors = c("#009A72", "#2AA57E", "#6DB089", "#B9BA94", "#FFC4A0", "#FFAA94", "#FFA198", "#FFA9A8", "#FAC4C6"), guide = "none") +
  theme_void()

