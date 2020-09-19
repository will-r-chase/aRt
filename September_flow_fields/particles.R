library(tidyverse)
library(ambient)
library(particles)
library(tidygraph)


##particles##

#setup a grid and calculate a noise value for it
grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(noise = gen_simplex(x, y))

#try making it a matrix and calculating the angle based on each noise value
field <- as.matrix(grid, x, value = normalize(noise, to = c(-1, 1))) * (2 * pi)

#particle simulation, taken from particles vignette
sim <- create_ring(1000) %>% 
  simulate(alpha_decay = 0, setup = aquarium_genesis()) %>% 
  wield(reset_force, xvel = 0, yvel = 0) %>% 
  wield(field_force, angle = field, vel = 0.1, xlim = c(-5, 5), ylim = c(-5, 5)) %>% 
  evolve(100, record)

traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
names(traces) <- c('x', 'y')
traces$particle <- rep(1:1000, 100)

ggplot(traces) +
  geom_path(aes(x, y, group = particle), size = 0.1) + 
  theme_void() + 
  theme(legend.position = 'none')

ggsave("particles_test2.png", height = 10, width = 10)

#testing same as before but with curl noise
grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(noise = gen_simplex(x, y))

curl <- curl_noise(gen_perlin, x = grid$x, y = grid$y)

#calculating the angle between the original point and the curl point
grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)

#use curl angle in matrix
field <- as.matrix(grid, x, value = angle)

sim <- create_empty(1000) %>% 
  simulate(alpha_decay = 0, setup = aquarium_genesis(vel_max = 0)) %>% 
  wield(reset_force, xvel = 0, yvel = 0) %>% 
  wield(field_force, angle = field, vel = 0.1, xlim = c(-5, 5), ylim = c(-5, 5)) %>% 
  evolve(100, record)

traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
names(traces) <- c('x', 'y')
traces$particle <- rep(1:1000, 100)

ggplot(traces) +
  geom_path(aes(x, y, group = particle), size = 0.3, alpha = 0.5) + 
  theme_void() + 
  theme(legend.position = 'none')

ggsave("curl2.png", width = 15, height = 15)


### a whole mess of experiments

##some color palettes
tsu <- c('#687f72', '#cc7d6c', '#dec36f', '#dec7af', '#ad8470', '#424637')
tsu_bg <- '#cfc7b9'

cc273 <- c('#363d4a', '#7b8a56', '#ff9369', '#f4c172')
cc273_bg <- '#f0efe2'

kov_02 <- c('#e8dccc', '#e94641', '#eeaeae')
kov_02_bg <- '#6c96be'

knotberry1_bg <- "#e5ded8"
custom_pal <- c("#686D2C", "#F4CB4C", "#99673A")

bl_yl <- c('#4CA66B', '#00b2dd')
bl_yl_bg <- '#EEEEEE'

rolfs_1r <- c('#004996', '#567bae', '#ff4c48', '#ffbcb3')
rolfs_1r_bg <- "#fff8e7"

dt01 <- c('#172a89', '#f7f7f3')
dt01_bg <- '#f3abb0'

jud_pg <- c('#f04924', '#fcce09', '#408ac9')
jud_pg_bg <- '#ffffff'

jud <- c('#f04924', '#fcce09', '#408ac9')
jud_bg <- '#ffffff'

#wandering
grid <-
  long_grid(x = seq(0, 10, length.out = 1000), 
            y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_perlin(x = x, y = y, frequency = 2), 
    y1 = y + gen_perlin(x = x, y = y, frequency = 0.5)
  )

curl <- curl_noise(gen_simplex, x = grid$x1, y = grid$y1)

grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)

field <- as.matrix(grid, x, value = angle)

sim <- create_ring(10000) %>% 
  simulate(alpha_decay = 0, setup = petridish_genesis(vel_max = 0, max_radius = 1)) %>% 
  wield(reset_force, xvel = 0, yvel = 0) %>% 
  wield(field_force, angle = field, vel = 0.15, xlim = c(-20, 25), ylim = c(-20, 25)) %>% 
  evolve(100, record)

traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
names(traces) <- c('x', 'y')
traces$particle <- rep(1:10000, 100)

traces2 <- 
  traces %>%
  group_by(particle) %>%
  mutate(color = sample(rolfs_1r, 1, replace = TRUE))

ggplot(traces2) +
  geom_path(aes(x, y, group = particle, color = color), size = 0.03, alpha = 0.5) + 
  scale_color_identity(guide = "none") +
  theme_void() + 
  theme(legend.position = 'none', panel.background = element_rect(fill = rolfs_1r_bg, color = "NA"),
        panel.border = element_blank())

ggsave("curl1_seed345_purple.png", width = 4.1, height = 5.8, dpi = 300)

#something new
seed <- sample(1:2000, 1)

grid <-
  long_grid(x = seq(0, 10, length.out = 1000), 
            y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_perlin(x = x, y = y, frequency = 1, seed = seed), 
    y1 = y + gen_perlin(x = x, y = y, frequency = -0.5, seed = seed)
  )

curl <- curl_noise(gen_perlin, seed = seed, x = grid$x1, y = grid$y1)

grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)

field <- as.matrix(grid, x, value = angle)

sim <- create_ring(10000) %>% 
  simulate(alpha_decay = 0, setup = petridish_genesis(vel_max = 0, max_radius = 1)) %>% 
  wield(reset_force, xvel = 0, yvel = 0) %>% 
  wield(field_force, angle = field, vel = 0.12, xlim = c(-45, 40), ylim = c(-45, 40)) %>% 
  evolve(100, record)

traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
names(traces) <- c('x', 'y')
traces$particle <- rep(1:10000, 100)

traces2 <- 
  traces %>%
  group_by(particle) %>%
  mutate(color = sample(rolfs_1r, 1, replace = TRUE))

ggplot(traces2) +
  geom_path(aes(x, y, group = particle, color = color), size = 0.035, alpha = 0.6) + 
  scale_color_identity(guide = "none") +
  theme_void() + 
  theme(legend.position = 'none', panel.background = element_rect(fill = rolfs_1r_bg))

ggsave("curl2_seed1440_purple_4x6.png", width = 4, height = 6)


#another one
seed <- sample(1:2000, 1)

grid <-
  long_grid(x = seq(0, 10, length.out = 1000), 
            y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_perlin(x = x, y = y, frequency = 3, seed = seed), 
    y1 = y + gen_perlin(x = x, y = y, frequency = 1.5, seed = seed)
  )

curl <- curl_noise(gen_perlin, seed = seed, x = grid$x1, y = grid$y1)

grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)

field <- as.matrix(grid, x, value = angle)

sim <- create_empty(10000) %>% 
  simulate(alpha_decay = 0, setup = aquarium_genesis(vel_max = 0)) %>% 
  wield(reset_force, xvel = 0, yvel = 0) %>% 
  wield(field_force, angle = field, vel = 0.15, xlim = c(-40, 40), ylim = c(-40, 40)) %>% 
  evolve(100, record)

traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
names(traces) <- c('x', 'y')
traces$particle <- rep(1:10000, 100)

traces2 <- 
  traces %>%
  group_by(particle) %>%
  mutate(color = sample(dt01, 1, replace = TRUE))

ggplot(traces2) +
  geom_path(aes(x, y, group = particle, color = color), size = 0.03, alpha = 0.9) + 
  scale_color_identity(guide = "none") +
  theme_void() + 
  theme(legend.position = 'none', panel.background = element_rect(fill = dt01_bg))

ggsave("curl7_seed1455.png", width = 15, height = 15)

#one more
seed <- sample(1:2000, 1)

grid <-
  long_grid(x = seq(0, 10, length.out = 1000), 
            y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_perlin(x = x, y = y, frequency = 3, seed = seed), 
    y1 = y + gen_perlin(x = x, y = y, frequency = 4, seed = seed)
  )

curl <- curl_noise(gen_perlin, seed = seed, x = grid$x1, y = grid$y1)

grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)

field <- as.matrix(grid, x, value = angle)

sim <- create_empty(10000) %>% 
  simulate(alpha_decay = 0, setup = aquarium_genesis(vel_max = 0)) %>% 
  wield(reset_force, xvel = 0, yvel = 0) %>% 
  wield(field_force, angle = field, vel = 0.15, xlim = c(-30, 30), ylim = c(-40, 40)) %>% 
  evolve(100, record)

traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
names(traces) <- c('x', 'y')
traces$particle <- rep(1:10000, 100)

traces2 <- 
  traces %>%
  group_by(particle) %>%
  mutate(color = sample(rolfs_1r, 1, replace = TRUE))

ggplot(traces2) +
  geom_path(aes(x, y, group = particle, color = color), size = 0.03, alpha = 0.5) + 
  scale_color_identity(guide = "none") +
  theme_void() + 
  theme(legend.position = 'none', panel.background = element_rect(fill = rolfs_1r_bg))

ggsave("curl8_seed15.png", width = 15, height = 15)


