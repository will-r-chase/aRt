library(dplyr)
library(ggplot2)
library(ambient)

##worley noise with distance2sub
grid <- 
  long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(noise = gen_worley(x, y, value = "distance2sub"))

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = noise)) +
  theme_void() +
  theme(legend.position = "none")

#worley noise with distance2sub and distance_ind 1-5
grid <- 
  long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(noise = gen_worley(x, y, value = "distance2sub", jitter = 0.4, distance_ind = c(1, 5)))

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = noise)) +
  scale_fill_gradientn(colors = c("black", "#47C2C9", "#E384BD", "white")) +
  theme_void() +
  theme(legend.position = "none")

#worley noise with distance2div and ridged fractal
grid <- 
  long_grid(x = seq(1, 10, length.out = 1000), y = seq(1, 10, length.out = 1000)) %>%
  mutate(fractal = fracture(gen_worley, ridged, value = "distance2div", octaves = 8, x = x, y = y))

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = fractal)) +
  theme_void() +
  theme(legend.position = "none")

#simplex noise after two rounds of seed point warping
grid <-
  long_grid(x = seq(0, 10, length.out = 1000), 
            y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_perlin(x = x, y = y, frequency = 1), 
    y1 = y + gen_perlin(x = x, y = y, frequency = 2),
    x2 = x1 + gen_simplex(x = x1, y = y1, frequency = 1),
    y2 = y1 + gen_simplex(x = x1, y = y1, frequency = 3),
    simplex_warp = gen_simplex(x = x1, y = y2)
  )

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = simplex_warp)) +
  scale_fill_gradientn(colors = c('#253852', '#51222f', '#b53435', '#ecbb51', "#eeccc2"), guide = "none") +
  theme_void() +
  theme(legend.position = "none")

#simplex ridged fractal blended with warped spheres, masked by worley distance2mul
grid <-
  long_grid(x = seq(0, 10, length.out = 1000), 
            y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_simplex(x, y) / 2, 
    y1 = y + gen_simplex(x, y) * 2,
    worley = gen_worley(x, y, value = 'distance2mul', jitter = 0.5),
    simplex_frac = fracture(gen_simplex, ridged, octaves = 10, x = x, y = y),
    full = blend(normalise(worley), normalise(simplex_frac), gen_spheres(x1, y1))
  )

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = full)) +
  scale_fill_gradientn(colors = c("black", "#DC1F24", "#EDE8E8","#4BC4CB"), guide = "none") +
  theme_void() +
  theme(legend.position = "none", plot.background = element_blank(), panel.background = element_blank())

#worley noise warped by worley noise with distance2div and ridged fractal
grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(
    x1 = x + fracture(gen_worley, ridged, octaves = 8, x = x, y = y, value = "distance2div", distance = "euclidean",
                      distance_ind = c(1, 2), jitter = 0.4),
    y1 = y + fracture(gen_worley, ridged, octaves = 8, x = x, y = y, value = "distance2div", distance = "euclidean",
                      distance_ind = c(1, 2), jitter = 0.4),
    simplex_warp = gen_worley(x = x1, y = y1, value = "distance")
  )

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = simplex_warp)) +
  theme_void() +
  theme(legend.position = "none")

#seed points warped by distance2div worley ridged fractal, blending worley and worley ridged fractal with cubic noise
grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(
    x1 = x + fracture(gen_worley, ridged, octaves = 8, x = x, y = y, value = "distance2div", distance = "euclidean",
                      distance_ind = c(1, 2), jitter = 0.5),
    y1 = y + fracture(gen_worley, ridged, octaves = 8, x = x, y = y, value = "distance2div", distance = "euclidean",
                      distance_ind = c(1, 3), jitter = 0.4),
    worley_warp = gen_worley(x = x1, y = y1, value = "distance", jitter = 0.4, distance = "manhattan"),
    worley_warp2 = fracture(gen_worley, ridged, octaves = 8, x = x1, y = y1, value = "distance2div", distance = "euclidean",
                            distance_ind = c(1, 2), jitter = 0.5),
    cubic = gen_cubic(x = x * 3, y = y / 3),
    blend = blend(normalize(cubic), worley_warp, worley_warp2)
  )

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = blend)) +
  scale_fill_gradientn(colors = c('#f0efe2', '#363d4a', '#7b8a56', '#ff9369', '#f4c172'), guide = "none") +
  theme_void() +
  theme(legend.position = "none")