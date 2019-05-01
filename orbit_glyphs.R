library(tidyverse)
#devtools::install_github("romainfrancois/rap")
library(rap)

r0 <- 0.05
r0_0 <- 0.06
r1 <- 1
r2 <- 1.1
r3 <- 1.125
r4 <- 0.2
r5 <- 0.22
r6 <- 0.235

diamond <- tibble(angle = seq(0, 2*pi, length.out = 5), x = r1*cos(angle), y = r1*sin(angle), size = 0.8, id = 4, line = "dotted") %>%
  mutate(rownum = 1:nrow(.)) %>%
  rap(cir1 = ~tibble(angle = seq(0, 2*pi, length.out = 50), x = x+r0*cos(angle), y = y+r0*sin(angle), size = 0.4, id = rownum, line = "solid"),
      cir2 = ~tibble(angle = seq(0, 2*pi, length.out = 50), x = x+r0_0*cos(angle), y = y+r0_0*sin(angle), size = 0.3, id = rownum, line = "solid"))

circle1 <- tibble(angle = seq(0, 2*pi, length.out = 200), x = r1*cos(angle), y = r1*sin(angle), size = 1.5, id = 1, line = "solid")
circle2 <- tibble(angle = seq(0, 2*pi, length.out = 200), x = r2*cos(angle), y = r2*sin(angle), size = 0.2, id = 2, line = "solid")
circle3 <- tibble(angle = seq(0, 2*pi, length.out = 200), x = r3*cos(angle), y = r3*sin(angle), size = 0.2, id = 3, line = "solid")
circle4 <- tibble(angle = seq(0, 2*pi, length.out = 200), x = r4*cos(angle), y = r4*sin(angle), size = 0.2, id = 4, line = "solid")
circle5 <- tibble(angle = seq(0, 2*pi, length.out = 200), x = r5*cos(angle), y = r5*sin(angle), size = 0.5, id = 5, line = "solid")
circle6 <- tibble(angle = seq(0, 2*pi, length.out = 200), x = r6*cos(angle), y = r6*sin(angle), size = 0.5, id = 6, line = "dotted")

#radius of circle inscribed in diamond
#diameter is length of side of diamond
#chord length (diamond side) is 2*r*sin(angle/2)

r_inner <- (2*r1*sin((pi/2)/2)/2)

square <- tibble(angle = seq(pi/4, pi/4+2*pi, length.out = 5), x = r_inner*cos(angle), y = r_inner*sin(angle), size = 0.8, id = 9, line = "dotted")

shapes <- rbind(circle1, circle2, circle3, circle5, circle6, diamond %>% select(-cir1, -cir2, -rownum), square)

ggplot() +
  geom_point(data = diamond, aes(x = x, y = y), size = 4.5, color = "white") +
  geom_point(data = square, aes(x = x, y = y), size = 3, color = "white") +
  geom_path(data = shapes, aes(x = x, y = y, group = id, size = size), linetype = shapes$line, color = "white") +
  geom_path(data = small_cir1, aes(x = x, y = y, group = id, size = size), linetype = small_cir$line, color = "white") +
  geom_path(data = small_cir2, aes(x = x, y = y, group = id, size = size), linetype = small_cir2$line, color = "white") +
  geom_polygon(data = circle4, aes(x = x, y = y), color = "white", fill = "white") +
  scale_size_identity() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#141414"))
  NULL

ggsave("orbit_glyph_test.png", device = "png")  
  

small_cir1 <- bind_rows(diamond$cir1)
small_cir2 <- bind_rows(diamond$cir2)

ggplot(diamond, aes(x = x, y = y)) +
  geom_point(size = 4.5) +
  geom_path(aes(group = id, size = size), linetype = "dotted") +
  geom_path(data = small_cir1, aes(x = x, y = y, group = id, size = size), linetype = small_cir$line) +
  geom_path(data = small_cir2, aes(x = x, y = y, group = id, size = size), linetype = small_cir2$line) +
  scale_size_identity()
