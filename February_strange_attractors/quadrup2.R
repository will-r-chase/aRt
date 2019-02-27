library(ggplot2)
library(dplyr)
library(purrr)

#cream #FAF4E7
#charcoal #1E1E1E
opt = theme(legend.position  = "none",
            panel.background = element_rect(fill="white"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())


###########1--diamond
createTrajectory <- function(n, x0, y0, t0, a, b, c, v) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] <- y[i-1] - sign(x[i-1])*sin(log(abs(b*t[i-1]-c)))*atan(abs(c*t[i-1]-b)^2)
    y[i] <- a - x[i-1] + cos(b*t[i-1])^2 - sin(c*y[i-1])
    t[i] <- t[i-1] + v
  }
  
  data.frame(x = x, y = y)
}

a=-25
b=1
c=23
v=3

df=createTrajectory(1000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt
ggsave("quadrup_1.png", device = "png")

###########2
createTrajectory <- function(n, x0, y0, t0, a, b, c, v) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] <- y[i-1] - sign(x[i-1])*sin(log(abs(b*t[i-1]-c)))*atan(abs(c*t[i-1]-b)^2)
    y[i] <- a - x[i-1]
    t[i] <- t[i-1] + v
  }
  
  data.frame(x = x, y = y)
}

a=5
b=5
c=5
v=5

df=createTrajectory(1000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt
ggsave("quadrup_2.png", device = "png")

###########3
createTrajectory <- function(n, x0, y0, t0, a, b, c, v) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] <- y[i-1] - sign(x[i-1])*sin(log(abs(b*t[i-1]-c)))*atan(abs(c*t[i-1]-b)^2) + cos(c*t[i-1])*x[i-1]
    y[i] <- a - x[i-1]
    t[i] <- t[i-1] + v
  }
  
  data.frame(x = x, y = y)
}

a=0.1
b=21
c=0.55
v=10

df=createTrajectory(1000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt
ggsave("quadrup_3.png", device = "png")

###########4
createTrajectory <- function(n, x0, y0, t0, a, b, c, v) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] <- y[i-1] - sign(x[i-1])*sin(log(abs(b*t[i-1]-c)))*atan(abs(c*t[i-1]-b)^2) + cos(c*t[i-1])*x[i-1]
    y[i] <- a - x[i-1]
    t[i] <- t[i-1] + v
  }
  
  data.frame(x = x, y = y)
}

a=0.1
b=20
c=0.5
v=10

df=createTrajectory(1000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt
ggsave("quadrup_4.png", device = "png")




###########1--diamond black
createTrajectory <- function(n, x0, y0, t0, a, b, c, v) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] <- y[i-1] - sign(x[i-1])*sin(log(abs(b*t[i-1]-c)))*atan(abs(c*t[i-1]-b)^2)
    y[i] <- a - x[i-1] + cos(b*t[i-1])^2 - sin(c*y[i-1])
    t[i] <- t[i-1] + v
  }
  
  data.frame(x = x, y = y)
}

a=-25
b=1
c=23
v=3

df=createTrajectory(1000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="black", shape=46, alpha=.05) + opt
ggsave("diamond_black.png", device = "png")