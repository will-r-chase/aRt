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


###########1
createTrajectory <- function(n, x0, y0, a, b, c) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  for(i in 2:n) {
    x[i] <- y[i-1]-1-sqrt(abs(b*x[i-1]-c))*sign(x[i-1]-1)
    y[i] <- a-x[i-1]-1
  }
  
  data.frame(x = x, y = y)
}

a=2
b=1
c=8
v=3

df=createTrajectory(3000000, 0, 0, a, b, c)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt
ggsave("hopalong_1.png", device = "png")

###########2
createTrajectory <- function(n, x0, y0, t0, a, b, c, v) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] <- y[i-1]-1-sqrt(abs(b*x[i-1]-c))*sign(x[i-1]-1)+t[i-1]
    y[i] <- a-x[i-1]-1-t[i-1]
    t[i] <- t[i-1] + v
  }
  
  data.frame(x = x, y = y)
}

a=9
b=6
c=8
v=0.1

df=createTrajectory(1000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt
ggsave("hopalong_2.png", device = "png")

###########3
createTrajectory <- function(n, x0, y0, t0, a, b, c, v) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] <- y[i-1]-1-sqrt(abs(b*x[i-1]-c))*sign(x[i-1]-1)+t[i-1]
    y[i] <- a-x[i-1]-1-t[i-1]
    t[i] <- t[i-1] + v
  }
  
  data.frame(x = x, y = y)
}

a=5.5
b=1.2
c=8
v=6

df=createTrajectory(1000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt
ggsave("hopalong_3.png", device = "png")

###########4
createTrajectory <- function(n, x0, y0, t0, a, b, c, v) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] <- y[i-1]-1-sqrt(abs(b*x[i-1]-c))*sign(x[i-1]-1)+a*t[i-1]
    y[i] <- a-x[i-1]-1-a*t[i-1]
    t[i] <- t[i-1] + v
  }
  
  data.frame(x = x, y = y)
}

a=10
b=1
c=1
v=0.1

df=createTrajectory(1000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt
ggsave("hopalong_4.png", device = "png")

###########6
createTrajectory <- function(n, x0, y0, t0, a, b, c, v) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] <- y[i-1]-1-sqrt(abs(b*x[i-1]-c))*sign(x[i-1]-1)+a*cos(b*t[i-1])
    y[i] <- a-sin(x[i-1])-1-a*cos(b*t[i-1])
    t[i] <- t[i-1] + v
  }
  
  data.frame(x = x, y = y)
}

a=5.4
b=5
c=3
v=1

df=createTrajectory(1000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt
ggsave("hopalong_6.png", device = "png")
