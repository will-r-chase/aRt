library(ggplot2)
library(dplyr)
library(purrr)

#cream #FAF4E7
#charcoal #1E1E1E
opt = theme(legend.position  = "none",
            panel.background = element_rect(fill="#fff9f9"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())

###########1
createTrajectory <- function(n, x0, y0, t0, p, q, a1, a2, a3, a4, a5, a6, f1, f2, f3, f4, f5, f6) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] = a1*sin(f1*y[i-1])^p+a2*cos(f2*y[i-1])^q+a3*sin(f3*t[i-1])^p
    y[i] = a4*cos(f4*x[i-1])^q+a5*sin(f5*y[i-1])^p+a6*cos(f6*t[i-1])^q
    t[i] = t[i-1]+v
  }
  
  data.frame(x = x, y = y)
}

a1 <- 0.8
a2 <- 1.65
a3 <- 1.1
a4 <- 0.8
a5 <- 1.2
a6 <- 1.4
f1 <- 0.4
f2 <- 2.11
f3 <- 1.5
f4 <- -1.1
f5 <- 1.3
f6 <- 0.7
p <- 7
q <- 5
v <- 0.15

df=createTrajectory(5000000, 0, 0, 0, p, q, a1, a2, a3, a4, a5, a6, f1, f2, f3, f4, f5, f6)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.03) + opt
ggsave("fuji_1.png", device = "png")


###########2
createTrajectory <- function(n, x0, y0, t0, p, q, a1, a2, a3, a4, a5, a6, f1, f2, f3, f4, f5, f6) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] = a1*sin(f1*y[i-1])^p+a2*cos(f2*y[i-1])^q+a3*sin(f3*t[i-1])^p
    y[i] = a4*cos(f4*x[i-1])^q+a5*sin(f5*y[i-1])^p+a6*cos(f6*t[i-1])^q
    t[i] = t[i-1]+v
  }
  
  data.frame(x = x, y = y)
}

a1 <- 1
a2 <- 1
a3 <- 1
a4 <- 2.3
a5 <- 2.3
a6 <- 2.3
f1 <- 4
f2 <- 4
f3 <- 4
f4 <- 2.1
f5 <- 2.1
f6 <- 2.1
p <- 0
q <- 19
v <- 10

df=createTrajectory(5000000, 0, 0, 0, p, q, a1, a2, a3, a4, a5, a6, f1, f2, f3, f4, f5, f6)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.03) + opt
ggsave("fuji_2.png", device = "png")

###########3
createTrajectory <- function(n, x0, y0, t0, p, q, a1, a2, a3, a4, a5, a6, f1, f2, f3, f4, f5, f6) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] = a1*sin(f1*y[i-1])^p+a2*cos(f2*y[i-1])^q+a3*sin(f3*t[i-1])^p
    y[i] = a4*cos(f4*x[i-1])^q+a5*sin(f5*y[i-1])^p+a6*cos(f6*t[i-1])^q
    t[i] = t[i-1]+v
  }
  
  data.frame(x = x, y = y)
}

a1 <- 1.8
a2 <- -3
a3 <- 2.2
a4 <- 0.454
a5 <- 2.3
a6 <- -0.9
f1 <- 3.76
f2 <- 3.221
f3 <- 0.1
f4 <- 1.598
f5 <- -2.356
f6 <- -1
p <- 9
q <- 14
v <- 3.5

df=createTrajectory(3000000, 0, 0, 0, p, q, a1, a2, a3, a4, a5, a6, f1, f2, f3, f4, f5, f6)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.03) + opt
ggsave("fuji_3.png", device = "png")

###########4
createTrajectory <- function(n, x0, y0, t0, p, q, a1, a2, a3, a4, a5, a6, f1, f2, f3, f4, f5, f6) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  t <- vector(mode = "numeric", length = n)
  
  x[1] <- x0
  y[1] <- y0
  t[1] <- t0
  for(i in 2:n) {
    x[i] = a1*sin(f1*y[i-1])^p+a2*cos(f2*y[i-1])^q+a3*sin(f3*t[i-1])^p
    y[i] = a4*cos(f4*x[i-1])^q+a5*sin(f5*y[i-1])^p+a6*cos(f6*t[i-1])^q
    t[i] = t[i-1]+v
  }
  
  data.frame(x = x, y = y)
}

a1 <- 1.8
a2 <- 1.1
a3 <- 2.2
a4 <- 0.654
a5 <- 2.3
a6 <- 0.9
f1 <- 1.3
f2 <- 2
f3 <- 1.89
f4 <- 1.3
f5 <- 1.5
f6 <- 1.1
p <- 6
q <- 12
v <- 8.4

df=createTrajectory(1000000, 0, 0, 0, p, q, a1, a2, a3, a4, a5, a6, f1, f2, f3, f4, f5, f6)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.03) + opt
ggsave("fuji_4.png", device = "png")
