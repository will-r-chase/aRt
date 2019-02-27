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

t=x
xnew=b*y+w
w=a*x+(1-a)*2*x*x/(1+x*x)
ynew=w-t

w=a*x

###########1--diamond
createTrajectory <- function(n, x0, y0, a, b) {
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  
  w <- function(z, A){
    val <- A*z+2*(1-A)*(z^2)*(1+z^2)^(-2)
    return(val)
  }
  
  x[1] <- x0
  y[1] <- y0
  
  for(i in 2:n) {
    x[i] <- b*y[i-1] + w(x[i-1], a)
    y[i] <- w(x[i], a) - x[i-1]
  }
  
  data.frame(x = x, y = y)
}

a=0.96737636289763
b=-0.9223452

df=createTrajectory(500000, 0.5, 0.5, a, b)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt
ggsave("quadrup_1.png", device = "png")


w <- function(z, A){
  val <- A*z+2*(1-A)*(z^2)*(1+z^2)^(-2)
  return(val)
}
w(1, 0.8)
