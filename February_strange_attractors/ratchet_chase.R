library(Rcpp)
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
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double c, double d, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1])+a*sin(c*t[i-1]);
            y[i] = sin(b*x[i-1])+d*cos(b*y[i-1])+d*sin(a*t[i-1]);
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-2.2
b=-1.1
c=0.55
d=-0.7
v=0.2

df=createTrajectory(4000000, 0, 0, 0, a, b, c, d, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.01) + opt
ggsave("ratchet_1.png", device = "png")

###########2 -- First Contact ******
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double c, double d, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1])+a*sin(c*t[i-1]);
            y[i] = sin(b*x[i-1])-d*cos(b*y[i-1])-d*sin(a*t[i-1]);
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-2
b=-1.3
c=0.5
d=-0.45
v=0.15

img <- imager::load.image("star_background.jpg")

g <- grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE)

df=createTrajectory(2500000, 0, 0, 0, a, b, c, d, v)
minX <- min(df$x)
maxX <- max(df$x)
minY <- min(df$y)
maxY <- max(df$y)
ggplot(df, aes(x, y)) + annotation_custom(g, minX-1, maxX+1, minY-1, maxY+1) + 
  geom_point(color="#d7ecee", shape=46, alpha=.055) + opt + ylim(minY-0.7, maxY+0.3) + xlim(minX-0.5, maxX+0.5)
ggsave("first_contact.png", device = "png")

###########3
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double c, double d, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = a*sin(a*y[i-1])+c*cos(a*x[i-1])+d*sin(a*t[i-1]);
            y[i] = a*sin(b*x[i-1])-c*cos(b*y[i-1])-d*sin(b*t[i-1]);
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-1.9
b=-0.6
c=0.6
d=-1
v=0.12

df=createTrajectory(3000000, 0, 0, 0, a, b, c, d, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.01) + opt
ggsave("ratchet_3.png", device = "png")

###########4
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double c, double d, double e, double f, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = a*sin(a*y[i-1])+c*cos(a*x[i-1])-d*sin(e*t[i-1])*sin(e*t[i-1]);
            y[i] = a*sin(b*x[i-1])-c*cos(b*y[i-1])+d*sin(f*t[i-1])*sin(f*t[i-1]);
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=3
b=-3
c=2.3
d=1.2
e=5
f=-2
v=0.5

df=createTrajectory(5000000, 0, 0, 0, a, b, c, d, e, f, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.01) + opt
ggsave("ratchet_4.png", device = "png")


###########5
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double c, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(t[i-1]*y[i-1]/b)*y[i-1]+c*cos(a*t[i-1]-b*x[i-1]);
            y[i] = x[i-1]+sin(t[i-1])/b+a*sin(c*y[i-1]);
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-0.5
b=1
c=-20
v=0.25

df=createTrajectory(1500000, 0, 0, 0, a, b, c, v)

xmax <- max(df$x)/2
xmin <- min(df$x)/2
ymax <- max(df$y)/2
ymin <- min(df$y)/2

df_clip <- df %>%
  filter(x > xmin & x < xmax) %>%
  filter(y > ymin & y < ymax)

ggplot(df_clip, aes(x, y)) + geom_point(color="black", shape=46, alpha=.02) + opt
ggsave("ratchet_5.png", device = "png")


###########6
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(x[i-1]*y[i-1]/b)*y[i-1]+cos(a*x[i-1]-t[i-1]);
            y[i] = t[i-1]+sin(y[i-1])/b;
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=1
b=-0.3
v=0.25

df=createTrajectory(1500000, 0, 0, 0, a, b, v)

xmax <- max(df$x)/2
xmin <- min(df$x)/2
ymax <- max(df$y)/2
ymin <- min(df$y)/2

df_clip <- df %>%
  filter(x > xmin & x < xmax) %>%
  filter(y > ymin & y < ymax)

ggplot(df_clip, aes(x, y)) + geom_point(color="black", shape=46, alpha=.02) + opt
ggsave("ratchet_6.png", device = "png")


###########7
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(x[i-1]*y[i-1]/b)*t[i-1]+cos(a*t[i-1]-y[i-1]);
            y[i] = x[i-1]+sin(t[i-1])/b;
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-1
b=1
v=0.25

df=createTrajectory(1500000, 0, 0, 0, a, b, v)

xmax <- max(df$x)/2
xmin <- min(df$x)/2
ymax <- max(df$y)/2
ymin <- min(df$y)/2

df_clip <- df %>%
  filter(x > xmin & x < xmax) %>%
  filter(y > ymin & y < ymax)

ggplot(df_clip, aes(x, y)) + geom_point(color="black", shape=46, alpha=.02) + opt
ggsave("ratchet_7.png", device = "png")


###########8---unspooled
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(x[i-1]*y[i-1]/b)*y[i-1]+cos(a*t[i-1]-y[i-1]);
            y[i] = y[i-1]+sin(t[i-1])/b;
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=3 #whole numbers make a single spool, halves make 2 spools
b=-1.5
v=0.11

df=createTrajectory(3000000, 0, 0, 0, a, b, v)

xmax <- max(df$x)/2
xmin <- min(df$x)/2
ymax <- max(df$y)/2
ymin <- min(df$y)/2

df_clip <- df %>%
  filter(x > xmin & x < xmax) %>%
  filter(y > ymin & y < ymax)

ggplot(df_clip, aes(x, y)) + geom_point(color="#3a1d00", shape=46, alpha=.025) + opt
ggsave("unspooled_2.png", device = "png")

###########9--triangles
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(x[i-1]*t[i-1]/b)*y[i-1]+cos(a*t[i-1]-y[i-1]);
            y[i] = y[i-1]+sin(x[i-1])/b;
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=1.5 #whole numbers make a single spool, halves make 2 spools
b=0.3
v=0.15

df=createTrajectory(1500000, 0, 0, 0, a, b, v)

xmax <- max(df$x)/1.5
xmin <- min(df$x)/1.5
ymax <- max(df$y)/1.5
ymin <- min(df$y)/1.5

df_clip <- df %>%
  filter(x > xmin & x < xmax) %>%
  filter(y > ymin & y < ymax)

ggplot(df_clip, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.025) + opt
ggsave("ratchet_9.png", device = "png")


###########10--3d box
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double c, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = a*sin(a*y[i-1])+c*cos(a*t[i-1]);
            y[i] = a*sin(b*x[i-1])-c*cos(b*t[i-1]);
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=5
b=-10
c=10
v=0.1

df=createTrajectory(1000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt
ggsave("ratchet_10_box2.png", device = "png")


###########box_3
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double c, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = a*sin(a*y[i-1])+c*cos(a*t[i-1]);
            y[i] = a*sin(b*x[i-1])-c*cos(b*t[i-1]);
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=1
b=-10
c=10
v=0.1

df=createTrajectory(2000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="black", shape=46, alpha=.05) + opt
ggsave("box_3.png", device = "png")

###########box_W
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double c, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = a*sin(a*y[i-1])+c*cos(a*t[i-1]);
            y[i] = a*sin(b*x[i-1])-c*cos(b*t[i-1]);
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=2.5
b=-10
c=10
v=0.1

df=createTrajectory(2000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="black", shape=46, alpha=.05) + opt + scale_y_reverse()
ggsave("box_W.png", device = "png")

###########box_C
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double c, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = a*sin(a*y[i-1])+c*cos(a*t[i-1]);
            y[i] = a*sin(b*x[i-1])-c*cos(b*t[i-1]);
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=5
b=-10
c=10
v=0.1

df=createTrajectory(2000000, 0, 0, 0, a, b, c, v)
df$x <- df$x*1.2
ggplot(df, aes(x, y)) + geom_point(color="black", shape=46, alpha=.05) + opt
ggsave("box_C.png", device = "png")

###########box_for_color
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double c, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = a*sin(a*y[i-1])+c*cos(a*t[i-1]);
            y[i] = a*sin(b*x[i-1])-c*cos(b*t[i-1]);
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=10
b=-10
c=9.1
v=5

df=createTrajectory(2000000, 0, 0, 0, a, b, c, v)
ggplot(df, aes(x, y)) + geom_point(color="black", shape=46, alpha=.05) + opt
ggsave("black_box.png", device = "png")

###########9--triangles-black
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, double t0, 
            double a, double b, double v) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            NumericVector t(n);
            x[0]=x0;
            y[0]=y0;
            t[0]=t0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(x[i-1]*t[i-1]/b)*y[i-1]+cos(a*t[i-1]-y[i-1]);
            y[i] = y[i-1]+sin(x[i-1])/b;
            t[i] = t[i-1]+v;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=1.5 #whole numbers make a single spool, halves make 2 spools
b=0.3
v=0.15

df=createTrajectory(1500000, 0, 0, 0, a, b, v)

xmax <- max(df$x)/1.5
xmin <- min(df$x)/1.5
ymax <- max(df$y)/1.5
ymin <- min(df$y)/1.5

df_clip <- df %>%
  filter(x > xmin & x < xmax) %>%
  filter(y > ymin & y < ymax)

ggplot(df_clip, aes(x, y)) + geom_point(color="black", shape=46, alpha=.025) + opt
ggsave("black_triangles.png", device = "png")

