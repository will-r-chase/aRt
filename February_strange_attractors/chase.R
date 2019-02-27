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

##########1
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])*sin(a*y[i-1])+c*cos(a*x[i-1])*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])*sin(b*x[i-1])+d*cos(b*y[i-1])*cos(a*x[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-2
b=1
c=0.5
d=-0.9

df=createTrajectory(5000000, 0, 0, a, b, c, d)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.01) + opt
ggsave("chase_1.png", device = "png")

###########2
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])*sin(a*y[i-1])+c*cos(a*x[i-1])*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])*sin(b*x[i-1])-d*cos(b*y[i-1])*cos(a*x[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-3 #making a smaller makes it more "spread out"? but don't make it smaller than 3. making it bigger is bad, just makes lines
b=-3 #making b smaller seems good... not sure, making it from -3 to 3 doesn't change, but 1 to -1 does...?
c=-0.5
d=-0.5

df=createTrajectory(10000000, 0, 0, a, b, c, d)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.1) + opt
ggsave("chase_2.png", device = "png")

###########3
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])*sin(a*y[i-1])+c*cos(a*x[i-1])*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])*sin(b*x[i-1])+d*cos(b*y[i-1])*cos(a*x[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-2.5 #making a smaller makes it more "spread out"? but don't make it smaller than 3. making it bigger is bad, just makes lines
b=-2 #making b smaller seems good... not sure, making it from -3 to 3 doesn't change, but 1 to -1 does...?
c=-0.5
d=0.6

df=createTrajectory(5000000, 0, 0, a, b, c, d)
ggplot(df, aes(x, y)) + geom_point(color="black", shape=46, alpha=.01) + opt
ggsave("chase_3.png", device = "png")

###########4
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])*sin(a*y[i-1])+c*cos(a*x[i-1])*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])*sin(b*x[i-1])+d*cos(b*y[i-1])*cos(a*x[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-2.5 #making a smaller makes it more "spread out"? but don't make it smaller than 3. making it bigger is bad, just makes lines
b=-2 #making b smaller seems good... not sure, making it from -3 to 3 doesn't change, but 1 to -1 does...?
c=-0.8
d=1

df=createTrajectory(5000000, 0, 0, a, b, c, d)
ggplot(df, aes(x, y)) + geom_point(color="black", shape=46, alpha=.01) + opt
ggsave("chase_4.png", device = "png")

###########5
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])*sin(a*y[i-1])-c*cos(a*x[i-1])*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])*sin(b*x[i-1])+d*cos(b*y[i-1])*cos(a*x[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-2.5 #making a smaller makes it more "spread out"? but don't make it smaller than 3. making it bigger is bad, just makes lines
b=-2 #making b smaller seems good... not sure, making it from -3 to 3 doesn't change, but 1 to -1 does...?
c=-0.75
d=1

df=createTrajectory(6000000, 0, 0, a, b, c, d)
ggplot(df, aes(x, y)) + geom_point(color="#E69CA8", shape=46, alpha=.01) + opt
ggsave("chase_5.png", device = "png")

###########6
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1])+d*sin(b*x[i-1]);
            y[i] = sin(b*x[i-1])-d*cos(b*y[i-1])-a*cos(a*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-2.5
b=-2
c=-0.5
d=1.5

df=createTrajectory(5000000, 0, 0, a, b, c, d)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.01) + opt
ggsave("chase_6.png", device = "png")

###########7
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1])+d*sin(b*x[i-1]);
            y[i] = sin(b*x[i-1])-d*cos(b*y[i-1])-a*cos(a*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-2
b=0.2
c=-0.5
d=-0.9

df=createTrajectory(7000000, 0, 0, a, b, c, d)
ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.01) + opt
ggsave("chase_7.png", device = "png")


##########black_chase_1
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])*sin(a*y[i-1])+c*cos(a*x[i-1])*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])*sin(b*x[i-1])+d*cos(b*y[i-1])*cos(a*x[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-2
b=1
c=0.5
d=-0.9

df=createTrajectory(5000000, 0, 0, a, b, c, d)
ggplot(df, aes(x, y)) + geom_point(color="black", shape=46, alpha=.01) + opt
ggsave("black_chase_1.png", device = "png")