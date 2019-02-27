#bedhead
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(x[i-1]*y[i-1]/b)*y[i-1]+cos(a*x[i-1]-y[i-1]);
            y[i] = x[i-1]+sin(y[i-1])/b;
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=1
b=0.25

df3=createTrajectory(4000000, 1, 1, a, b)

#something new
#color by dist from origin
eu_dist <- function(x1, y1, x2, y2) {
  sqrt((x1-x2)^2 + (y1-y2)^2)
}

test_df <- df3[1:100, ]

df3$dist <- map2_dbl(df3$x, df3$y, ~eu_dist(.x, .y, 1, 1))

pulse_pal <- colorRampPalette(c("#FE1BE1", "#A300FF", "#57F7F5", "#57F7F5", "#57F7F5", "#57F7F5"))
pulse_pal2 <- colorRampPalette(c("#FE1BE1", "#FE1BE1", "#57F7F5", "#57F7F5", "#57F7F5", "#57F7F5", "#57F7F5"))
pulse_pal3 <- colorRampPalette(c("#FE1BE1", "#AA1BFE", "#57F7F5", "#57F7F5", "#57F7F5"))
pulse_pal4 <- colorRampPalette(c("#AA1BFE", "#57F7F5", "#57F7F5"))

#clip outer points
xmax <- max(df3$x)/2.5
xmin <- min(df3$x)/2.5
ymax <- max(df3$y)/2.5
ymin <- min(df3$y)/2.5

df3_clip <- df3 %>%
  filter(x > xmin & x < xmax) %>%
  filter(y > ymin & y < ymax)

#plot
ggplot(df3_clip, aes(x, y)) + 
  geom_point(aes(color = dist), shape=46, alpha=.01) + 
  scale_color_gradientn(colors=pulse_pal(500)) +
  opt

ggsave("pulse4.png", device = "png")