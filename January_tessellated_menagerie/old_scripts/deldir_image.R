library(imager)
library(dplyr)
library(deldir)
library(ggplot2)
library(scales)
library(colourlovers)
library(rlist)
library(purrr)

# Read and convert to grayscale
load.image("inputs/giraffe.jpg") %>% grayscale() -> x

# This is just to define frame limits
x %>% 
  as.data.frame() %>% 
  group_by() %>% 
  summarize(xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y)) %>% 
  as.vector()->rw

# Filter image to convert it to bw
x %>%
  threshold("45%") %>% 
  as.cimg() %>% 
  as.data.frame() -> df
  
#already binary
x %>%
  as.cimg() %>%
  as.data.frame() -> df

# Function to compute and plot Voronoi tesselation depending on sample size
doPlot = function(n) {
  #Voronoi tesselation
  df %>% 
    sample_n(n, weight=(1-value)) %>% 
    select(x,y) %>% 
    deldir(rw=rw, sort=TRUE) %>% 
    .$delsgs -> data
  
  # This is just to add some alpha to lines depending on its longitude
  data %>% 
    mutate(long=sqrt((x1-x2)^2+(y1-y2)^2),
           alpha=findInterval(long, quantile(long, probs = seq(0, 1, length.out = 20)))/21)-> data
  
  # A little bit of ggplot to plot results
  data %>% 
    ggplot(aes(alpha=(1-alpha))) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color="black", lwd=1) +
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0), trans=reverse_trans())+
    theme(legend.position  = "none",
          panel.background = element_rect(fill="white"),
          axis.ticks       = element_blank(),
          panel.grid       = element_blank(),
          axis.title       = element_blank(),
          axis.text        = element_blank())->plot
  
  return(plot)
}

doPlot(2000)


############for filled image
# Read and convert to grayscale
load.image("inputs/giraffe.jpg") %>% grayscale() -> x

# This is just to define frame limits
x %>% 
  as.data.frame() %>% 
  group_by() %>% 
  summarize(xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y)) %>% 
  as.vector()->rw

# Filter image to convert it to bw
x %>%
  threshold("45%") %>% 
  as.cimg() %>% 
  as.data.frame() -> df

#already binary
x %>%
  as.cimg() %>%
  as.data.frame() -> df

#sample points in black area
x %>%
  as.cimg() %>%
  as.data.frame() %>%
  sample_n(600, weight = (1 - value)) %>% 
  select(x, y) -> df

#for voronoi
crea = function(tile) {
  tile %>% 
    list.match("ptNum|x|y|area") %>% 
    as.data.frame()
}

#for triang
triang_area <- function(data) {
  x <- data$x
  y <- data$y
  mat <- matrix(data = c(1,1,1,x[1],x[2],x[3],y[1],y[2],y[3]), nrow = 3, ncol = 3, byrow = TRUE)
  area <- 0.5*det(mat)
  return(area)
}

#for triang
df %>% 
  deldir(sort = TRUE)  %>% 
  triang.list() %>% 
  map( ~mutate(.x, area = triang_area(.x))) %>%
  bind_rows(.id = "id") %>%
  select(id, x, y, area) -> triang_df

#for voronoi
df %>% 
  deldir(sort = TRUE)  %>% 
  tile.list() %>% 
  list.filter(sum(bp) == 0) %>% 
  list.filter(length(intersect(which(x == 0), which(y == 0))) == 0) %>% 
  lapply(crea) %>% 
  list.rbind() ->  df_polygon

#choose random color pal
palette <- 
  sample(clpalettes('top'), 1)[[1]] %>% 
  swatch %>% 
  .[[1]]

# Draw image with geom_polygon. Colur depends on area
ggplot(df_polygon, aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, color=area, group = ptNum), 
               show.legend = FALSE, size=0)+
  scale_fill_gradientn(colors=sample(palette, length(palette))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), trans=reverse_trans())

#for triang
ggplot(triang_df, aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, color=area, group = id), 
               show.legend = FALSE, size=0)+
  scale_fill_gradientn(colors=sample(palette, length(palette))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), trans=reverse_trans())

