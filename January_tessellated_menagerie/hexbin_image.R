library(tidyverse)
library(hexbin)
library(imager)

#load image
im <- load.image("inputs/texas.jpg") %>% grayscale()

# Filter image to convert it to bw
# sample points within image
im %>%
  #threshold("30%") %>% 
  as.cimg() %>% 
  plot() %>%
  as.data.frame() %>%
  # filter(x > 10 & x < 490) %>%
  # filter(y > 10 & y < 340) %>%
  sample_n(7000, weight = (1 - value)) %>% 
  select(x, y) -> df

#funct to space out points
buffer.f <- function(foo, buffer, reps){
  # Make list of suitable vectors
  suitable <- list()
  for(k in 1:reps){
    # Make the output vector
    outvec <- as.numeric(c())
    # Make the vector of dropped (buffered out) points
    dropvec <- c()
    for(i in 1:nrow(foo)){
      # Stop running when all points exhausted
      if(length(dropvec)<nrow(foo)){
        # Set the rows to sample from
        if(i>1){
          rowsleft <- (1:nrow(foo))[-c(dropvec)]
        } else {
          rowsleft <- 1:nrow(foo)
        }
        # Randomly select point
        outpoint <- as.numeric(sample(as.character(rowsleft),1))
        outvec[i] <- outpoint
        # Remove points within buffer
        outcoord <- foo[outpoint,c("x","y")]
        dropvec <- c(dropvec, which(sqrt((foo$x-outcoord$x)^2 + (foo$y-outcoord$y)^2)<buffer))
        # Remove unnecessary duplicates in the buffered points
        dropvec <- dropvec[!duplicated(dropvec)]
      } 
    } 
    # Populate the suitable points list
    suitable[[k]] <- outvec
  }
  # Go through the iterations and pick a list with the most data
  best <- unlist(suitable[which.max(lapply(suitable,length))])
  foo[best,]
}

df_buffer <- buffer.f(df, 15, 500)

#colors
pal <- c("#FFFF78", "#FFD92E", "#F7AD00", "#D15F02", "#523B27", "#523B27", "#2E2402")
pal2 <-c("#F7F7F7", "#002665", "#B80A2E", "#B80A2E")
#plot points to check
ggplot(df, aes(x=x, y=y)) + geom_point() + scale_y_reverse() + theme_void()

#plot hex image
ggplot(df, aes(x=x, y=y)) + 
  geom_hex(bins = 50) + 
  scale_fill_gradientn(colors = pal2, guide = FALSE) +
  scale_y_reverse() + 
  theme_void()

ggplot(df, aes(x=x, y=y)) + 
  geom_hex(bins = 50) + 
  scale_fill_gradientn(colors = pal, guide = FALSE) +
  scale_y_reverse() + 
  theme_void() +
  theme(panel.background = element_rect(fill = "#3B3B3B"))

ggplot(df, aes(x=x, y=y)) + 
  geom_hex(bins = 50, size = 0) + 
  scale_fill_gradientn(colors = pal, guide = FALSE) +
  scale_y_reverse() + 
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent"))

ggsave("hex_bee.pdf", device = "pdf", bg = "transparent")






########
#I was trying to figure out hexbin to get polygons
#but will do later if needed
hexes <- hexbin(df_buffer$x, df_buffer$y, xbins = 50)
hex_centers <- hcell2xy(hexes)
hex_centers <- as.data.frame(hex_centers)
hex_polys <- hexpolygon(hex_centers$x, hex_centers$y, hexC = hexcoords(dx = 8, dy = 8*sqrt(3)/2))
hex_df <- data.frame(x = as.numeric(hex_polys$x), y = as.numeric(hex_polys$y))
hex_df$id <- rep(1:length(hex_centers$x), each = 6)

ggplot(hex_df, aes(x = x, y = y, group = id)) + geom_polygon() + scale_y_reverse()

