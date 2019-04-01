library(tidyverse)
library(data.table)
library(rnaturalearth)
library(rnaturalearthdata)
library(maptools)
library(sf)
library(SOmap)
library(ggforce)
library(gganimate)

#note I used a larger pi file which I can't upload to github
#you can use a smaller one in the /data/ folder
#or message me directly for the big pi_billion file

#read in data
digits <- fread("pi_billion.txt")

#split into groups of 11
data_split <- stringi::stri_sub(digits$V1, seq(1, stringi::stri_length(digits$V1),by=11), length=11)
data_split[1]

#subset a smaller amount
test_set <- data_split[1:1000000]
test_list <- as.list(test_set)

#split into 5-5-1 groups
test_list_split <- lapply(test_list, function(x) {stringi::stri_sub(x, seq(1, stringi::stri_length(x),by=5), length=5)}  )
test_df_list <- lapply(test_list_split, function(x) {data.frame(x_seq = x[1], y_seq = x[2], color = x[3], stringsAsFactors = FALSE)})

#bind into dataframe and convert to numeric
test_df_final <- bind_rows(test_df_list, .id = "id")
test_df_final[] <- lapply(test_df_final, as.numeric)

#convert sampled digits to lat/lon space
lat_direct <- scales::rescale(test_df_final$y_seq, to = c(-90, 90))
lon_direct <- scales::rescale(test_df_final$x_seq, to = c(-180, 180))
loc_direct <- data.frame(lon = lon_direct, lat = lat_direct)

#figure out which points are on land/ocean
data("wrld_simpl")
pts <- SpatialPoints(loc_direct, proj4string=CRS(proj4string(wrld_simpl)))
## Find which points fall over land
ii <- !is.na(over(pts, wrld_simpl)$FIPS)
loc_direct$land <- ii

#this is to get country borders for plotting/transformation
countries <- ne_countries(returnclass = "sf")
world_proj <- st_transform(countries, crs = 54009)

#project points to mollweide projections
points2 <- SOproj(lon = loc_direct$lon, lat = loc_direct$lat, data = loc_direct$land, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0+ellps=WGS84 +datum=WGS84 +units=m")

points2 <- as.data.frame(points2)

names(points2) <- c("land", "x", "y")

#set color
points2$color <- test_df_final$color
colors <- c("#E01C70", "#6820A5", "#910599", "#31A79B", "#7BD657", "#B9E128", "#FFB3FD", "#FB9D4F", "#51ADFF", "#9239F6")
points3 <-
  points2 %>%
  filter(land) %>%
  mutate(color = as.factor(color))

#saving points because it's slow to get them
#saveRDS(points3, "pi_1mil_sets.rds")
points3 <- readRDS("pi_1mil_sets.rds")

#subset down to 30,000 and add index col
points4 <- points3[1:30000, ]
points4 <- 
  points4 %>% 
  mutate(row = row_number())

#plot static
ggplot() +
  geom_sf(data = world_proj, fill = "NA", col = "NA", lwd = 0) +
  geom_point(data = points4, aes(x, y, color = color), stroke = 0, alpha = 0.7, size = 1) +
  scale_color_manual(values = c("#E01C70", "#6820A5", "#910599", "#31A79B", "#7BD657", "#B9E128", "#FFB3FD", "#FB9D4F", "#51ADFF", "#9239F6"), guide = FALSE) +
  theme_minimal() +
  labs(title = "Digits of pi projected to the globe", subtitle = "Digit: ") +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid.major=element_line(colour="transparent"),
        panel.background = element_rect(fill = "#14183D", color = "NA"), plot.background = element_rect(fill = "#14183D", color = "NA"), 
        panel.border = element_blank(), plot.title = element_text(color = "white"), plot.subtitle = element_text(color = "white"))

ggsave("test_png_1.png", device = "png", type = "cairo", height = 8, width = 10)

#to animate, save pngs and then make into gif/movie with ffmpeg or gifski
for(i in 1:300) {
  p <- 
    ggplot() +
    geom_sf(data = world_proj, fill = "NA", col = "NA", lwd = 0) +
    geom_point(data = points4[1:(i*100), ], aes(x, y, color = color), stroke = 0, alpha = 0.7, size = 1) +
    scale_color_manual(values = c("#E01C70", "#6820A5", "#910599", "#31A79B", "#7BD657", "#B9E128", "#FFB3FD", "#FB9D4F", "#51ADFF", "#9239F6"), guide = FALSE) +
    theme_minimal() +
    labs(title = "Digits of pi projected to the globe", subtitle = paste0("Digit: ", i*100*11)) +
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid.major=element_line(colour="transparent"),
          panel.background = element_rect(fill = "#14183D", color = "NA"), plot.background = element_rect(fill = "#14183D", color = "NA"), 
          panel.border = element_blank(), plot.title = element_text(color = "white"), plot.subtitle = element_text(color = "white"))
  
  ggsave(paste0("gif_frames/", "filename-", str_pad(i, 3, pad="0"), ".png"), p, device = "png", type = "cairo", height = 8, width = 10)
}
