library(tidyverse)
library(gglogo)

extrafont::fonts()

alphas <- c(letters, 0:9, ":")
alphabet_mine <- createPolygons(alphas, font="Times New Roman")
alphabet_mine$group <- factor(alphabet_mine$group, levels=alphas)

qplot(x, y, geom="polygon", data=alphabet_mine) + 
  facet_wrap(facets=~group, ncol=13) +
  coord_equal()

text_list <- 
  split(alphabet_mine, alphabet_mine$group)

line1 <- "rstudio::conf"
line2 <- "2019"

text_line1 <- 
  tibble(group = unlist(str_split(line1, "")), pos = 1:nchar(line1)) %>%
  left_join(alphabet_mine, by = "group") %>%
  mutate(x_new = x)

line1_list <-  split(text_line1, text_line1$pos)

for(i in 2:length(line1_list)) {
  prev_max <- max(line1_list[[i-1]]$x_new)
  line1_list[[i]]$x_new <- line1_list[[i]]$x_new + prev_max
}

line1_df <- bind_rows(line1_list)
line1_df[is.na(line1_df$x_new), "x_new"] <- line1_df[is.na(line1_df$x_new), "x"]
line1_df$y_new <- line1_df$y
ggplot(line1_df, aes(x = x_new, y = y, group = pos)) + geom_polygon(fill = "black") + theme_void()

text_line2 <- 
  tibble(group = unlist(str_split(line2, "")), pos = 1:nchar(line2)) %>%
  left_join(alphabet_mine, by = "group") %>%
  mutate(x_new = x)

line2_list <-  split(text_line2, text_line2$pos)

for(i in 2:length(line2_list)) {
  prev_max <- max(line2_list[[i-1]]$x_new)
  line2_list[[i]]$x_new <- line2_list[[i]]$x_new + prev_max
}

line2_df <- bind_rows(line2_list)
line2_df[is.na(line2_df$x_new), "x_new"] <- line2_df[is.na(line2_df$x_new), "x"]
line2_df$x_new <- line2_df$x_new + max(line1_df$x_new)/2.83
line2_df$y_new <- line2_df$y - max(line1_df$y)
line2_df$pos <- line2_df$pos + max(line1_df$pos)

both_lines_df <- rbind(line1_df, line2_df)

ggplot(both_lines_df, aes(x = x_new, y = y_new, group = pos)) + geom_polygon(fill = "black") + theme_void()

ggsave("rstudio_conf_poly.png", device = "png", type = "cairo", height = 3, width = 10, units = "in")
saveRDS(both_lines_df, "text_polys.rds")
