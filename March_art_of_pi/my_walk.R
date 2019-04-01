library(tidyverse)
library(gganimate)

#read in digits
digits <- data.table::fread("data/PI_100000.txt")
class(digits$V1)

#split into individuals
data_split <- strsplit(digits$V1, "0")
data2 <- unlist(data_split)

#make list, remove empty entries
dat_list <- as.list(data2)
dat_list <- dat_list[dat_list != ""]

#make every other sequence negative
list_split <- lapply(dat_list, function(x) {stringi::stri_sub(x, seq(1, stringi::stri_length(x),by=1), length=1)})
list_num <- lapply(list_split, as.numeric)
list_num1 <- list_num[seq_along(list_num) %% 2 > 0]
list_num2 <- list_num[seq_along(list_num) %% 2 ==0]
list_num2 <- lapply(list_num2, function(x) {x*-1})
list_order <- order(c(seq_along(list_num1), seq_along(list_num2)))
list_comb <- c(list_num1, list_num2)[list_order]

#remove zeros from digits to match our encoding
pi_seq <- str_remove_all(digits$V1, "0")
#split into single digits
pi_seq <- sapply(pi_seq, function(x) {stringi::stri_sub(x, seq(1, stringi::stri_length(x),by=1), length=1)})
#calculate sum of the digits
df <- tibble(cumsum = cumsum(unlist(list_comb)), digit = pi_seq, index = 1:length(cumsum))

colors = c("#E01C70", "#6820A5", "#910599", "#31A79B", "#7BD657", "#B9E128", "#FFB3FD", "#FB9D4F", "#51ADFF", "#9239F6")

#static plot
ggplot(df, aes(x = index, y = cumsum, color = index)) +
  geom_line() +
  scale_color_viridis_c(option = "magma", direction = -1, guide = FALSE) +
  theme_minimal() +
  labs(x = "", y = "") +
  #scale_x_reverse() +
  #coord_flip() +
  theme(plot.background = element_rect(fill = "#181818", color = "NA"), panel.background = element_blank(),
        panel.grid.major = element_line(color = "#4A4E4D"), panel.grid.minor = element_blank(), 
        axis.text.x = element_blank())

#trying polar coords
ggplot(df, aes(x = index, y = cumsum, color = index)) +
  geom_line() +
  scale_color_viridis_c(option = "magma", direction = -1, guide = FALSE) +
  theme_minimal() +
  labs(x = "", y = "") +
  #scale_x_reverse() +
  #coord_flip() +
  theme(plot.background = element_rect(fill = "#181818", color = "NA"), panel.background = element_blank(),
        panel.grid.major = element_line(color = "#4A4E4D"), panel.grid.minor = element_blank(), 
        axis.text.x = element_blank()) +
  coord_polar()


#animate it!
anim <- 
  ggplot(df, aes(x = index, y = cumsum, color = index)) +
  geom_line() +
  scale_color_viridis_c(option = "magma", direction = -1, guide = FALSE) +
  theme_minimal() +
  labs(x = "", y = "") +
  #scale_x_reverse() +
  #coord_flip() +
  theme(plot.background = element_rect(fill = "#181818", color = "NA"), panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "#4A4E4D"), panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(), panel.grid.major.x = element_blank()) +
  transition_reveal(index)

animate(anim, nframes = 200, fps = 10, type = "cairo", width = 1500, height = 1000, renderer = gifski_renderer(loop = FALSE))
anim_save("my_walk.gif")
