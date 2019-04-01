library(tidyverse)
library(ggforce)
library(gganimate)

#read in digits
digits <- data.table::fread("data/PI_100000.txt")

#split into groups of 4
data_split <- stringi::stri_sub(digits$V1, seq(1, stringi::stri_length(digits$V1),by=4), length=4)

#make only 50 spirographs
test_set <- data_split[1:50]
test_list <- as.list(test_set)

#split the groups of 4 into single digits, then make into dataframes
test_list_split <- lapply(test_list, function(x) {stringi::stri_sub(x, seq(1, stringi::stri_length(x),by=1), length=1)}  )
test_df_list <- lapply(test_list_split, function(x) {data.frame(R = x[1], r = x[2], d = x[3], outer = x[4], stringsAsFactors = FALSE)})

#bind into a single dataframe and make numeric
test_df_final <- bind_rows(test_df_list, .id = "id") 
test_df_final[] <- lapply(test_df_final, as.numeric)

colors = c("#E01C70", "#6820A5", "#910599", "#31A79B", "#7BD657", "#B9E128", "#FFB3FD", "#FB9D4F", "#51ADFF", "#9239F6")

#choose outer or not, increment so we don't have any 0s in spirograph--throws error
pi_df <- 
  test_df_final %>%
  mutate(outer = ifelse(outer < 5, FALSE, TRUE), 
         R = R + 1, r = r + 1, d = d + 1, color = sample(colors, 50, replace = TRUE)) 

#plot static
ggplot() +
  geom_spiro(data = pi_df, aes(R = R, r = r, d = d, outer = outer, color = color, group = id), size = 1) +
  theme_void() +
  scale_color_identity() +
  facet_wrap( ~id) +
  coord_equal() +
  theme(plot.background = element_rect(color = "NA", fill = "#14183D"), strip.background = element_blank(),
        strip.text.x = element_blank())

ggsave("sPIrograph_50.png", device = "png", type = "cairo", height = 20, width = 20)

#I decided on this after plotting static and choosing
#these are which ones to keep
keep <- c(12, 2, 4, 8, 15, 19, 20, 22, 23, 25, 31, 32, 34, 48)
pi_df2 <- 
  pi_df %>%
  filter(id %in% keep)

# create normal ggplot object
# (note: setting group = "some constant" wouldn't have change anything here;
# each row of data would still correspond to a different group value...)
p <- ggplot() +
  geom_spiro(data = pi_df2, aes(R = R, r = r, d = d, outer = outer, color = color), size = 1) +
  theme_void() +
  scale_color_identity() +
  coord_equal() +
  theme(plot.background = element_rect(color = "NA", fill = "#14183D"), strip.background = element_blank(),
        strip.text.x = element_blank())

# get layer data created by geom_spiro; sort the rows just in case
data2 <- layer_data(p) %>%
  select(x, y, colour, group, index) %>%
  arrange(group, index)

# use group as the transition variable, & set the actual group aesthetic
# to some constant
anim <- 
  ggplot(data2,
       aes(x = x, y = y, group = "a")) +
  geom_path(aes(color = colour), size = 1) + 
  theme_void() +
  scale_color_identity() +
  coord_equal() +
  theme(plot.background = element_rect(color = "NA", fill = "#14183D"), strip.background = element_blank(),
        strip.text.x = element_blank()) +
  transition_states(states = group, transition_length = 3, state_length = 3) +
  ease_aes("back-in-out")

animate(anim, nframes = 300, detail = 3, fps = 10, device = "png", type = "cairo", height = 700, width = 700)
anim_save("spiroband.gif")
