library(fields)
library(gganimate)
library(tidyverse)

#Read in pi string and turn into vector
piChar <- read.table("data/PI_og_100000.txt", stringsAsFactors=F, colClasses = c("character"))[1,1]
piVec <- as.numeric(strsplit(piChar, "")[[1]])

#Starting at number 3 at position 0,0
x <- y <- rep(NULL, length(piVec))
x[1] <- 0
y[1] <- 0

#Calculate new position for each digit, based on the position of the old digit and the 
#angle is determined by the digit itself
for (i in 2:length(piVec)){
  x[i] <- x[(i-1)] + sin((pi*2)*(piVec[i]/10))
  y[i] <- y[(i-1)] + cos((pi*2)*(piVec[i]/10))  
}

#color schemes
rainbowColDark <- c("#EFB605","#EB8612","#DD0030","#B3005A","#80348F","#3365AA","#07A071","#7EB852")
rainbowColDark <- designer.colors(n=10, col=rainbowColDark)

#Save all information in a data frame for plotting with ggplot2
Pi.frame <- data.frame(PI=piVec[-1], x=x[-length(x)], y=y[-length(y)], 
                       ID=1:(length(x)-1), stringsAsFactors=F)

#static
ggplot(Pi.frame[1:8000,], aes(x=x, y=y, group = "1")) +
  geom_path(aes(color = ID), size=0.7) + 
  scale_colour_gradientn(colours = rainbowColDark) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank()) 
##THIS WORKS##
#Color according to position in pi, thus looping throug the chosen colors once
piPlot <- ggplot(Pi.frame[1:1000,], aes(x=x, y=y, group = "1")) +
  geom_path(aes(color = ID), size=0.7) + 
  scale_colour_gradientn(colours = rainbowColDark) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank()) +
        transition_reveal(id = "1", along = ID) +
        view_follow()

animate(piPlot, nframes = 200, fps = 10, type = "cairo", rewind = TRUE)

#this is to make it slowww down, incrementing the first 24 points
times <- rep(100, nrow(Pi.frame))
times[1:24] <- c(50000, 40000, 30000, 30000, 30000, 30000, 20000, 20000, 20000, 10000, 10000, 10000, 10000, 10000, 5000, 5000, 5000, 5000, 1000, 1000, 1000, 500, 500, 500)

pi_slowdown <- Pi.frame %>%
  mutate(show_time = ifelse(ID %in% 1:100, times, 1),
         reveal_time = cumsum(show_time))

#this makes it pause at positions in show_time
pi_pause <- Pi.frame %>%
  mutate(show_time = ifelse(ID %in% c(500, 2000, 5000, 7000), 500, 1)) %>%
  uncount(show_time) %>%
  mutate(reveal_time = row_number())

pi_slow <- ggplot(pi_slowdown[1:nrow(pi_slowdown),], aes(x=x, y=y, group = "1")) +
  geom_path(aes(color = ID), size=0.7) + 
  scale_colour_gradientn(colours = rainbowColDark) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank()) +
  transition_reveal(id = "1", along = reveal_time) +
  view_follow()

animate(pi_slow, nframes = 200, fps = 10, type = "cairo", renderer = av_renderer())
anim_save("pi_slow.mp4")

pi_stop <- ggplot(pi_pause[1:nrow(pi_pause),], aes(x=x, y=y, group = "1")) +
  geom_path(aes(color = ID), size=0.7) + 
  scale_colour_gradientn(colours = rainbowColDark) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank()) +
  transition_reveal(id = "1", along = reveal_time) +
  view_follow()

animate(pi_stop, nframes = 400, fps = 10, type = "cairo", renderer = av_renderer())
anim_save("pi_pause.mp4")
