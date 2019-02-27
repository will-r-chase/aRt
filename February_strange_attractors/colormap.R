library(EBImage)
library(colourlovers)
library(tidyverse)
library(ghibli)

#read in the image and convert to greyscale
img <- readImage("chase_6.png")
gray <- channel(img, "gray")

#you can generate a random palette from colourlovers like this
ran_pal <-
  colorRampPalette(
    sample(clpalettes('top'), 1)[[1]] %>% 
      swatch %>% 
      .[[1]])

#or define your own palette
blue_beige <- colorRampPalette(c("#6386A5", "#fbe3d1"))

#map the color palette to the image
#play round and try different stuff
img_col <- colormap(gray, blue_beige(256))
display(img_col, method = "raster")

img_col <- colormap(gray, ghibli_palette("MononokeMedium", 256, type = "continuous"))
display(img_col, method = "raster")

img_col <- colormap(gray, rev(ghibli_palette("SpiritedMedium", 256, type = "continuous")))
display(img_col, method = "raster")

img_col <- colormap(gray, ghibli_palette("PonyoMedium", 256, type = "continuous"))
display(img_col, method = "raster")

img_col <- colormap(gray, viridis::magma(256))
display(img_col, method = "raster")

#when you are satisfied with the image, save it
writeImage(img_col, "chase_6_color.png")





######my stuff##############
img <- readImage("diamond_black.png")
gray <- channel(img, "gray")

img_col <- colormap(gray, rev(ghibli_palette("LaputaMedium", 256, type = "continuous")))
display(img_col, method = "raster")

writeImage(img_col, "diamond_color.png")

###################################
img <- readImage("black_box.png")
gray <- channel(img, "gray")

img_col <- colormap(gray, rev(ghibli_palette("KikiMedium", 256, type = "continuous")))
display(img_col, method = "raster")

writeImage(img_col, "box_color.png")

###################################
img <- readImage("black_chase_1.png")
gray <- channel(img, "gray")

img_col <- colormap(gray, ghibli_palette("PonyoMedium", 15, type = "continuous"))
display(img_col, method = "raster")

writeImage(img_col, "chase_1_color.png")
