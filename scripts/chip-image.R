library(magick)
library(tidyverse)

img = image_read("data/sample.jpg")


chip_size = 512
overlap = 50

### get image dims
info = image_info(img)
width = info$width
height = info$height


### get number of horizontal and vertical chips (does not include the final rightmost and bottom-most chip in each dimension)
window_step = (chip_size*(overlap/100))

n_h_ch = floor(width / (chip_size*(overlap/100))) - 1 # subtract one because the final value less then a full chip-width from the edge of the image
n_v_ch = floor(height / (chip_size*(overlap/100))) - 1

### get the coords of the upper left corner of each chip
cols = 1:n_h_ch
rows = 1:n_v_ch

corners_x = (cols-1) * window_step
corners_y = (rows-1) * window_step
# add a final column to include the incomplete remainder
corners_x = c(corners_x,(width-chip_size))
corners_y = c(corners_y,(height-chip_size))

tiles = expand_grid(x_coord=corners_x,y_coord=corners_y)
tiles$colnum = rep(1:length(corners_x),each=(length(corners_y)))
tiles$rownum = rep(1:length(corners_y),(length(corners_x)))
tiles$tile_id = 1:nrow(tiles)

## crop to each tile; write to file
for(i in tiles$tile_id) {
  
  tile_x = tiles[i,"x_coord"]
  tile_y = tiles[i,"y_coord"]
  
  tile_colnum = tiles[i,"colnum"]
  tile_rownum = tiles[i,"rownum"]
  
  crop_string = paste0(chip_size,"x",chip_size,"+",tile_x,"+",tile_y)
  img_crop = image_crop(img,crop_string)
  
  # name for image is colnum-rownum
  name = paste0("chip_",tile_colnum,"-",tile_rownum,".jpg")
  
  image_write(img_crop,paste0("data/chipped/",name),format="jpg")
}




## record orig size

