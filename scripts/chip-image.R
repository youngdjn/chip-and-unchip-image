library(magick)
library(tidyverse)

img = image_read("data/sample.jpg")
chip_dir = "data/chipped/"


chip_size = 512 # pixels
overlap = 50 # percent

if(chip_size %% 4 != 0) {
  stop("Chip size must be a multiple of 4.")
}

### get number of horizontal and vertical chips (does not include the final rightmost and bottom-most chip in each dimension)
window_step = ((chip_size*(overlap/100))/2 %>% floor)*2


### get image dims
info = image_info(img)
width = info$width
height = info$height




n_h_ch = floor(width / window_step) - 1 # subtract one because the final value less then a full chip-width from the edge of the image
n_v_ch = floor(height / window_step) - 1

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
  name = paste0("chip_",tile_rownum,"-",tile_colnum,".jpg")
  
  image_write(img_crop,paste0("data/chipped/",name),format="jpg")
}


### Write data needed to reconstruct

# overlap percent
# n tiles wide
# n tiles long
# total width
# total length

# how much does the last col extend beyond the second-to-last?
last_col_width = nth(corners_x,-1) - nth(corners_x,-2)
last_row_width = nth(corners_y,-1) - nth(corners_y,-2)

chip_dat = data.frame(overlap = overlap, chip_size = chip_size, window_step = window_step, ncols = n_h_ch+1, nrows = n_v_ch+1, last_col_width, last_row_width, total_width = width, total_height = height)

write_csv(chip_dat,paste0(chip_dir,"chip_dat.csv"))


