library(magick)

img = image_read("data/sample.jpg")


chip_size = 512
overlap = 50

### get image dims
info = image_info(img)
width = info$width
height = info$height


### get number of horizontal and vertical chips (does not include the final rightmost and bottom-most chip in each dimension)
n_h_ch = width / (chip_size*(overlap/100)) %>% floor
n_v_ch = height / (chip_size*(overlap/100)) %>% floor

### get the coords of the upper left corner of each chip
corners_x = 
  
corners_y = 



## record orig size

