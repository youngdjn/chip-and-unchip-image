library(magick)
library(tidyverse)

tile_dir = "data/chipped/"
write_file = "data/sample_reassembled.jpg"

chip_dat = read_csv(paste0(tile_dir,"chip_dat.csv"))

# compute the number of pixels to chop off the sides when stitching the overlapping tiles together
clip_width = (chip_dat$chip_size - chip_dat$window_step) / 2
keep_width = chip_dat$chip_size - 2 * clip_width

# compute the standard crop offset
offset = clip_width

## if it's the right edge or bottom edge and it overlaps more than the standard amount, how much to clip for the left or top side?
last_col_extends = chip_dat$last_col_width
horiz_keep_width_lastcol = last_col_extends + clip_width
horiz_offset_lastcol = chip_dat$chip_size - horiz_keep_width_lastcol

last_row_extends = chip_dat$last_row_width
vert_keep_width_lastrow = last_row_extends + clip_width
vert_offset_lastrow = chip_dat$chip_size - vert_keep_width_lastrow

## if it's the first row or col, don't clip off the top or left edge as is done for all the internal ones
vert_keep_width_firstrow = keep_width + clip_width
horiz_keep_width_firstcol = keep_width + clip_width




## make a table of chip row and column and the chip file name

chip_files = list.files(tile_dir,pattern="(jpg$|png$|JPG$|PNG$|jpeg$|JPEG$|tif$|TIF$|tiff$|TIFF$)")

chip_table = data.frame(filename = chip_files %>%as.character)

chip_table = chip_table %>%
  mutate(pre_extension = str_split(filename,pattern=fixed(".")) %>% map(1)) %>%
  mutate(coords = str_split(pre_extension, pattern = fixed("_")) %>% map(2)) %>%
  mutate(row = str_split(coords,pattern=fixed("-")) %>% map(1) %>% as.numeric) %>%
  mutate(col = str_split(coords,pattern=fixed("-")) %>% map(2) %>% as.numeric) %>%
  arrange(row,col)


rows_unmerged = NULL
  
for(i in 1:chip_dat$nrows) {
  
  chip_row_unmerged = NULL
  
  for(j in 1:chip_dat$ncols) {
    
    # open tile
    tile_dat = chip_table %>%
      filter(row == i, col == j)
    tile_file = paste0(tile_dir,tile_dat$filename)
    tile = image_read(tile_file)
    
    # make the crop text
    
    #defaults
    vert_offset = offset
    horiz_offset = offset
    vert_keep_width = keep_width
    horiz_keep_width = keep_width
    
    #special cases
    if(i == 1) { # first row
      vert_offset = 0
      vert_keep_width = vert_keep_width_firstrow
    }
    
    if(j == 1) { # first col
      horiz_offset = 0
      horiz_keep_width = horiz_keep_width_firstcol
    }
    
    if(i == chip_dat$nrows) { # last row
      vert_offset = vert_offset_lastrow
      vert_keep_width = vert_keep_width_lastrow
    }
    
    if(j == chip_dat$ncols) { # last col
      horiz_offset = horiz_offset_lastcol
      horiz_keep_width = horiz_keep_width_lastcol
    }
    
    crop_string = paste0(horiz_keep_width,"x",vert_keep_width,"+",horiz_offset,"+",vert_offset)
    
    tile_cropped = image_crop(tile, crop_string)
    
    if(length(chip_row_unmerged) == 0) {
      chip_row_unmerged = tile_cropped
    } else {
      chip_row_unmerged = c(chip_row_unmerged,tile_cropped)
    }
  
  }
  
  chip_row_merged = image_append(chip_row_unmerged)
  
  if(length(rows_unmerged) == 0) {
    rows_unmerged = chip_row_merged
  } else {
    rows_unmerged = c(rows_unmerged,chip_row_merged)
  }


}

reassembled = image_append(rows_unmerged,stack = TRUE)

image_write(reassembled, write_file)
