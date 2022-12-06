library("tidyverse")

input <- readLines("day06/input.txt")

find_marker <- function(dat, marker = c("packet", "message")){ 
  breaks <- switch(marker, 
                   "packet" = 4, 
                   "message" = 14)
  
  dat <- strsplit(dat, "")[[1]]
  
  starts <- 1:(length(dat)- (breaks - 1))
  ends <- breaks:length(dat)
  
  sets <- map2(starts, ends, ~dat[.x:.y])
  first <- which(map_lgl(sets, ~length(unique(.x)) == breaks))[1]
  
  str_locate(paste(dat, collapse = ""), paste0("(", paste(sets[[first]], collapse = ""), ")"))[1,2]
}

find_marker(input, "packet")
find_marker(input, "message")
