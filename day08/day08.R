library("tidyverse")

example <- c("30373",
             "25512",
             "65332",
             "33549",
             "35390")
example <- map2(example, 1:length(example), ~tibble(!!paste0("V", .y) := strsplit(.x, "")[[1]])) %>% bind_cols()

width <- nchar(readLines("day08/input.txt", n = 1))

input <- read.fwf("day08/input.txt", widths = rep(1, width)) %>% as.matrix()

tree_checker <- function(dat){
  width <- ncol(dat)
  coords_to_check <- expand.grid(x = 2:(width-1), y = 2:(width-1))
  
  is_visible <- function(dat, x, y, xmax, ymax){
    tree <- as.numeric(dat[[x,y]])
    adjacent <- list("left" = dat[(x-1):1, y], 
                     "right" = dat[(x+1):xmax, y], 
                     "up" = dat[x, (y-1):1],
                     "down" = dat[x, (y+1):ymax])
    
    out <- any(map_lgl(adjacent, ~all(as.numeric(unlist(.x)) < tree)))
    #print(paste(tree, out))
    return(out)
    
  }
  
  sum(pmap_dbl(coords_to_check, ~is_visible(dat, .x, .y, width, width))) + (width*2) + (width-2)*2
}

tree_checker(example)
tree_checker(input)

#part 2

tree_rater <- function(dat){
  width <- ncol(dat)
  coords_to_check <- expand.grid(x = 2:(width-1), y = 2:(width-1))
  
  is_visible <- function(dat, x, y, xmax, ymax){
    tree <- as.numeric(dat[[x,y]])
    adjacent <- list("left" = dat[(x-1):1, y], 
                     "right" = dat[(x+1):xmax, y], 
                     "up" = dat[x, (y-1):1],
                     "down" = dat[x, (y+1):ymax])
    
    view_score <- map(adjacent, function(view){
      view <- unlist(view)
      first_block <- which(view >= tree)[1]
      if(is.na(first_block)){
        return(length(view))
      } else {
        return(length(view[1:first_block]))
      }
    })

    reduce(view_score, prod)
  }
  
  max(pmap_dbl(coords_to_check, ~is_visible(dat, .x, .y, width, width)))
}

tree_rater(example)
tree_rater(input)
