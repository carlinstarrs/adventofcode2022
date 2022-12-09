library("tidyverse")

example <- c("R 4",
             "U 4",
             "L 3",
             "D 1",
             "R 4",
             "D 1",
             "L 5",
             "R 2")



input <- readLines("day09/input.txt") 

get_headcoords <- function(dat){
  moves <- dat %>% 
    strsplit(" ") %>% 
    unlist() %>% 
    matrix(ncol = 2, byrow = TRUE) 
  
  head_coords <- matrix(c(0, 0), ncol = 2)
  
  #translate motions to coordinates
  walk2(moves[,1], as.numeric(moves[,2]), function(direction, distance){
    x <- tail(head_coords, 1)[1]
    y <- tail(head_coords, 1)[2]
    
    coords <- case_when(direction == "R" ~ c(x:(x-distance), rep(y, distance + 1)),
                        direction == "L" ~ c(x:(x+distance), rep(y, distance + 1)),
                        direction == "U" ~ c(rep(x, distance + 1), y:(y-distance)),
                        direction == "D" ~ c(rep(x, distance + 1), y:(y+distance)))
    
    coords <- matrix(coords, ncol = 2)[-1,]
    head_coords <<- rbind(head_coords, coords)
  })
  
  return(head_coords)
}

move_tail <- function(headx, heady, tailx, taily){
  distx <- abs(headx-tailx) 
  disty <- abs(heady-taily)
  
  if(distx + disty < 2){ #don't move
    next_tailx <- tailx
    next_taily <- taily
  } else if(disty == 2 & headx == tailx){ #head must have moved vertically
    #tail should move vertically the same direction
    next_tailx <- tailx
    next_taily <- taily + (1 * sign(heady-taily))
  } else if(distx == 2 & heady == taily){ #head must have moved horizontally
    next_tailx <- tailx + (1 * sign(headx-tailx))
    next_taily <- taily
  } else if(distx + disty > 2){ #head is more than 2 away in a diagonal direction
    next_tailx <- tailx + (1 * sign(headx-tailx))
    next_taily <- taily + (1 * sign(heady-taily))
  } else { #head must have moved diagonally, don't move
    next_tailx <- tailx
    next_taily <- taily
  }
  
  matrix(c(next_tailx, next_taily), ncol = 2)
}


stretch_rope <- function(dat, starting_knots){
  knots <- map(1:starting_knots, ~matrix(c(0, 0), ncol = 2))
  knots[[1]] <- get_headcoords(dat)
  
  walk(1:nrow(knots[[1]]), function(i){
    map(2:length(knots), function(knot){
      head_coords <- knots[[knot-1]]
      tail_coords <- knots[[knot]]
      
      knots[[knot]] <<- rbind(knots[[knot]], move_tail(head_coords[i,1], head_coords[i,2], tail_coords[i,1], tail_coords[i,2]))
    })
  })
  
  tibble(tail(knots, 1)[[1]]) %>% 
    unique() %>% 
    nrow()
}

stretch_rope(example, 2)
stretch_rope(input, 2)
stretch_rope(input, 10)






