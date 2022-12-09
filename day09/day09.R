#Consider a rope with a knot at each end; these knots mark the head and the tail of the rope. 
#If the head moves far enough away from the tail, the tail is pulled toward the head.


#you should be able to model the positions of the knots on a two-dimensional grid

#Then, by following a hypothetical series of motions (your puzzle input) for the head, you can determine how the tail will move.

#in fact, the head (H) and tail (T) must always be touching (diagonally adjacent and even overlapping both count as touching):

#If the head is ever two steps directly up, down, left, or right from the tail, the tail must also move one step in that direction so it remains close enough:

#Otherwise, if the head and tail aren't touching and aren't in the same row or column, the tail always moves one step diagonally to keep up:


#You just need to work out where the tail goes as the head follows a series of motions. Assume the head and the tail both start at the same position, overlapping.

example <- c("R 4",
             "U 4",
             "L 3",
             "D 1",
             "R 4",
             "D 1",
             "L 5",
             "R 2")



input <- readLines("day09/input.txt") 

dat <- example

dat <- input
moves <- dat %>% 
  strsplit(" ") %>% 
  unlist() %>% 
  matrix(ncol = 2, byrow = TRUE) 

head_coords <- matrix(c(0, 0), ncol = 2)

#translate matrix to coordinates
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

tail_coords <- matrix(c(0, 0), ncol = 2)

walk2(head_coords[,1], head_coords[,2], function(headx, heady){
  tailx <- tail(tail_coords, 1)[1]
  taily <- tail(tail_coords, 1)[2]
  
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
  
  coords <-  matrix(c(next_tailx, next_taily), ncol = 2)
  
  tail_coords <<- rbind(tail_coords, coords)
})


tibble(tail_coords) %>% 
  unique() %>% 
  nrow()


