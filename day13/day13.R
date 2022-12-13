library("tidyverse")

input <- read_file("day13/input.txt")
example <- read_file("day13/example.txt")

dat <- example


dat2 <- dat %>% 
  str_split(., "\n\n|\r\n\r\n") %>% 
  map(~str_split(.x, "\n|\r\n")) %>% 
  .[[1]]

list_comparison <- function(x, y){
  browser()
  x <- unlist(x, recursive = FALSE)
  y <- unlist(y, recursive = FALSE)
  
  out <- do.call(get_func(class(x), class(y)), list(x = x, y = y))
  return(out)
}

integer_comparison <- function(x,y){
  if(x == y){ 
    return(NA)
  } else if(x < y) {
    return(TRUE)
  } else if(x > y){
    return(FALSE)
  }
}

get_func <- function(class1, class2){
  if(all(c(class1, class2) == "integer")){
    return("integer_comparison")
  } else if(all(c(class1, class2) == "list")){
    return("list_comparison")
  } else {
    return("mixed_comparison")
  }
}

map(dat2, function(x){
  browser()
  x <- map(x, jsonlite::fromJSON)
  classes <- unlist(map(x, class))
  
  j <- 1
  out <- c()
  while(!any(out[!is.na(out)]) & j <= length(x[[1]])){
    if(length(unique((classes))) > 1){
      x[[classes == "integer"]] <- list(x[[classes == "integer"]])
    }
    
    
    
    out <- do.call(get_func(class(x[[1]][j]), class(x[[2]][j])), list(x = x[[1]][j], y = x[[2]][j]))
    j <- j + 1
  }
 
  return(out)

  # 
  # if(class(x[[1]]) == "integer" & class(x[[2]]) == "integer"){
  #   out <- integer_comparison(x[[1]], x[[2]])
  # } else if((class(x[[1]]) == "integer" & class(x[[2]]) != "integer") & class(x[[2]]) == "integer" & class(x[[1]]) != "integer"){
  #   intx <- which(c(class(x[[1]]), class(x[[2]])) == "integer")
  #   x[intx] <- list(x[[intx]])
  #   out <- mixed_comparison(x[[1]], x[[2]])
  # } else {
  #   browser()
  #   out <- list_comparison(x[[1]], x[[2]])
  # }
  # 
  # 
  # if(all(unlist(map(x, class)) %in% c("list", "matrix", "array"))){
  #   #both are lists
  # } else if(all(unlist(map(x, class)) == "integer")){
  #   #both are integers
  #   return(all(map2_lgl(x[[1]], x[[2]], ~.x <= .y)))
  # } else {
  #   browser()
  # 
  # }
})

#If both values are integers, the lower integer should come first. If the left integer is lower than the right integer, the inputs are in the right order. If the left integer is higher than the right integer, the inputs are not in the right order. Otherwise, the inputs are the same integer; continue checking the next part of the input.

#If both values are lists, compare the first value of each list, then the second value, and so on. If the left list runs out of items first, the inputs are in the right order. If the right list runs out of items first, the inputs are not in the right order. If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.


#If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison. For example, if comparing [0,0,0] and 2, convert the right value to [2] (a list containing 2); the result is then found by instead comparing [0,0,0] and [2].