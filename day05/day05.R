library("tidyverse")

example <- c("[ ] [D] [ ]",   
             "[N] [C] [ ]",   
             "[Z] [M] [P]", 
             "1   2   3", 
             "", 
             "move 1 from 2 to 1",
             "move 3 from 1 to 3",
             "move 2 from 2 to 1",
             "move 1 from 1 to 2")

dat <- example

input <- readLines("day05/input.txt")

move_crates <- function(dat, crate_mover = 9000){
  stack_ids <- strsplit(dat[grepl("^( |)\\d", dat)], " ")[[1]] %>% .[.!= ""]
  crates <- NULL
  crates <- dat[!grepl("^move|^( |)\\d", dat) & dat != ""] %>% 
    str_replace_all("    ", "[ ]") %>% 
    str_remove_all("\\[| ") %>% 
    as_tibble() %>%
    separate(col = "value", sep = "\\]", into = paste0("X", stack_ids)) %>% 
    as.list() %>% 
    map(~.x[.x!=""])
  
  moves <- matrix(unlist(str_extract_all(dat[grepl("^move ", dat)], "\\d+")), ncol = 3, byrow = TRUE) %>% 
    as_tibble() %>% 
    set_names(c("move", "from", "to")) %>% 
    mutate_all(as.numeric)

  pwalk(moves, function(move, from, to){ 
    move_crates <- crates[[from]][1:move]
    if(crate_mover == 9000) move_crates <- rev(move_crates)
    
    crates[[to]] <<- c(move_crates, crates[[to]])
    crates[[from]] <<- crates[[from]][-(1:move)]
  })
  
  paste(map_chr(crates, ~.x[1]), collapse = "")
}

move_crates(example)
move_crates(input)
move_crates(example, crate_mover = 9001)
move_crates(input, crate_mover = 9001)
