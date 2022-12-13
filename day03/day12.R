library("tidyverse")
library("igraph")

input <- "day12/input.txt"
example <- "day12/example.txt"

dat <- example
dat <- input


mm <- readLines(dat) %>% 
  map_dfr(~setNames(strsplit(.x, "")[[1]], paste0("X", 1:nchar(.x)))) %>% 
  as.matrix() 

dict <- setNames(c(1, 1:length(letters), 26), c("S", letters, "E"))

ii <- matrix(1:length(mm), ncol = ncol(mm))

weights <- imap_dfr(mm, function(val, id){
  # if(id == 3355) browser()
  x <- col(mm)[id]
  y <- row(mm)[id]

  adjs <- list(c(y, x+1),  
               c(y, x-1), 
               c(y+1, x), 
               c(y-1, x))
  
  adjs <- discard(adjs, ~.x[1] > max(row(mm)) | .x[1] < min(row(mm)))
  adjs <- discard(adjs, ~.x[2] > max(col(mm)) | .x[2] < min(col(mm)))
  
  paths <- tibble(from = id, 
                  to =  ii[matrix(unlist(adjs), ncol = 2, byrow = TRUE)], 
                  from_val = val, 
                  to_val = mm[matrix(unlist(adjs), ncol = 2, byrow = TRUE)]) %>% 
    mutate(elev = dict[to_val] - dict[from_val])
  paths[paths$elev <= 1,]
}) 

aa <- graph_from_data_frame(weights %>% select(from, to))


g2 <- shortest_paths(graph_from_data_frame(weights %>% select(from, to)), from = which(mm == "S"), to = which(mm == "E"))

mm[g2$vpath[[1]]]

length(mm[g2$vpath[[1]]])-1

test <- anti_join(weights, edgelist, by= c("from" = "Var1", "to" = "Var2"))

