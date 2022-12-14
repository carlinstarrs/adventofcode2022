library("tidyverse")
library("igraph")

input <- "day12/input.txt"
example <- "day12/example.txt"

dat <- example
dat <- input

dict <- setNames(c(1, 1:length(letters), 26), c("S", letters, "E"))

tick <- Sys.time()
gridvals <- readLines(dat) %>% 
  map_dfr(~setNames(strsplit(.x, "")[[1]], paste0("X", 1:nchar(.x)))) %>% 
  rowid_to_column("x") %>% 
  pivot_longer(-x, 
               names_to = "y",
               names_pattern = "(\\d+$)") %>% 
  mutate(y = as.numeric(y), 
         elev = dict[value]) %>% 
  rowid_to_column("id") 

adjvals <- gridvals %>% 
  group_by(x) %>% 
  mutate(lx = x, 
         ly = ifelse(y-1 >= min(y), y-1, NA), 
         rx = x, 
         ry = ifelse(y+1 <= max(y), y+1, NA)) %>% 
  group_by(y) %>% 
  mutate(ux = ifelse(x-1 >= min(x), x-1, NA), 
         uy = y, 
         dx = ifelse(x+1 <= max(x), x+1, NA), 
         dy = y) %>%
  ungroup() %>% 
  select(id, matches(".x$|.y$")) %>% 
  pivot_longer(-id) %>% 
  mutate(dir = str_extract(name, "^."), 
         coord = str_extract(name, ".$")) %>% 
  group_by(id, dir) %>% 
  filter(!any(is.na(value))) %>% 
  select(-name) %>% 
  pivot_wider(names_from = coord, 
              values_from = value) %>% 
  left_join(gridvals %>% select(x, y, value, elev, to_id = id), by = c("x", "y")) %>% 
  rename(from_id = id) %>% 
  rename_with(~paste0("to_", .x), .cols = -c(from_id, to_id))

valid_moves <- gridvals %>% 
  rename_with(~paste0("from_", .x)) %>% 
  left_join(adjvals, by = c("from_id")) %>% 
  mutate(elev_diff = to_elev - from_elev) %>% 
  filter(elev_diff <= 1)


#part 1
ggraph <- graph_from_edgelist(valid_moves %>% 
                      select(from_id, to_id) %>% 
                      as.matrix()) 

g2 <- ggraph %>% 
  shortest_paths(from = gridvals$id[gridvals$value == "S"], to = gridvals$id[gridvals$value == "E"]) %>% 
  .$vpath 

map_chr(unlist(g2), ~gridvals$value[gridvals$id == .x])

length(g2[[1]]) - 1

#part2 
out <- map_dbl(gridvals$id[gridvals$value == "a"], function(start){
  ggraph %>% 
    shortest_paths(from = start, to = gridvals$id[gridvals$value == "E"]) %>% 
    .$vpath %>%
    unlist() %>% 
    length()
}) 

min(out[out > 0]) - 1


tock <- Sys.time()

tock - tick

