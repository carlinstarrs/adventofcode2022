library("tidyverse")

example <- c("1000",
             "2000",
             "3000",
             "",
             "4000",
             "",
             "5000",
             "6000",
             "",
             "7000",
             "8000",
             "9000",
             "",
             "10000")

calorie_counter <- function(dat){
  data.frame("n" = dat) %>% 
    mutate(is_blank = n == "", 
           grp = cumsum(is_blank) + 1) %>% 
    filter(n != "") %>% 
    group_by(grp) %>% 
    summarize(calories = sum(as.numeric(n))) 
}

calorie_counter(example)

input <- readLines("day01/input.txt")

calorie_counter(input) %>% slice_max(calories)

calorie_counter(input) %>% slice_max(calories, n = 3) %>% pull(calories) %>% sum()


#can do in one line this way: 
a <- read_file("day01/input.txt")
sum(sort(map_dbl(strsplit(a, "\n\n")[[1]], ~sum(as.numeric(strsplit(.x, "\n")[[1]]))), decreasing = TRUE)[1:3])


