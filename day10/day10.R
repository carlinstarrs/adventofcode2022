library("tidyverse")

example <- c("noop",
             "addx 3",
             "addx -5")

input <- readLines("day10/input.txt")

example2 <- readLines("day10/example.txt")

dat <- example
dat <- example2
dat <- input


interesting_cycles <- seq(20, 1000, by = 40)

out <- imap_dfr(c("start 1", dat), function(x, i){
    if(grepl("addx", x)){
      ss <- strsplit(x, " ")[[1]]
      tibble("inst" = rep(ss[1], 2), 
             "val" = c(0,  as.numeric(ss[2])))
    } else if(grepl("start", x)){
      return(tibble("inst" = "start", 
                    "val" = 1))
    } else {
      return(tibble("inst" = "noop", 
                    "val" = 0))
    }
}) %>% 
  mutate(cycle = 1:(nrow(.)), 
         x = cumsum(val), 
         signal_strength = cycle * x, 
         interesting = cycle %in% interesting_cycles) 

out %>% 
  filter(interesting == TRUE) %>% 
  pull(signal_strength) %>% 
  sum()

crt <- out %>% 
  mutate(pos = rep_len(0:39, length.out = nrow(out)),
         crt_row = between(pos, x-1, x+1),
         crt_row = case_when(crt_row == TRUE ~ "#",
                             TRUE ~ ".")) 

data.frame((matrix(head(crt$crt_row, -1), ncol = 40, byrow = TRUE))) %>% apply(1, paste, collapse = "")

