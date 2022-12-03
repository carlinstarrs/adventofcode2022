library("tidyverse")

example <- c("vJrwpWtwJgWrhcsFMMfFFhFp",
             "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
             "PmmdzqPrVvPwwTWBwg",
             "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
             "ttgJtRGJQctTZtZT",
             "CrZsJsPPZsGzwwsLwLmpwMDw")


input <- readLines("day03/input.txt")

dat <- example

get_sack_priority <- function(dat){
  priorities <- setNames(1:52, c(letters, LETTERS))
  sacks <- map(dat, ~split(strsplit(.x, "")[[1]], rep(c(1,2), each = nchar(.x)/2)))
  priorities[map_chr(sacks, ~reduce(.x, intersect))] |> sum()
}

get_sack_priority(example)
get_sack_priority(input)

#part 2
get_sack_priority2 <- function(dat){
  priorities <- setNames(1:52, c(letters, LETTERS))
  starts <- seq(1, length(dat), by = 3)
  ends <- c(tail(starts, -1) - 1, length(dat))
  sacks <- map2(starts, ends, ~dat[.x:.y])
  priorities[map_chr(sacks, ~reduce(map(.x, ~strsplit(.x, "")[[1]]), intersect))] |> sum()
}

get_sack_priority2(example)
get_sack_priority2(input)
