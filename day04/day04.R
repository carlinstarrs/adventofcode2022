input <- readLines("day04/input.txt")


example <- c("2-4,6-8",
             "2-3,4-5",
             "5-7,7-9",
             "2-8,3-7",
             "6-6,4-6",
             "2-6,4-8")

get_contained_ranges <- function(dat){
  out <- map(dat, function(pairs){
    section_assignments <- map( str_replace_all(strsplit(pairs, ",")[[1]], "-", ":"), ~eval(parse(text = .x)))
    lengths <- map(section_assignments, ~length(.x))
    overlaps <- reduce(section_assignments, intersect)
    if(any(length(overlaps) %in% lengths)) return(overlaps) else return(NULL)
  })
  
  result <- out %>% compact() %>% length()
  return(result)
}

get_contained_ranges(example)

get_contained_ranges(input)


get_any_overlaps <- function(dat){
  out <- map(dat, function(pairs){
    section_assignments <- map(str_replace_all(strsplit(pairs, ",")[[1]], "-", ":"), ~eval(parse(text = .x)))
    lengths <- map(section_assignments, ~length(.x))
    overlaps <- reduce(section_assignments, intersect)
  })
  
  result <- out %>% compact() %>% length()
  return(result)
}

get_any_overlaps(example)
get_any_overlaps(input)