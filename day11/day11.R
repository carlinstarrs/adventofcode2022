library("tidyverse")

#monkeys operate based on how worried you are (starting item) about each item

# 
# Monkey 0:
#   Starting items: 79, 98 (worry)
# Operation: new = old * 19 (how worry level changes post-inspection)
# Test: divisible by 23 (test to see what mokey will do)
# If true: throw to monkey 2
# If false: throw to monkey 3

example <- read_file("day11/example.txt")

#After each monkey inspects an item but before it tests your worry level, your relief that the monkey's inspection didn't damage the item causes your worry level to be divided by three

#Count the total number of times each monkey inspects items over 20 rounds

input <- read_file("day11/input.txt")

parse_monkey_input <- function(dat){
  dat <- strsplit(dat, "\r\n\r\n|\n\n")[[1]]
  
  out <- map(dat, function(x){ 
    ss <- str_remove_all(str_split(x, "\r\n|\n")[[1]], "^ +|^_+|\\\n")
    ss <- ss[ss!=""]
    fields <- str_replace_all(unlist(str_extract_all(ss, ".+(?=:)")), " ", "_")
    fields[1] <- str_remove(fields[1], "_\\d+")
    values <- str_extract_all(ss, "(?<=: ).+$")
    values[1] <- str_extract(ss[1], "\\d+")
    
    out <- setNames(as.list(values), fields) %>% 
      as_tibble() #%>% 
      #mutate(Monkey = as.numeric(Monkey) + 1)
 
    #print(out$Monkey)
    rules <- out %>% 
      select(-Starting_items) %>% 
      mutate(across(matches("^If_|^Test"), ~str_extract_all(.x, "\\d+")[[1]])) %>% 
      mutate(Operation = str_remove(Operation, "new = "), 
             Monkey = as.integer(Monkey))
    
    monkey_starts <- out %>% 
      separate_rows(Starting_items, sep = ", ") %>% 
      select(Monkey, Starting_items)
    
    return(list("rules" = rules, 
                "monkey_starts" = monkey_starts))
  })
  
  return(out)
}

monkey_business <- function(dat, rounds = 20, worry_reducer = 3){
  monkeydat <- parse_monkey_input(dat)
  monkey_rules <- map_dfr(monkeydat, ~.[["rules"]]) 

  if(worry_reducer == 1){
    #this would make you too worried
    test_mod <- prod(as.numeric(monkey_rules$Test)) #this will make it better
  } 
  
  monkey_q <- setNames(map(monkeydat, ~as.numeric(unlist(.x[["monkey_starts"]][,2]))), unique(monkey_rules$Monkey))
  monkeyed_items <- setNames(as.list(rep(0, length(unique(monkey_rules$Monkey)))), unique(monkey_rules$Monkey))
  
  take_turn <- function(item, monkey){
    rule <- monkey_rules[as.numeric(monkey)+1,]
    Worry_level <- floor(eval(parse(text = str_replace_all(rule$Operation, "old", as.character(item))))/worry_reducer)
    if(worry_reducer == 1){
      Worry_level <- Worry_level %% test_mod
    }
    if(Worry_level %% as.numeric(rule$Test) == 0){
      monkey_q[[rule$If_true]] <<- c(monkey_q[[rule$If_true]], Worry_level)
    } else {
      monkey_q[[rule$If_false]] <<- c(monkey_q[[rule$If_false]], Worry_level)
    }
    monkeyed_items[[monkey]] <<- monkeyed_items[[monkey]] + 1
    monkey_q[[monkey]] <<- monkey_q[[monkey]][-1]
  }
  
  monkeys <- as.character(rep(monkey_rules$Monkey, rounds))
  
  walk(monkeys, function(monkey){
    items <- monkey_q[[monkey]]
    walk(items, ~take_turn(.x, monkey))
  })

  prod(sort(unlist(monkeyed_items), decreasing = TRUE)[1:2])
}

monkey_business(example)
monkey_business(input)

monkey_business(example, rounds = 10000, worry_reducer = 1)
monkey_business(input, rounds = 10000, worry_reducer = 1)
