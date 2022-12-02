library("tidyverse")

example <- c("A Y",
             "B X",
             "C Z")

input <- readLines("day02/input.txt")

scorer <- function(x){
  map(x, ~case_when(grepl("X|A", .x) ~ 1, 
                    grepl("Y|B", .x) ~ 2, 
                    grepl("Z|C", .x) ~ 3))
}

winner <- function(player1, player2){
  choices <- c("player1" = player1, "player2" = player2)
  
  if(player2 == player1) return("draw")
  if(all(choices %in% c(1, 3))) return(names(choices)[which(choices == 1)]) #rock 1 defeats scissors 3
  if(all(choices %in% c(3, 2))) return(names(choices)[which(choices == 3)]) #scissors 3 defeats paper 2
  if(all(choices %in% c(2, 1))) return(names(choices)[which(choices == 2)]) #paper 2 defeats rock 1 
}

play_rps <- function(dat){
  data.frame("play" = dat) %>% 
    separate(play, into = c("player1", "player2"), sep = " ") %>% 
    mutate(across(c(player1, player2), ~unlist(scorer(.x)))) %>% 
    mutate(roundwin = map2(player1, player2, winner), 
           winscore = case_when(roundwin == "player2" ~ 6, 
                                roundwin == "player1" ~ 0, 
                                roundwin == "draw" ~ 3), 
           totalscore = winscore + player2)
}

play_rps(example) %>% pull(totalscore) %>% sum()
play_rps(input) %>% pull(totalscore) %>% sum()


#part2 
play_rps_correctly <- function(dat){
  data.frame("play" = dat) %>% 
    rowid_to_column() %>% 
    separate(play, into = c("player1", "player2"), sep = " ") %>% 
    mutate(across(c(player1), ~unlist(scorer(.x)))) %>% 
    mutate(what_do = case_when(player2 == "X" ~ "player1", 
                               player2 == "Y" ~ "draw", 
                               player2 == "Z" ~ "player2"), 
           option1 = 1, 
           option2 = 2, 
           option3 = 3) %>% 
    pivot_longer(cols = matches("option")) %>% 
    mutate(roundwin = unlist(map2(player1, value, winner))) %>% 
    filter(roundwin == what_do) %>% 
    mutate(winscore = case_when(roundwin == "player2" ~ 6, 
                                roundwin == "player1" ~ 0, 
                                roundwin == "draw" ~ 3), 
           totalscore = winscore + value)
}

play_rps_correctly(example) %>% pull(totalscore) %>% sum()
play_rps_correctly(input) %>% pull(totalscore) %>% sum()

