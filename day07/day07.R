
library("tidyverse")
example <- c("$ cd /",
              "$ ls",
              "dir a",
              "14848514 b.txt",
              "8504156 c.dat",
              "dir d",
              "$ cd a",
              "$ ls",
              "dir e",
              "29116 f",
              "2557 g",
              '62596 h.lst',
              "$ cd e",
              "$ ls",
              "584 i",
              "$ cd ..",
              "$ cd ..",
              "$ cd d",
              "$ ls",
              "4060174 j",
              "8033020 d.log",
              "5626152 d.ext",
              "7214296 k")

input <- readLines("day07/input.txt")
dat <- example


dat <- input

instructions <- setNames(which(grepl("^\\$", dat)), dat[grepl("^\\$", dat)])

filetree <- tibble()
level <- 0

imap(instructions, function(id, instruction){
  if(grepl("cd \\/", instruction)){
    #outermost dir
    level <<- 1
    filetree <<- rbind(filetree, tibble("L1" = "/"))
    dirname <<- "/"
  } else if(grepl("cd \\.\\.", instruction)){
    #move out one level
    level <<- level - 1
  } else if(grepl("cd .+", instruction)){
    #move to directory named .
    dirname <<- paste0("dir ", str_extract(instruction, "(?<=\\$ cd ).+"))
    #nothing happens?
  } else if(grepl("ls", instruction)){
    #get filenames and sizes below until next instruction
    next_id <- replace_na(instructions[instructions > id][1], length(dat)+1)
    result <- dat[(id+1):(next_id-1)]
    level <<- level + 1
    if(ncol(filetree) < level) filetree <<- filetree %>% bind_cols(tibble(!!paste0("L", level) := list(NA)))
    filetree[,level][filetree[,level-1] == dirname] <<- list(result)
    filetree <<- filetree %>% unnest(level)
    filetree[,level] <<- map(filetree[,level], ~as.list(.x))
    
  }
})

filepaths <- filetree %>% 
  mutate(across(everything(), as.character)) %>%
  unite(col = "filepath", everything(), sep = "/", na.rm = TRUE) %>% 
  mutate(files = str_extract(filepath, "([^\\/]+$)"), 
         filepath = str_remove(filepath, files)) %>% 
  separate(col = files, into = c("size", "filename"), sep = " ") %>% 
  arrange(filepath)

totalsize <- filepaths %>%
  group_by(filepath) %>% 
  summarise(totalsize = sum(as.numeric(size)))

totalsize %>% 
  filter(totalsize < 100000) %>% 
  pull(totalsize) %>% 
  sum()
