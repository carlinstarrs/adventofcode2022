
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

filepaths <- list()

imap(instructions, function(id, instruction){
  if(grepl("cd \\/", instruction)){
    curdir <<- "/"
  } else if(grepl("cd \\.\\.", instruction)){
    #move out one level
    curdir <<- str_remove(str_remove(curdir, "([^\\/]+$)"), "\\/$")
  } else if(grepl("cd .+", instruction)){
    #move to directory
    #curdir <<- paste0(str_remove(curdir, "\\/$"), "/dir ", str_extract(instruction, "(?<=\\$ cd dir).+"))
    curdir <<- file.path(curdir, str_extract(instruction, "(?<=\\$ cd ).+"))
  } else if(grepl("ls", instruction)){
    #get filenames and sizes below until next instruction
    next_id <- replace_na(instructions[instructions > id][1], length(dat)+1)
    result <- dat[(id+1):(next_id-1)]
    files <- result[!grepl("dir", result)]
    dirs <- result[grepl("dir", result)]
    filepaths[[curdir]] <<- c(files)
  }
})

bbs <- list()

imap(filepaths, function(files, dir){
  out <- filepaths[grepl(str_replace_all(paste0("^", str_replace(dir, "/$", "")), "\\/", "\\\\/"), names(filepaths))] %>% 
    unlist() %>% 
    str_split(" ") %>% 
    map(~as.numeric(.x[1])) %>% 
    reduce(sum)
  bbs <<- bind_rows(bbs, tibble("dir" = dir, 
                                "size" = out))
})

sum(bbs$size[bbs$size < 100000])

