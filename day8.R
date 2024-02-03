# Advent Of Code 2020 - Day 8 ----
# Day 8: Handheld Halting
library(magrittr)

input <- readLines("input/day8.txt")

input_df <- do.call("rbind", strsplit(input, split = " ", fixed = TRUE)) %>% 
    as.data.frame()

names(input_df) <- c("ins", "move")

head(input_df)
