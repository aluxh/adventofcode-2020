# Advent Of Code 2020 - Day 7 ----
# Day 7: Handy Haversacks
library(stringr)
library(magrittr)

# PART 1 ----
input <- readLines("input/day7.txt")
cleaned_input <- gsub(pattern = " (bags|bag)", replacement = "", x = input)
cleaned_input <- gsub(pattern = ".$", replacement = "", x = cleaned_input)
cleaned_input <- strsplit(cleaned_input, split = " contain ")

# Search Bags
track_bags <- function(results = list(), bag_list, search_color) {
    
    index <- grep(lapply(bag_list, "[[", 2), pattern = search_color)
    if (length(index) == 0) return (results)
    
    found_bag <- bag_list[index]
    next_search <- paste(lapply(found_bag, "[[", 1), collapse = "|")
    
    return (track_bags(c(results, next_search), bag_list, next_search))
}

result <- unlist(track_bags(bag_list = cleaned_input, search_color = "shiny gold"))
color_bags <- unique(unlist(strsplit(result, split = "|", fixed = TRUE)))

# Number of color bags contain at least one shiny gold bag
length(color_bags)


# PART 2 ----
cleaned_input

# 1. Look into the list of bags and find the parent bag.
# 2. Retrieve how many children bags are in the parent bag. Sum the count and multiply by the number of parent bags.
# 3. Retrieve the colors of the children bags inside the parent bags. Then, for each color, return to step 1.

# Retrieve the bag names 
bags <- sapply(cleaned_input, "[[", 1)

# Assign the parent bags' names to each list
list_of_bags <- setNames(strsplit(sapply(cleaned_input, "[[", 2), split = ", ", fixed = TRUE), bags)

count_bag <- function(bag_list, search_color) {
    
    bag_count <- sapply(bag_list[[search_color]], function(x) {
        str_extract(x, "\\d+(?=\\s)") %>% 
            as.numeric()
    })
    
    next_search <- sapply(bag_list[[search_color]], function(x) {
        str_extract(x, "(?!\\d+\\s)(\\w+\\s\\w+)") 
    })
    
    sum(map2_dbl(bag_count, next_search, function(x, y) {
        x * (1 + count_bag(bag_list, y))
    }), na.rm = TRUE)
}

count_bag(list_of_bags, "shiny gold")





