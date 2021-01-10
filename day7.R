# Advent Of Code 2020 - Day 7 ----
# Day 7: Handy Haversacks
# PART 1 ----

input <- readLines("input/day7.txt")

cleaned_input <- gsub(pattern = " bags", replacement = "", x = input)
cleaned_input <- strsplit(cleaned_input, split = " contain ")


# Search
track_bags <- function(results = list(), bag_list, search_color) {
    
    index <- grep(lapply(bag_list, "[[", 2), pattern = search_color)
    
    if (length(index) == 0) return (results)
    
    found_bag <- bag_list[index]
    next_search <- paste(lapply(found_bag, "[[", 1), collapse = "|")
    
    return (track_bags(c(results, next_search), bag_list, next_search))
}

first <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = "shiny gold")]
first_layer <- paste(lapply(first, "[[", 1), collapse = "|")


second <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = first_layer)]
second_layer <- paste(lapply(second, "[[", 1), collapse = "|")


third <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = second_layer)]
third_layer <- paste(lapply(third, "[[", 1), collapse = "|")


fourth <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = third_layer)]
fourth_layer <- paste(lapply(fourth, "[[", 1), collapse = "|")


fifth <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = fourth_layer)]
fifth_layer <- paste(lapply(fifth, "[[", 1), collapse = "|")


sixth <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = fifth_layer)]
sixth_layer <- paste(lapply(sixth, "[[", 1), collapse = "|")


seventh <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = sixth_layer)]
seventh_layer <- paste(lapply(seventh, "[[", 1), collapse = "|")


eighth <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = seventh_layer)]
eighth_layer <- paste(lapply(eighth, "[[", 1), collapse = "|")


ninth <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = eighth_layer)]
ninth_layer <- paste(lapply(ninth, "[[", 1), collapse = "|")


tenth <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = ninth_layer)]
tenth_layer <- paste(lapply(tenth, "[[", 1), collapse = "|")


eleventh <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = tenth_layer)]
eleventh_layer <- paste(lapply(eleventh, "[[", 1), collapse = "|")


twelveth <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = eleventh_layer)]
twelveth_layer <- paste(lapply(twelveth, "[[", 1), collapse = "|")


thirteenth <- cleaned_input[grep(lapply(cleaned_input, "[[", 2), pattern = twelveth_layer)]




bags <- c(first_layer, second_layer, third_layer, fourth_layer, fifth_layer, sixth_layer, seventh_layer, eighth_layer, ninth_layer,
          tenth_layer, eleventh_layer, twelveth_layer)
bags
result <- unique(unlist(strsplit(first, split = "|", fixed = T)))
length(result)














