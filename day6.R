# Advent Of Code 2020 - Day 6 ----
# Day 6: Custom Customs
# PART 1 ----
input <- readLines("day6.txt")

# Cleaning up the text
joined_input <- paste(input, collapse = "\n")
cleaned_input <- strsplit(joined_input, split = "\n\n", fixed = TRUE)[[1]]
list_of_qsn <- strsplit(cleaned_input, split = "\n", fixed = TRUE)

list_of_unique_qsn <- lapply(list_of_qsn, function(x) {
    
    combine_list_of_qsn <- paste0(x, collapse = "")
    vec_qsn <- strsplit(combine_list_of_qsn, split = "")[[1]]
    
    return (unique(vec_qsn))
})

# Number of unique qsn answered 
sum(lengths(list_of_unique_qsn))

# PART 2 ----
find_intersect <- function(tmp_list = vector(), qsn_list) {
    x <- strsplit(qsn_list, split = "")
    # For Groups of 1 person, who answered all the qsn, it will be calculated.
    # IF there is no temporarily stored intersect results (testing is.logical)
    # AND the qsn_list has only 1 set of answers
    # RETURN the answer as is.
    if (is.logical(tmp_list) && length(qsn_list) == 1) return (x[[1]])
    
    # Exit when only left 1 in the list, and 
    # Exit by comparing it with the temporarily saved result
    if (length(x) == 1) return (intersect(tmp_list, x[[1]]))
    
    # First time, the temporarily saved result has nothing,
    # it will compare itself with next item
    # Then, call itself, and remove one item from the qsn list.
    if (is.logical(tmp_list)) {
        tmp_list <- intersect(x[[1]], x[[2]])
        return (find_intersect(tmp_list, qsn_list[-c(2)]))
    } else {
        tmp_list <- intersect(tmp_list, intersect(x[[1]], x[[2]]))
        return (find_intersect(tmp_list, qsn_list[-c(2)]))
    }
}

results <- lapply(list_of_qsn, function(x) {
    return (find_intersect(qsn_list = x))
})
# x <- lengths(results)
sum(lengths(results))




find_intersect_v1 <- function(qsn_list) {
    x <- strsplit(qsn_list, split = "")
    
    tmp <- vector()
    for (i in 1:length(x)) {
        if (is.logical(tmp)) {
            tmp <- intersect(x[[1]], x[[i]])
        } else {
            tmp <- intersect(tmp, intersect(x[[1]], x[[i]]))
        }
    }
    return (tmp)
}

# Test----
test_results <- list()
for (each_line in list_of_qsn) {
    temp_list <- find_intersect_v1(each_line)
    temp_list <- paste(temp_list, collapse = "")
    test_results <- c(test_results, temp_list)
    # print(results)
    
}
print(test_results)
sum(unlist(lapply(test_results, nchar)))
y <- unlist(lapply(test_results, nchar))


#  [1] "m" "z" "o" "a" "t" "s" "c" "r" "f" "e" "k"
#  [2] ""
#  [3] "f"
#  [4] ""
#  [5] "w" "g" "z" "j" "x" "e" "c" "t" "s" "f" "b" "u" "q" "k" "y" "i" "h" "p" "v" "r" "m" "l"
#  [6] "k" "i" "b" "x" "g" "z" "p" "q" "l" "n" "y"

x <- strsplit(list_of_qsn[[49]], split = "")

tmp <- vector()
for (i in 2:length(x)) {
    if (is.logical(tmp)) {
        tmp <- intersect(x[[1]], x[[i]])
        print(tmp)
    } else {
        print(tmp)
        tmp <- intersect(tmp, x[[i]])
    }
}
print(tmp)





