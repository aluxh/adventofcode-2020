# Advent Of Code 2020 - Day 5 ----
# Day 5: Binary Boarding
# PART 1 ----

input <- readLines("input/day5.txt")

find_pos <- function(range = c(0, 127), alpha) {
    # Recursion function to find the exact position of the row or column ----
    # If the range is not the same number, it will divide the range into 2 depending
    # whether it is upper or lower limit. Then, keep redoing until the range is the same number
    # Then, return that number
    
    # input: 
    # 1. range: vector of 2 numbers
    # 2. alpha: indicates whether to take the upper or lower halves
    # return:
    # 1. if range contains same number, return one number
    # 2. if range doesn't contain same number, repeat the function
    # ----
    if (range[1] == range[2]) return (unique(range))

    x <- strsplit(alpha, split = "")[[1]]
    if (x[1] %in% c("F", "L")) {
        # if the alphabet is F or L, it will take upper limit
        # for max digit, it will always be odd number
        range <- c(range[1], floor((range[2] - range[1])/2) + range[1])
        # remove the first alphabet
        string <- paste0(x[2:length(x)], collapse = "")
        return (find_pos(range, string))
    } else { # else choose lower limit
        range <- c(ceiling((range[2] - range[1])/2) + range[1], range[2])
        string <- paste0(x[2:length(x)], collapse = "")
        return (find_pos(range, string))
    }
}

find_seat <- function(ticket) {
    # Making sure there are 10 characters
    if (nchar(ticket) != 10) { stop("There should be 10 characters in the ticket") }
    
    row <- substr(ticket, 1, 7)
    col <- substring(ticket, 8)
    seat <- vector()
    # Making sure the ticket contains the correct alphabets
    if (strsplit(row, "")[[1]] %in% c("B", "F") && strsplit(col, "")[[1]] %in% c("L", "R")) {
        seat <- c(seat, find_pos(range = c(0, 127), alpha = row))
        seat <- c(seat, find_pos(range = c(0, 7), alpha = col))
    }
    seat <- c(seat, seat[1] * 8 + seat[2])
    return (seat)
}

# Test ----
# BFFFBBFRRR: row 70, column 7, seat ID 567.
# FFFBBBFRRR: row 14, column 7, seat ID 119.
# BBFFBBFRLL: row 102, column 4, seat ID 820.
find_seat("BFFFBBFRRR")
find_seat("FFFBBBFRRR")
find_seat("BBFFBBFRLL")

# Apply function to the entire list ----
list_of_seats <- lapply(input, find_seat)
seats_df <- data.frame(matrix(unlist(list_of_seats), ncol = 3, byrow = TRUE))
names(seats_df) <- c("row", "column", "id")

# Highest Seat ID ----
with(seats_df, seats_df[id == max(id), ])

# PART 2 ----
list_of_ids <- sort(unlist(lapply(list_of_seats, '[[', 3)))
setdiff(80:919, list_of_ids)








