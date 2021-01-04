# Advent Of Code 2020 - Day 4 ----
# Day 4: Passport Processing
# PART 1

# raw input ----
input <- readLines("day4.txt")

# clean input ----
joined_input <- paste(input, collapse = "\n")
cleaned_input <- strsplit(joined_input, split = "\n\n", fixed = TRUE)[[1]]
cleaned_input <- gsub("\n", " ", cleaned_input)

has_req_fields <- function(inputs) {
    # Function to check whether it has the required field
    # Inputs: cleaned_inputs (1 passport per line)
    # Return: Index of valid passport
    # List of required Passport Fields 
    # cid is optional, hence ignored here
    req_pp_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    
    # split to focus on only fields
    input_list <- strsplit(inputs, split = ":[a-zA-Z0-9#]+")
    
    # Trim the white spaces for each list items
    # Then, find out each list item, which has missing required fields using setdiff
    # Then, if the number of missing items is more than 0, will return TRUE, else FALSE
    processed_passport <- lapply(input_list, function(x) {
        cleaned <- trimws(x)
        length(setdiff(req_pp_fields, cleaned)) == 0
    })
    return (unlist(processed_passport))
}

# Number of valid passport ----
processed_results <- has_req_fields(cleaned_input)
sum(processed_results)

# PART 2 ----
is_between_years <- function(x, tf, year1, year2) {
    # Function to check whether it is between the requirements for year-related fields
    # Inputs: year field (vector), tf = targeted field, year1 = min, year2 = max
    # Return: TRUE/FALSE
    if (grepl(tf, x)) {
        extract_field <- as.numeric(substring(x, regexpr(":", x)+1))
        return (extract_field >= year1 && extract_field <= year2)
    } else {
        return (FALSE)
    }
}

has_val_height <- function(x) {
    # Function to check whether vector has valid height range
    # input: height field (vector)
    # return: TRUE/FALSE
    if (grepl("in|cm", x)) {
        # Extract string between ":" and "cm or inch", and convert to numeric
        hgt <- as.numeric(substring(x, regexpr(":", x) + 1, regexpr("cm|in", x)-1))
        if (grepl("cm", x)) {
            return (hgt >= 150 && hgt <= 193)
        } else {
            return (hgt >= 59 && hgt <= 76)
        }
    } else {
        return (FALSE)
    }
}

has_val_hcl <- function(x) {
    cl <- substring(x, regexpr(":", x) + 1)
    return (grepl("^#", cl) && (grepl("^[a-zA-Z0-9]{6}$", substring(cl, 2)))) 
}

has_val_ecl <- function(x) {
    el <- substring(x, regexpr(":", x) + 1)
    return (el %in% c("amb","blu","brn","gry","grn","hzl","oth"))
}

has_val_pid <- function(x) {
    pid <- substring(x, regexpr(":", x) + 1)
    return (grepl("^[0-9]{9}$", pid))
}


pp_w_req_fields <- cleaned_input[processed_results]
pp_w_req_fields <- strsplit(pp_w_req_fields, split = " ", fixed = TRUE)

# Number of valid passports ----
processed_passport <- sapply(pp_w_req_fields, function(x) {
    results <- vector()
    for (each_field in x) {
        processed_passport <- switch(EXPR = substr(each_field, 1, 3), 
               "byr" = is_between_years(each_field, "byr", 1920, 2002),
               "iyr" = is_between_years(each_field, "iyr", 2010, 2020),
               "eyr" = is_between_years(each_field, "eyr", 2020, 2030),
               "hgt" = has_val_height(each_field),
               "hcl" = has_val_hcl(each_field),
               "ecl" = has_val_ecl(each_field),
               "pid" = has_val_pid(each_field),
               "cid" = TRUE,
               FALSE
        )
        results <- c(results, processed_passport)
    }
    return (all(results))
})

sum(processed_passport)
































