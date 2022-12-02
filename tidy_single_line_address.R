#' Code based on NHSBSA Data Analytics https://github.com/nhsbsa-data-analytics/addressMatchR

# function to tidy single line address data
tidy_single_line_address <- function(df, col) {

df <- df %>%
  # Uppercase
  dplyr::mutate(single_line_address = toupper({{ col }})) %>%
  # replace special characters with a single space
  dplyr::mutate(single_line_address = gsub("[–=+,.();:#''?!%§$´`{}\\/\\*@[\\]\\-\\\\]", " ",{{ col }}, ignore.case = TRUE, perl = TRUE)) %>%
  # add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
  dplyr::mutate(single_line_address = gsub("([[:alpha:]])([[:digit:]])", "\\1 \\2",{{ col }}, ignore.case = TRUE, perl = TRUE)) %>%
  # add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
  dplyr::mutate(single_line_address = gsub("([[:digit:]])([[:alpha:]])", "\\1 \\2",{{ col }}, ignore.case = TRUE, perl = TRUE)) %>%
  # replace the ampersand character with the string "and"
  dplyr::mutate(single_line_address = gsub("&", " AND ",{{ col }}, ignore.case = TRUE, perl = TRUE)) %>%
  # replace any multiple spaces with a single space
  dplyr::mutate(single_line_address = gsub("( ){2,}", " ",{{ col }}, ignore.case = TRUE, perl = TRUE)) %>%
  # remove leading/trailing whitespace
  dplyr::mutate(single_line_address = trimws({{ col }}))

}
