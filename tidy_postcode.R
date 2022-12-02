#' Altered code from NHSBSA Data Analytics https://github.com/nhsbsa-data-analytics/nhsbsaR
#'
#' Function to tidy postcode data

tidy_postcode <- function(df, col) {

  # Tide the postcode column
df <- df %>%
  # Uppercase
  dplyr::mutate(postcode = toupper({{ col }})) %>%
  # Remove anything not a character or digit
  dplyr::mutate(postcode = gsub("[^A-Z0-9]", "",{{ col }}, ignore.case = TRUE, perl = TRUE))

}
