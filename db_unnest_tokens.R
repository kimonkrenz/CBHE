#' Altered code from NHSBSA Data Analytics https://github.com/nhsbsa-data-analytics/nhsbsaR

# Utilising tidy's unnest_tokens
db_unnest_tokens <- function(df, col, drop = TRUE, regex = "[:space:]") {

  df <- tidytext::unnest_tokens(df, "token", col, token = "regex", pattern = regex, format = "text", to_lower = FALSE, drop = TRUE, collapse = NULL) %>% dplyr::mutate(token_number = row.names(.))
  
}
