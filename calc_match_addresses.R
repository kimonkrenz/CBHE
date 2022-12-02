#' Altered code from NHSBSA Data Analytics https://github.com/nhsbsa-data-analytics/addressMatchR
#' Match two sets of addresses
#'
#' Match a distinct dataframe of primary addresses to a distinct dataframe of
#' lookup addresses.
#'
#' Returns a dataframe of exact matches (postcode and single line address) and
#' non exact matches (postcode and fuzzy single line address). For non exact
#' matches it will retain draws.
#'
#' @param primary_df Dataframe of distinct primary addresses
#' @param primary_postcode_col Column containing the primary postcode
#' @param primary_address_col Column containing the primary single line address
#' @param lookup_df Dataframe of distinct lookup addresses
#' @param lookup_postcode_col Column containing the lookup postcode
#' @param lookup_address_col Column containing the lookup single line address


calc_match_addresses <- function(
  primary_df,
  primary_postcode_col,
  primary_address_col,
  primary_id,
  lookup_df,
  lookup_postcode_col,
  lookup_address_col,
  lookup_id
) {

  # Rename the lookup postcode column name to be the same as the primary
  # postcode column name
  lookup_df <- lookup_df %>%
    dplyr::rename("{primary_postcode_col}" := .data[[lookup_postcode_col]])

  # First step is to do the exact matches. We mock join address column here so
  # that we can do the join easily.
  exact_match_df <-
    dplyr::inner_join(
      x = primary_df %>%
        # Mock a join address column on the primary dataframe
        dplyr::mutate(join_address = .data[[primary_address_col]]),
      y = lookup_df %>%
        # Mock a join address column on the lookup dataframe
        dplyr::mutate(join_address = .data[[lookup_address_col]]),
      by = c(primary_postcode_col, "join_address"),
      suffix = c("", "_lookup"),
      copy = TRUE
    ) %>%
    dplyr::select(-join_address)

  # Now get the rows that haven't already been matched
  non_exact_match_df <- primary_df %>%
    dplyr::anti_join(
      y = exact_match_df,
      na_matches = "na",
      copy = TRUE,
      by = primary_address_col
    )

  # Filter non exact matches to postcodes in the lookup
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::semi_join(
      y = lookup_df %>%
        dplyr::select(.data[[primary_postcode_col]]),
      copy = TRUE,
      by = primary_postcode_col
    )

  # Tokenise non exact match addresses
  non_exact_match_df <- non_exact_match_df %>%
    db_unnest_tokens(col = primary_address_col, drop = FALSE) %>%
    dplyr::mutate(token_weight = ifelse(grepl("[0-9]", token), 4, 1))

  # Add the theoretical max score for each non exact match address
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(across(-c(token_number, token, token_weight))) %>%
    dplyr::mutate(max_score = sum(token_weight, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Tokenise lookup addresses
  lookup_df <- lookup_df %>%
    db_unnest_tokens(col = lookup_address_col, drop = FALSE) %>%
    dplyr::select(-token_number) %>%
    dplyr::distinct() %>%
    dplyr::mutate(token_weight = ifelse(grepl("[0-9]", token), 4, 1))

  # We want to minimise the amount of Jaro–Winkler calculations we do. So first
  # do the exact token level matches
  non_exact_match_exact_match_df <- non_exact_match_df %>%
    dplyr::inner_join(
      y = lookup_df,
      by = c(primary_postcode_col, "token_weight", "token"),
      suffix = c("", "_lookup"),
      copy = TRUE
    ) %>%
    dplyr::mutate(token_lookup = token)

  # Now get the remaining candidates to consider for Jaro–Winkler matching
  # (character token types that aren't an exact match)
  non_exact_match_jw_match_df <- non_exact_match_df %>%
    dplyr::inner_join(
      y = lookup_df,
      by = c(primary_postcode_col, "token_weight"),
      suffix = c("", "_lookup"),
      copy = TRUE
    ) %>%
    dplyr::filter(
      token_weight == 1,
      token != token_lookup
    )

  # We can also apply some other filters
  non_exact_match_jw_match_df <- non_exact_match_jw_match_df %>%
    dplyr::filter(
      # Tokens share the same first letter
      substr(token_lookup, 1, 1) == substr(token, 1, 1) |
        # Tokens share same second letter
        substr(token_lookup, 2, 2) == substr(token, 2, 2) |
        # Tokens share same last letter
        substr(token_lookup, nchar(token_lookup), nchar(token_lookup)) == substr(token, nchar(token_lookup), nchar(token_lookup)) |
        # One token is a substring of the other
        stringr::str_detect(token_lookup, token) |
        stringr::str_detect(token, token_lookup)
    )

  # Now calculate the jarrow winkler scores
  non_exact_match_jw_match_df <- non_exact_match_jw_match_df %>%
    dplyr::mutate(score = stringdist::stringsim(token, token_lookup, method = "jw", p = 0))

  # And filter to scores above 0.8
  non_exact_match_jw_match_df <- non_exact_match_jw_match_df %>%
    dplyr::filter(score > 0.8)

  # Now stack the non exact exact and Jaro–Winkler matches back together
  non_exact_match_df <- dplyr::union_all(
    x = non_exact_match_exact_match_df %>%
      dplyr::mutate(score = 1),
    y = non_exact_match_jw_match_df
  )

  # Multiply the score by the token weight
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::mutate(score = score * token_weight)

  # Get the max score for each primary token in the primary address from each
  # lookup address
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(dplyr::across(-c(token_lookup, score))) %>%
    dplyr::summarise(score = max(score, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Sum the score for each single line address combination
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(
      dplyr::across(-c(token_number, token, token_weight, score))
    ) %>%
    dplyr::summarise(score = sum(score, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Normalise the score
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::mutate(score = score / max_score) %>%
    dplyr::select(-max_score)

  # Take the top scoring lookup address for each primary address (if there are
  # draws then keep all of them)
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(
      .data[[primary_postcode_col]], .data[[primary_address_col]]
    ) %>%
    dplyr::slice_max(order_by = score) %>%
    dplyr::ungroup()

  # Stack the exact and non exact matches together and output
  dplyr::union_all(
    x = exact_match_df %>%
      dplyr::mutate(score = 1, match_type = "exact"),
    y = non_exact_match_df %>%
      dplyr::mutate(match_type = "non-exact")
  )

}
