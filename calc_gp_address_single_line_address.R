#' Altered code from NHSBSA Data Analytics https://github.com/nhsbsa-data-analytics/nhsbsaR
#'
#' Function for GP addess data

calc_gp_address_single_line_address <- function(
  df,
  include_postcode = FALSE
) {

  # Create the single line address
  df <- dplyr::mutate(df,
        single_line_address = paste0( 
        ifelse(nameofbuilding == "", "", paste0(nameofbuilding, ", ") ),
        ifelse(numberofbuilding == "", "", paste0(numberofbuilding, ", ") ),
        ifelse(nameofroad == "", "", paste0(nameofroad, ", ") ),
        ifelse(nameoflocality == "", "", paste0(nameoflocality, ", ") ),
        ifelse(nameoftown == "", "", paste0(nameoftown, ", ") )
        ))

  # Add the postcode if necessary
  if (include_postcode) {

    df <- dplyr::mutate(df,
        single_line_address = paste0(single_line_address, postcode)
      )

  }

  df

}
