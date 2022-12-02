#' Altered code from NHSBSA Data Analytics https://github.com/nhsbsa-data-analytics/nhsbsaR
#'
#' Function for addressbase premium data

calc_addressbase_premium_single_line_address <- function(
  df,
  include_postcode = FALSE
) {

  # Create the single line address
  df <- dplyr::mutate(df,
        single_line_address = paste0( 
        ifelse(department_name == "", "", paste0(department_name, ", ") ),
        ifelse(organisation_name == "", "", paste0(organisation_name, ", ") ), 
        ifelse(sub_building_name == "", "", paste0(sub_building_name, ", ") ),
        ifelse(building_name == "", "", paste0(building_name, ", ") ),
        ifelse(building_number == "", "", paste0(building_number, ", ") ),
        ifelse(po_box_number == "", "", paste0("PO BOX ", po_box_number, ", ") ),
        ifelse(dependent_thoroughfare == "", "", paste0(dependent_thoroughfare, ", ") ),
        ifelse(thoroughfare == "", "", paste0(thoroughfare, ", ") ),
        ifelse(double_dependent_locality == "", "", paste0(double_dependent_locality, ", ") ),
        ifelse(dependent_locality == "", "", paste0(dependent_locality, ", ") ),
        ifelse(post_town == "", "", paste0(post_town, ", ") )
        ))


  # Add the postcode if necessary
  if (include_postcode) {

    df <- dplyr::mutate(df,
        single_line_address = paste0(single_line_address, postcode)
      )

  }

  df

}
