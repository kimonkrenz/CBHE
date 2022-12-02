# Reduce Input Tables
input_addresses <- function(
  df,
  postcode,
  address,
  id
) { 

# use only relevant columns

df <- df[,c(id,postcode,address)]

}
