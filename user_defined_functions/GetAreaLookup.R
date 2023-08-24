##
# Area lookup dataframe
##

get_area_lookup <- function() {
  data.frame(
    area_full_name = c(
      'GP Practice',
      'Ward',
      'Primary Care Network',
      'CCG',
      'Place Based'
    ),
    area_short_name = c(
      'gpp_name',
      'electoral_ward_or_division',
      'pcn',
      'ccg',
      'du'
    )
  )
}
