# A function to read ONS deprivation scores (per measure) for all LSOAs, and 
#   compute the total IMD score. Specific IMD measures can be ignored in the
#   IMD calculation, and the weightings are re-scaled to account for this.
#
# Input: None
# Output: Dataframe listing LSOA code, IMD total score

get_deprivation <- function() {
  
  # Read IMD sheet
  deprivation <- read_excel(
    config$deprivation_data, 
    sheet = "IoD2019 Transformed Scores"
  ) %>%
    select(
      -`LSOA name (2011)`, 
      -`Local Authority District code (2019)`,
      -`Local Authority District name (2019)`,
      -`Health Score - exponentially transformed`
    ) %>%
    rename(lsoa = `LSOA code (2011)`)
  
  # Get IMD measure weights
  imd_weights <- get_imd_weights()
  
  # Calculate total IMD score per LSOA and return
  deprivation %>%
    gather(key = "imd_name", value = "imd_score", -lsoa) %>%
    inner_join(imd_weights, by = "imd_name") %>%
    group_by(lsoa) %>%
    summarise(imd_total = sum(imd_score * weight)) %>%
    ungroup()
}
