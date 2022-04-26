format_search_string <- function(
  string_array,
  suppress = NULL,
  phase = c('4', 'not_applicable'),
  date = '1900-01-01'
) {
  if (!is.null(suppress)) {
    string_array <- string_array %>%
      str_remove(suppress) %>%
      na_if('') %>%
      na.omit()
  }

  string <- paste(
    paste0('(', string_array, ')'),
    collapse = ' && '
  )

  phase_lookup <- data.frame(
    input_phase = c('1', '2', '3', '4', 'not_applicable'),
    output_phase = c(
      '1 || "Phase 1" || "Phase I"',
      '2 || "Phase 2" || "Phase II"',
      '3 || "Phase 3" || "Phase III"',
      '4 || "Phase 4" || "Phase IV"',
      '"NA" || "Not Applicable"'
    )
  )

  study_phase <- phase_lookup %>%
    filter(input_phase %in% unlist(phase)) %>%
    select(output_phase) %>%
    summarise_all(paste, collapse = " || ") %>%
    as.character()

  string <- paste0(
    string,
    paste(
      ' && (has_published_results:true)',
      '&& (status:complete)',
      '&& (_exists_:documents.source_url)',
      paste0('&& (study_phase:', study_phase, ')'),
      paste0('&& (registration_date:{', date, ' TO ', today(), '})')
    )
  )

  string <- str_replace_all(string, 'hyperthyroid', 'hypothyroid')

  return(string)
}
