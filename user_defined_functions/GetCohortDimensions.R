get_cohort_dimensions <- function(){
  data.frame(
    dim_full_name = c(
      'LDimension',
      'GPDimension',
      'TDimension',
      'LTCs2Dimension',
      'SexDimension',
      'MDimension',
      'CCGDimension',
      'LCntDimension',
      'AgeDimension',
      'RskDimension',
      'DDimension',
      'WDimension',
      'numberSelLtc',
      'UDimension'
    ),
    dim_short_name = c(
      'locality',
      'gpp_code',
      'taxonomy',
      'long_term_conditions',
      'sex',
      'mosaic_label',
      'ccg_name',
      'other_ltc',
      'age',
      'risk_score',
      'deprivation_decile',
      'ward',
      'n_selected_ltc',
      'cost_group'
    )
  )
}
