##
# GLM predictor lookup dataframe
##

get_predictor_lookup <- function() {
  data.frame(
    predictor_full_name = c(
      'Age',
      'Age Band',
      'Sex',
      'Deprivation Decile',
      'Asthma',
      'Coronary Heart Disease',
      'Congestive Heart Failure',
      'Cancer',
      'COPD',
      'Depression',
      'Diabetes',
      'Hypertension',
      'Atrial Fibrillation',
      'Chronic Kidney Disease',
      'Dementia',
      'Epilepsy',
      'Hypothyroid',
      'Mental Health',
      'Learning Disability',
      'Osteoporosis',
      'Peripheral Artery Disease',
      'Rheumatoid Arthritis',
      'Stroke/TIA',
      'Mosaic Type'
    ),
    predictor_short_name = c(
      'age',
      'age_band_narrow',
      'sex',
      'imd_decile',
      'asthma',
      'chd',
      'heart_failure',
      'cancer',
      'copd',
      'depression',
      'diabetes',
      'hypertension',
      'atrial_fibrillation',
      'ckd',
      'dementia',
      'epilepsy',
      'hypothyroid',
      'mental_health',
      'learning_disabilities',
      'osteoporosis',
      'pad',
      'rheumatoid_arthritis',
      'stroke_tia',
      'mosaic_label'
    )
  )
}
