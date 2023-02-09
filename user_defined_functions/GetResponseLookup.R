##
# Long-term condition lookup dataframe
##

get_response_lookup <- function() {
  data.frame(
    response_full_name = c(
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
      'Shielding Patients'
    ),
    response_short_name = c(
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
      'spl'
    )
  )
}
