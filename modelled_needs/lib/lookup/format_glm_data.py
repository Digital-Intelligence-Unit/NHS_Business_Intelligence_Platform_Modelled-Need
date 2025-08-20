import pandas as pd
import numpy as np

config = {
    'max_age': 90,
    'min_age': 0,
    'age_band_width': 5,
}

def format_glm_data(patient_df, area_group, response_predict, age_factor):
    """
    Mutates the format of a patient dataframe into one used by a GLM.

    Args:
        patient_df (pd.DataFrame): Patient-level dataframe containing LTC booleans,
                                   predictor variables, and area variables.
        area_group (str): Area level by which to aggregate predictions.
        response_predict (list): LTC(s) to predict on.
        age_factor (str): Binary flag ('Y' or 'N') to convert age to five-year age bins.

    Returns:
        pd.DataFrame: Patient-level dataframe with mutated variables.
    """
    # Add flag for patients with no long-term conditions, change logical LTCs to binary,
    # combine CCG and locality, and filter out GP Practices with codes starting with 'Y'
    patient_df = patient_df.copy()
    patient_df = patient_df.applymap(lambda x: int(x) if isinstance(x, bool) else x)
    patient_df['area_var'] = patient_df[area_group]

    # Create "predict_var" to use in GLM, depending on response variable provided
    # If response variable = LTC(s), then change to binary
    # Combine LTCs if two are used
    def calculate_predict_var(row):
        return 1 if np.prod([row[col] for col in response_predict]) == 1 else 0

    patient_df['predict_var'] = patient_df.apply(calculate_predict_var, axis=1)
    patient_df = patient_df.drop(columns=response_predict + [area_group])

    # Remove response variable, calculate count of other LTCs per patient,
    # logarithmically re-scale IP/OP/AE appointments, and remove bad values
    patient_df = patient_df.dropna(subset=['predict_var', 'area_var'])

    # Add age banding if desired
    if age_factor == 'Y':
        patient_df['age'] = patient_df['age'].clip(
            lower=config['min_age'], upper=config['max_age']
        )
        patient_df['age'] = (config['age_band_width'] * 
                             np.ceil(patient_df['age'] / config['age_band_width'])).astype(int)

    # Return dataframe
    return patient_df