import numpy as np
from scipy.stats import chi2

def compare_model_output(real_df, predict_vector, n_predict, model_type, xg_pred=None):
    """
    Compares output LTC probabilities with observed LTC occurrences, calculating chi-square statistic,
    95% confidence interval, and significance.

    Args:
        real_df (pd.DataFrame): Patient-level dataframe containing LTC boolean.
        predict_vector (array-like): Predicted probabilities (logistic model) or model object (count model).
        n_predict (int): Number of predictor variables.
        model_type (str): Model used to predict response ('logistic' or 'count').
        xg_pred (list, optional): Predictor variables for count model.

    Returns:
        pd.DataFrame: Dataframe containing expected/observed occurrences, ratio, confidence interval,
                      chi-square statistic, and significance.
    """
    if model_type == 'logistic':
        # Combine patient data with LTC probability and re-format predict_var from binary factor (1, 2) to simple binary (0, 1)
        # real_df = real_df.copy()
        real_df['predicted'] = predict_vector
        real_df['actual'] = real_df['predict_var'] #.apply(lambda x: 1 if x == 'Y' else 0)
    else:
        # Combine patient data with inpatient, outpatient, and A&E predictions and calculate total predicted hospital attendance per patient
        xg_train = real_df[xg_pred].apply(lambda col: col.cat.codes if col.dtype.name == 'category' else col).to_numpy()
        real_df = real_df.copy()
        real_df['predicted'] = predict_vector.predict(xg_train)
        real_df = real_df.rename(columns={'predict_var': 'actual'})

    # Group by area, filter out low-count areas, and calculate expected occurrence, observed occurrence, chi-square, and p-value
    grouped = (
        real_df
            .groupby('area_var')
            .filter(lambda x: len(x) > 10)
            .groupby('area_var')
            .agg(
                expected=('predicted', lambda x: np.sum(x)),
                observed=('actual', lambda x: np.sum(x)),
            )
            .reset_index()
    )
    grouped['match_ratio'] = grouped['observed'] / grouped['expected']
    grouped['chi_square'] = (grouped['observed'] - grouped['expected']) ** 2 / grouped['expected']
    # grouped['p_value'] = grouped.apply(
    #     lambda row: chi2.sf(row['chi_square'], df=(len(real_df) - 1) * (n_predict - 1)), axis=1
    # )

    # Calculate confidence interval, significance, and add a significant Y/N flag
    grouped['match_lower'] = chi2.ppf(0.05, 2 * grouped['observed']) / (2 * grouped['expected'])
    grouped['match_higher'] = chi2.ppf(0.95, 2 * grouped['observed'] + 2) / (2 * grouped['expected'])
    grouped['expected_lower'] = grouped['observed'] / grouped['match_lower']
    grouped['expected_higher'] = grouped['observed'] / grouped['match_higher']
    grouped['significant'] = np.where(
        (np.sign((grouped['match_higher'] - 1) / (grouped['match_ratio'] - 1)) == 1) &
        (np.sign((grouped['match_lower'] - 1) / (grouped['match_ratio'] - 1)) == 1),
        'Yes',
        'No'
    )
    grouped['significance'] = np.where(
        grouped['significant'] == 'No',
        0,
        np.minimum(
            np.abs(grouped['match_higher'] - 1),
            np.abs(grouped['match_lower'] - 1)
        )
    )

    # Remove nan
    grouped = grouped.replace({np.nan: None})

    return grouped