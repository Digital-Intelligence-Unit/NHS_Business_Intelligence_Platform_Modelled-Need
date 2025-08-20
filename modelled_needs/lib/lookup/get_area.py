import pandas as pd

##
# Area lookup dataframe
##
def get_area_lookup():
    return pd.DataFrame({
        'area_full_name': [
            'GP Practice',
            'Ward',
            'Primary Care Network',
            'CCG',
            'ICP'
        ],
        'area_short_name': [
            'gpp_name',
            'electoral_ward_or_division',
            'pcn',
            'ccg',
            'icp'
        ]
    })
