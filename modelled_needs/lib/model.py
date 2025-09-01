import os
import pandas as pd
import sqlalchemy
from statsmodels.formula.api import glm
from statsmodels.api import families

from modelled_needs.lib.lookup.get_response import get_response_lookup
from modelled_needs.lib.lookup.get_area import get_area_lookup
from modelled_needs.lib.lookup.get_predictor import get_predictor_lookup
from modelled_needs.lib.lookup.format_glm_data import format_glm_data
from modelled_needs.lib.lookup.compare_model_output import compare_model_output

def get_postgres_engine():
    return sqlalchemy.create_engine(
        sqlalchemy.URL.create(
            "postgresql",
            username=os.environ.get("POSTGRES_USERNAME", "postgres"),
            password=os.environ.get("POSTGRES_PASSWORD", ""),
            host=os.environ.get("POSTGRES_HOST", "localhost"),
            database=os.environ.get("POSTGRES_DATABASE", "postgres"),
            port=os.environ.get("POSTGRES_PORT", "5432")
        )
    )

def get_patient_records(query, params):
    with get_postgres_engine().begin() as sqlConnection:
        patientRecordQuery = sqlConnection.execute(
            query, params
        )
        return pd.DataFrame(patientRecordQuery.fetchall(), columns=patientRecordQuery.keys())

# Main function to get the model
def get_model(
    response_filter_1='', response_filter_2='',
    predictors=[],
    area_level='', 
    filter_areas=[], 
    train_areas=[], 
    
    sampling='none', 
    age_as_a_factor='N',
):
    if not response_filter_1 and not response_filter_2:
        print('Please select a long-term condition, cohort, or count/cost variable to predict')
        return

    # Import lookup tables
    response_lookup = get_response_lookup()
    area_lookup = get_area_lookup()
    predictor_lookup = get_predictor_lookup()

    # Filter area/long-term condition/predictors to selected values
    area_level = area_lookup.loc[area_lookup['area_full_name'] == area_level, 'area_short_name'].values[0]

    # Convert response names
    keep_response = response_lookup.loc[
        response_lookup['response_full_name'].isin([response_filter_1, response_filter_2]),
        'response_short_name'
    ].tolist()

    if not keep_response:
        keep_response = [response_filter_1]

    # Join predictor variables into a string for use in formula
    predictor_formula = predictor_lookup.loc[
        predictor_lookup['predictor_full_name'].isin(predictors),
        'predictor_short_name'
    ].tolist()

    # SQL query for predictors
    predictor_sql = f"""
        SELECT {', '.join(
            keep_response + 
            predictor_formula + 
            [area_level] + (['pcn'] if area_level != 'pcn' else [])
        )}
        FROM population_master
        LEFT JOIN imd_2019 ON lsoa = lsoa_code
    """

    # Add filter areas?
    if filter_areas:
        patient_records = get_patient_records(
            sqlalchemy
                .text(predictor_sql.replace("ccg,", "population_master.ccg,") + """ 
                    JOIN ( 
                        values :filterAreas
                    ) t(ccg) on t.ccg = population_master.ccg
                """)
                .bindparams(sqlalchemy.bindparam("filterAreas", expanding=True)), 
            { 'filterAreas': filter_areas }
        )
    else:
        patient_records = get_patient_records(
            sqlalchemy.text(predictor_sql), {}
        )

    # Format data for logistic regression
    patient_records = format_glm_data(patient_records, area_level, keep_response, age_as_a_factor)

    # Specify training set
    if train_areas:
        train_areas = train_areas.replace('Fylde and Wyre', 'Fylde & Wyre').replace('F and W', 'F&W').replace('Ansdell and St Annes', 'Ansdell & St Annes')
        if area_level == 'pcn':
            training_records = patient_records[
                (patient_records['area_var'].isin(train_areas)) | (patient_records['ccg'].isin(train_areas))
            ]
        elif area_level == 'ccg_name':
            training_records = patient_records[
                (patient_records['pcn'].isin(train_areas)) | (patient_records['area_var'].isin(train_areas))
            ]
        else:
            training_records = patient_records[
                (patient_records['pcn'].isin(train_areas)) | (patient_records['ccg'].isin(train_areas))
            ]
    else:
        training_records = patient_records

    # Create model
    model = glm(
        formula="predict_var ~ " + ' + '.join(predictor_formula), 
        data=training_records, 
        family=families.Binomial(link = families.links.Logit())
    ).fit()

    # Make predictions
    print('Running logistic model...')
    predictions = compare_model_output(
        patient_records, 
        model.predict(patient_records), 
        len(predictors), 
        'logistic'
    )
    
    # Output data
    return {
        'model_match': predictions.to_dict('records'),
        "status": 200
    }