# Introduction
Lambda for the modelled needs api, this service now resides in aws lambda to improve performance and scaling capabilities.

## Testing
python -m virtualenv env
./env/Scripts/python -m poetry install
./env/Scripts/python -m poetry run python modelled_needs/index.test.py

## Indexes required
The following indexes are required on the population_master table

CREATE INDEX IF NOT EXISTS idx_population_master_ccg
ON population_master(ccg);

CREATE INDEX IF NOT EXISTS idx_population_master_gpp_code
ON population_master(gpp_code);

CREATE INDEX IF NOT EXISTS idx_population_master_gpp_name
ON population_master(gpp_name);

CREATE INDEX IF NOT EXISTS idx_population_master_lsoa
ON population_master(lsoa);