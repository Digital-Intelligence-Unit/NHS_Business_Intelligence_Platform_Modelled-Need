import index
import json
import os

os.environ['ENV'] = 'local'
result = index.LambdaHandler({
    "query": {
        "area_level": "GP Practice",
        "response_filter_1": "Coronary Heart Disease",
        "response_filter_2": "",
        "predictors": ["Age", "Sex"],
        "filter_areas": ['00Q','00R','00X','01A','01E','01K','02G','02M']
    }
})

print(json.dumps(result, indent=2))