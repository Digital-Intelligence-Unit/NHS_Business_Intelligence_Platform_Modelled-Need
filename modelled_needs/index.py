from lib.model import get_model
from lib.helpers.aws import AWSHelper

# Creates class instance for lambda
def LambdaHandler(event, lambda_context = None):
    # Initialise AWS
    AWSHelper().configureEnvironment()

    # Query model
    try:
        return get_model(
            event['query']['response_filter_1'],
            event['query']['response_filter_2'] if ('response_filter_2' in event['query']) else '',
            event['query']['predictors'],
            event['query']['area_level'] if ('area_level' in event['query']) else 'GP Practice',
            event['query']['filter_areas'],
            event['query']['train_areas'] if ('train_areas' in event['query']) else []
        )
    except BaseException as e:
        print(e)
        return {
            'model_match': [],
            'status': 500
        }
