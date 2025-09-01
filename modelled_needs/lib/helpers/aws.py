import os
import boto3
import json

class AWSHelper:
    session: boto3.Session

    def __init__(self):
        # Get secrets manager client
        if(os.getenv('ENV') == 'local'):
            self.session = boto3.Session(
                profile_name=os.environ.get('AWS_PROFILE')
            )
        else:
            self.session = boto3.Session()

    def configureEnvironment(self):
        # Get postgres credentials
        postgresCredentials = json.loads(
            self.session.client('secretsmanager').get_secret_value(SecretId='postgres')['SecretString']
        )
        os.environ['POSTGRES_HOST'] = postgresCredentials['host'] if(os.getenv('ENV') != 'local') else 'localhost'
        os.environ['POSTGRES_USERNAME'] = postgresCredentials['username']
        os.environ['POSTGRES_PASSWORD'] = postgresCredentials['password']