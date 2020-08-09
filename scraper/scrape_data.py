import pandas as pd
from sodapy import Socrata
import time


def query_socrata(dataset_url, identifier, query, limit=10000, offset=0):

    # Unauthenticated client only works with public data sets. Note 'None'
    # in place of application token, and no username or password:
    client = Socrata(dataset_url, None)

    # initialize master dataframe
    df = pd.DataFrame()

    # start timer for reference
    start_time = time.time()

    # begin query loop
    query_results = -1
    while query_results != 0:

        # add throttling
        throttled_query = query + f"""
            LIMIT {limit}
            OFFSET {offset}
            """

        # return query results
        results = client.get(identifier, query=throttled_query)

        # convert to pandas DataFrame
        results_df = pd.DataFrame.from_records(results)

        query_results = results_df.shape[0]

        if results_df.shape[0] < 1:
            print("Done.")
            print()

        else:
            # append to master dataframe
            df = df.append(results_df)

            # adjust offset to the next chunk
            offset += limit

            # printing for the user
            print("...")

            # throttle requests
            time.sleep(5)

    total_time = time.time() - start_time

    print(f"Data source: {dataset_url} - {identifier}")
    print(f"Time to execute: {round(total_time, 2)} seconds")
    print(f"Query summary: {df.shape[0]} rows and {df.shape[1]} columns")
    
    if 'date' in df.columns:
        df['date'] = pd.to_datetime(df['date'], errors='coerce')

    return df


def query_crime(dataset_url, identifier):
    
    crime_query = "SELECT * WHERE law_cat_cd = 'FELONY'"

    crime_df = query_socrata(dataset_url, identifier, crime_query)
    
    return crime_df

