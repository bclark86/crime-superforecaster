import os
import pandas
from scraper import load_yaml, query_crime


def main():

    config = load_yaml()

    raw_data_path = os.path.join(
        config['local_data']['dir'],
        config['local_data']['raw_dir']
    )

    if not os.path.exists(raw_data_path):
        os.makedirs(raw_data_path)

    crime_data_ty = query_crime(
        config['crime_data']['url'],
        config['crime_data']['current_year_id']
    )

    crime_data_ty.to_csv(
        os.path.join(raw_data_path, config['local_data']['current_year_file']),
        index=False
    )

    # crime_data_historical = query_crime(
    #     config['crime_data']['url'],
    #     config['crime_data']['historical_id']
    # )
    # 
    # crime_data_historical.to_csv(
    #     os.path.join(raw_data_path, config['local_data']['historical_file']),
    #     index=False
    # )


if __name__ == '__main__':
    main()
