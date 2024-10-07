import pandas as pd
import glob
import os
import logging

def merge_csv_files(dir_path, output_file_path):
    # Validate the directory path
    if not os.path.exists(dir_path):
        logging.error(f"The directory {dir_path} does not exist.")
        return

    # Get a list of all CSV files in the specified directory
    csv_files = glob.glob(dir_path + '/*.csv*')

    if not csv_files:
        logging.error(f"No CSV files found in the directory {dir_path}.")
        return

    # Initialize an empty list to hold DataFrames
    dfs = []

    # Read each CSV file into a DataFrame
    for file in csv_files:
        try:
            df = pd.read_csv(file)
            dfs.append(df)
            logging.info(f"Successfully read file {file}")
        except Exception as e:
            logging.error(f"Failed to read file {file} with error: {e}")

    # Concatenate all DataFrames into a single DataFrame
    combined_df = pd.concat(dfs)

    # Save the combined DataFrame to a new CSV file
    combined_df.to_csv(output_file_path, index=False)
    logging.info(f"Successfully saved the combined DataFrame to {output_file_path}")

# Define the directory path and output file path
dir_path = 'C:/Users/clair/OneDrive/Desktop/Bike Data/Datasets/2018/'
output_file_path = 'C:/Users/clair/OneDrive/Desktop/Bike Data/Datasets/2018.csv'

# Call the function
merge_csv_files(dir_path, output_file_path)