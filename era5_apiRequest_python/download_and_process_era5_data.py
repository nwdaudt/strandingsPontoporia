from class_env_data import ApiRequest

# Set up authentification for API request
# https://berenger.baospace.com/how-to-download-era-5-climate-datasets-using-python/

# call class (modify attributes accordingly, or run test suing default parameters):
A = ApiRequest()

# preparing the data and download netcdfs:
A.wrangling_and_download_data()
# clip land and calculate averages:
A.clip_data_and_summarise()
