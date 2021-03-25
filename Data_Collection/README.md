## Collecting Twitter data

The included four scripts describe the process fro collectin Twitter data in real time.
This is currently outdated (based on an earlier version of Twitter API) and is included to describe the manner in which data was obtained for the paper in question.

The scripts in this folder in the appropriate order:
1. Collection: Script that collects tweets from Twitter's streaming API
2. Pre-processing: Script that pre-processes the data by removing unwanted tweets
3. Processing: Lemmatization of tweets (and a stemming alternative)
4. Export for R: A script used to save processed data (processing is done in python) into feather, which can be easily read and utilized in R