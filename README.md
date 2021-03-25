## STM and LSS Tweet analysis

This repository contains code to reproduce the method presented in the following paper: Vydra, S. & Kantorowicz, J. (2021). Tracing policy-relevant information in social media: The case of Twitter before and during the COVID-19 crisis. _Statistics, Politics and Policy_ 

It utilizes processed tweets. The method to gather and process these Tweets is explained (and can be reproduced) using scripts in the Data_Collection folder. This data constitutes a simple data frame of processed text and document co-variates, making it simple to adapt this method to any textual data in this format.

The method consists of three scripts:
1. STM find k: Contains code to calculate and visualise metrics for model quality across models with varying topic number
2. LSS: Contains code that trains and inspects LSS models (to later be used in the STM script)
3. STM: Contains code that applies an LSS model and trains an STM model to then visualise topics (including their aggregate LSS score), difference in topic prevalence, and difference in LSS polarity scores


 