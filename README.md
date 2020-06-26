# TABASCO-MEXCOV-19

<p align="center">
  <img src = "figs/mexico.jpeg"/>
</p>

**Context**

Coronaviruses are a large family of viruses which may cause illness in animals or humans. In humans, several coronaviruses are known to cause respiratory infections ranging from the common cold to more severe diseases such as Middle East Respiratory Syndrome (MERS) and Severe Acute Respiratory Syndrome (SARS). The most recently discovered coronavirus is COVID-19 - World Health Organization. The number of new cases are increasing day by day around the world. This dataset has information from the states of Mexico at daily level.

**Content**

Population at state level: `population` <br/>
Individual level details per confirmed case: `confirmedraw` <br/>
Confirmed cases at daily level per region: `ts_confirmed`; Transposed: `trans_ts_confirmed` <br/>
Deaths at daily level per region: `ts_deaths`; Transposed: `trans_ts_deaths` <br/>
Estimated demographics based on census 2010: `demographics_2020e` <br/>
Estimated population based on census 2010: `population_2020e` <br/>

**Notes**

From your R script run

 1. `source("~/TABASCO-MEXCOV-19/src/packages/autoinstall.R")` to install/update and require used packages <br/>
 2. `source("~/TABASCO-MEXCOV-19/src/cleansing/<DATASET_NAME>.R")` to gather specific dataset <br/>


**Project Links**

  1. [Kaggle: COVID-19 Mexico](https://www.kaggle.com/carloslira/covid19-mexico)
  2. [Data source: github/carloscerlira](https://github.com/carloscerlira/COVIDMX/tree/master)
 
**Extra Links**
 
  1. [COVID-19 pandemic in Mexico](https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Mexico)
  2. [Demographics of Mexico](https://en.wikipedia.org/wiki/Demographics_of_Mexico#Demographic_dynamics)
  3. [Malattia da coronavirus (COVID-19)](https://www.google.com/search?sxsrf=ALeKk02Ayqjbn8ehNTAxQcjuA1NRcY_hHg%3A1592899181787&ei=bbbxXoTZL8fergSl1aD4Dw&q=mexico+covid+&oq=mexico+covid+&gs_lcp=CgZwc3ktYWIQAzIECCMQJzIECCMQJzIGCCMQJxATMgIIADIFCAAQywEyAggAMgUIABDLATIFCAAQywEyBQgAEMsBMgUIABDLAToGCAAQFhAeUJocWNEoYLEqaABwAHgAgAHPAYgBswiSAQU3LjIuMZgBAKABAaoBB2d3cy13aXo&sclient=psy-ab&ved=0ahUKEwjEw5Lvu5fqAhVHr4sKHaUqCP8Q4dUDCAw&uact=5#wptab=s:H4sIAAAAAAAAAONgVuLVT9c3NMwySk6OL8zJecTYxMgt8PLHPWGpyklrTl5jLOYS901NyUzOzEt1ySxOTSxO9clPTizJzM8T0uNic80rySypFFLhEpRCNUeDQYqfC1VISIOLA65XhotXilM_V98gydIgvgiompsLweXZxcTtkZqYU5IRXJJYUryIVQpEZxaXZCZnpCoUlyo45xfl5yWWZRaVFgMAVJFaHsIAAAA)
  
**ARIMA**
 
  1. [Kaggle](https://www.kaggle.com/nitishabharathi/the-story-of-covid-19-in-india-eda-and-prediction#Prediction-)
  2. [ARIMA in R](https://otexts.com/fpp2/arima-r.html) 
  3. [Using R for Time Series Analysis](https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html)
