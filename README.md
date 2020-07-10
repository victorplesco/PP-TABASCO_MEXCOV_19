# TABASCO-MEXCOV-19

<p align="center">
  <img src = "figs/mexico.jpeg"/>
</p>

**Context**

The COVID-19 pandemic in Mexico is part of the ongoing worldwide pandemic of coronavirus disease 2019 (COVID-19) caused by severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2). The virus was confirmed to have reached Mexico in February 2020. However, the National Council of Science and Technology (CONACYT) reported two cases of COVID-19 in mid-January 2020 in the states of Nayarit and Tabasco, one case per state. As of July 9, there had been 282,283 confirmed cases of COVID-19 in Mexico and 33,526 reported deaths.

**Content**

**[UPDATED 18/04]** Population at state level: `population` <br/>

**[UPDATED 25/06]** Individual level details per buffer: `buffersraw` <br/>
**[UPDATED 25/06]** Buffers at daily level per region: `buffers_ts`<br/>
**[UPDATED 25/06]** Deaths at daily level per region: `deaths_ts` <br/>

Structured data for classification: `dtf_classification` <br/>
Structured data for regression: `dtf_regression` <br/>
Structured data for ARIMA: `dtf_arima` <br/>

**[TO BE DEFINED]** Estimated demographics based on census 2010: `demographics_2020e` <br/>
**[TO BE DEFINED]** Estimated population based on census 2010: `population_2020e` <br/>

**Notes**

From your R script run

 1. `source("~/TABASCO-MEXCOV-19/src/packages/autoinstall.R")` to install/update and require used packages <br/>
 2. `source("~/TABASCO-MEXCOV-19/src/cleansing/<DATASET_NAME>.R")` to gather specific dataset <br/>


**Project Links**

  1. [Gobierno de Mexico](https://www.gob.mx/salud/documentos/datos-abiertos-152127)
  2. [COVID-19 pandemic in Mexico](https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Mexico)
  3. [Demographics of Mexico](https://en.wikipedia.org/wiki/Demographics_of_Mexico#Demographic_dynamics)
  4. [Real-time data (COVID-19)](https://www.google.com/search?sxsrf=ALeKk02Ayqjbn8ehNTAxQcjuA1NRcY_hHg%3A1592899181787&ei=bbbxXoTZL8fergSl1aD4Dw&q=mexico+covid+&oq=mexico+covid+&gs_lcp=CgZwc3ktYWIQAzIECCMQJzIECCMQJzIGCCMQJxATMgIIADIFCAAQywEyAggAMgUIABDLATIFCAAQywEyBQgAEMsBMgUIABDLAToGCAAQFhAeUJocWNEoYLEqaABwAHgAgAHPAYgBswiSAQU3LjIuMZgBAKABAaoBB2d3cy13aXo&sclient=psy-ab&ved=0ahUKEwjEw5Lvu5fqAhVHr4sKHaUqCP8Q4dUDCAw&uact=5#wptab=s:H4sIAAAAAAAAAONgVuLVT9c3NMwySk6OL8zJecTYxMgt8PLHPWGpyklrTl5jLOYS901NyUzOzEt1ySxOTSxO9clPTizJzM8T0uNic80rySypFFLhEpRCNUeDQYqfC1VISIOLA65XhotXilM_V98gydIgvgiompsLweXZxcTtkZqYU5IRXJJYUryIVQpEZxaXZCZnpCoUlyo45xfl5yWWZRaVFgMAVJFaHsIAAAA)
