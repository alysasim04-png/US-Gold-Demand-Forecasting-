# US-Gold-Demand-Forecasting-
An advanced macroeconomic time series forecasting pipeline utilizing STLF, ARIMA with reg and Ensemble Bagging frameworks in R

## Project Overview
This project was executed as part of advanced analytical coursework, achieving a perfect **100/100 technical evaluation score**. It implements rigorous data cleaning pipelines, structural stationarity transformations, and comparative predictive modeling.

##Tech Stack & Libraries used 
* **Language:** R
* **Core Analytics & Time Series:** 'forecast', 'tseries, 'zoo'
* **Data Wrangling & Pipeline Engineering:** 'tidyverse' (`dplyr`, `tidyr`, `purrr`), `lubridate`, `readxl`

## 🧠 Modeling Methodologies Evaluated
1. **STL Decomposition:** Leveraged to separate structural seasonal components from volatile demand trends.
2. **Dynamic Regression with ARIMA Errors (`xreg`):** Modeled autocorrelated residuals while incorporating a matrix of time-varying macroeconomic indicators.
3. **Bootstrap Aggregation (Bagged ETS Ensemble):** The winning architectural framework, successfully minimizing out-of-sample Root Mean Squared Error (RMSE) across a 2025 forecasting horizon.

## 📂 Repository Contents
* `codes.R`: Clean, production-ready, fully commented end-to-end code script.
* `Report.pdf`: Complete 28-page technical report detailing data cleaning logic, ADF stationarity validation tests, residual diagnostic plots, and predictive summaries.
