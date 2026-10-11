# Time Series Modeling and Forecasting

This repository presents a series of projects completed for the course **02417 Time Series Analysis** at the Technical University of Denmark (DTU).

The projects cover a progression of statistical time-series methods, from classical trend estimation and recursive forecasting to ARMA processes, dynamic regression, state-space models, and Kalman filtering.

The coursework combines statistical modelling, forecasting, model diagnostics, simulation, and uncertainty assessment using **R**.

---

## Project Overview

The coursework consists of four assignments covering increasingly advanced time-series modelling techniques:

| Assignment | Main Topics |
|---|---|
| **1 — Trend Modeling** | OLS, WLS, recursive least squares, forgetting factors, forecasting |
| **2 — ARMA & Seasonal Models** | Stationarity, invertibility, ACF/PACF, ARMA identification, seasonal processes |
| **3 — ARX & Forecasting** | AR models, solar-power forecasting, ARX / transfer-function models |
| **4 — State-Space & Kalman Filtering** | State-space models, Kalman filter, maximum likelihood estimation, Monte Carlo analysis |

Together, the assignments demonstrate how different time-series models can be selected, estimated, diagnosed, and used for forecasting under different data structures and modelling assumptions.

---

## Assignment 1 — Trend Modeling and Recursive Estimation

The first assignment focused on modelling and forecasting a time series of registered motor vehicles in Denmark.

### Methods

The analysis included:

- Data preprocessing and train/test splitting
- Ordinary Least Squares (OLS)
- Weighted Least Squares (WLS)
- Local linear trend modelling
- Forecasting with 95% prediction intervals
- RMSE evaluation
- Residual diagnostics
- Autocorrelation analysis
- Recursive Least Squares (RLS)
- RLS with forgetting factors
- One-step-ahead prediction
- Multi-step forecast evaluation
- Optimization of the forgetting factor λ

### Key Insight

A global linear trend can provide reasonable point forecasts while still failing to capture the temporal dependence and uncertainty structure of the series.

Residual autocorrelation and poor prediction-interval coverage demonstrated why forecast evaluation should consider more than point prediction accuracy alone.

The analysis also showed how weighting recent observations more heavily can allow a model to adapt to changing local trends.

---

## Assignment 2 — ARMA and Seasonal Time-Series Models

The second assignment introduced stochastic time-series processes and model identification.

### Topics Covered

- Stationarity
- Invertibility
- Autocorrelation Function (ACF)
- Partial Autocorrelation Function (PACF)
- Autoregressive processes
- Moving-average processes
- ARMA model identification
- Seasonal autoregressive processes
- Seasonal moving-average processes
- Time-series simulation

The assignment focused on understanding how theoretical model structures appear in observed time-series behaviour and how ACF/PACF patterns can support model identification.

---

## Assignment 3 — ARX Modeling and Forecasting

The third assignment extended the analysis toward practical forecasting and dynamic regression.

### Methods

The work included:

- Autoregressive model simulation
- AR model estimation
- Forecasting of monthly solar-power production
- Seasonal time-series modelling
- Residual diagnostics
- ACF-based model validation
- Multi-step forecasting
- ARX modelling
- Transfer-function modelling
- Dynamic relationships between input and output variables

### Key Insight

Model validation was treated as an essential part of forecasting rather than relying only on goodness of fit.

Residual behaviour was examined to determine whether the fitted models had adequately captured the temporal structure of the data before forecasts were produced.

The assignment also demonstrated how external explanatory variables can be incorporated into time-series models through ARX-type structures.

---

## Assignment 4 — State-Space Models and Kalman Filtering

The final assignment introduced state-space modelling and recursive estimation using the Kalman filter.

### Methods

The analysis included:

- State-space model formulation
- Latent-state estimation
- Kalman filtering
- Prediction and filtering uncertainty
- Maximum likelihood parameter estimation
- Monte Carlo simulation
- Forecast evaluation
- Transformer temperature prediction
- Robustness analysis under non-Gaussian noise

The assignment also investigated the behaviour of the Gaussian Kalman filter when the underlying noise followed a heavy-tailed Student-t distribution.

### Key Insight

State-space models provide a flexible framework for representing systems whose underlying states cannot be observed directly.

The Kalman filter enables these latent states to be estimated sequentially as new observations become available, while explicitly accounting for uncertainty.

The robustness analysis further demonstrated that model assumptions about the noise distribution can influence estimation and forecasting performance.

---

## My Contribution

I took **primary responsibility for the majority of the coursework and R-based analysis** presented in this project. Due to reduced group availability during the semester, I completed a substantial share of the assignments and technical work.

My contributions included:

- Implementing time-series models in R
- Developing forecasting workflows
- Implementing recursive estimation procedures
- Comparing model parameters and forecasting strategies
- Performing residual and autocorrelation diagnostics
- Evaluating forecast performance and uncertainty
- Producing statistical visualizations
- Interpreting model results
- Contributing substantially to the preparation of the assignment reports

The R code retained for Assignment 1 was developed as part of this work and demonstrates the implementation and evaluation of OLS, WLS, recursive least squares, forgetting-factor estimation, one-step-ahead prediction, and forecast-horizon-based parameter optimization.

---

## Programming & Implementation

The statistical analyses for the coursework were implemented in **R by the project group**, with the majority of the coursework and R-based analysis completed under my primary responsibility.

The work involved both standard statistical functions and custom implementation of modelling procedures required by the assignments.

Examples include:

- OLS and WLS estimation
- Recursive Least Squares
- Forgetting-factor updates
- One-step-ahead prediction
- Multi-step forecasting
- RMSE-based model comparison
- Forecast-horizon evaluation
- Residual diagnostics
- ACF analysis
- Time-series simulation
- ARMA / ARX modelling
- State-space analysis
- Kalman filtering
- Monte Carlo analysis
- Statistical visualization

The original R source code retained for **Assignment 1** is included in this repository. The source-code files for Assignments 2–4 were not retained, but their methodology, analyses, and results are documented in the corresponding reports.

---

## Assignment 1 R Implementation

The available Assignment 1 source code demonstrates several time-series modelling and forecasting procedures implemented in R.

### OLS and Forecast Evaluation

The analysis fits a global linear trend model and evaluates forecasts using:

- 95% prediction intervals
- RMSE
- Prediction-interval coverage
- Residual plots
- Residual ACF

### Weighted Least Squares

Exponentially decreasing weights are used to give greater importance to recent observations:

$$
w_t = \lambda^{N-t}
$$

Different values of the forgetting factor λ are compared to investigate how strongly the model should respond to recent observations.

### Recursive Least Squares

Recursive estimation was implemented to update model parameters sequentially as new observations became available.

Both standard RLS and RLS with forgetting were investigated.

### Forecast-Horizon Optimization

The analysis evaluates candidate forgetting factors over multiple forecast horizons.

For each horizon from **1 to 12 steps ahead**, RMSE is calculated across a grid of λ values to identify the forgetting factor producing the lowest forecast error.

This demonstrates how model tuning can depend on the intended forecasting horizon rather than having a single universally optimal parameter.

---

## Key Skills Demonstrated

### Time-Series Analysis

- Trend modelling
- Stationarity and invertibility
- ACF and PACF interpretation
- AR / MA / ARMA processes
- Seasonal modelling
- ARX models
- State-space models
- Kalman filtering

### Statistical Modeling

- Ordinary Least Squares
- Weighted Least Squares
- Recursive estimation
- Maximum likelihood estimation
- Dynamic model evaluation
- Monte Carlo simulation

### Forecasting

- One-step-ahead forecasting
- Multi-step forecasting
- Prediction intervals
- Forecast-horizon analysis
- RMSE evaluation
- Model comparison

### Model Diagnostics

- Residual analysis
- Autocorrelation diagnostics
- Prediction-interval coverage
- White-noise assessment
- Model-assumption evaluation

### Programming

- R
- Statistical programming
- Custom modelling functions
- Iterative algorithms
- Data preprocessing
- Statistical visualization

---

## Repository Structure

```text
time-series-modeling-and-forecasting/
├── assignment-1-trend-modeling/
│   ├── assignment-1-analysis.R
│   └── assignment-1-report.pdf
├── assignment-2-arma-seasonal/
│   └── assignment-2-report.pdf
├── assignment-3-arx-forecasting/
│   └── assignment-3-report.pdf
├── assignment-4-kalman-filter/
│   └── assignment-4-report.pdf
└── README.md
```

The repository includes all four assignment reports together with the retained R source code for Assignment 1.

---

## Project Information

**Course:** 02417 Time Series Analysis  
**Institution:** Technical University of Denmark (DTU)  
**Semester:** Spring 2026  
**Programming Language:** R  
**Project Type:** Statistical time-series modelling and forecasting
