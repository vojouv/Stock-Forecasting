# 📈 Stock Price Forecasting of PT Telekomunikasi Indonesia Tbk using ARIMA-GARCH

This project aims to forecast the **closing stock price** of **PT Telekomunikasi Indonesia Tbk (TLKM)** using a combined **ARIMA-GARCH** modeling approach.

---

## 🧠 Overview

The stock market plays a significant role in the economy, both directly and indirectly. Accurate forecasting of stock prices is essential for investors, policymakers, and researchers.

In this project:
- **ARIMA** is used to model time-series trends and dependencies.
- **GARCH** is used to model volatility clustering and conditional heteroskedasticity.

---

## 📅 Dataset

- **Time Range:** December 4, 2014 – December 17, 2014  
- **Observations:** 2,481  

---

## 🔍 Methodology

1. Stationarity check (ADF Test)
2. Differencing to achieve stationarity
3. Model selection based on AIC/BIC and residual diagnostics
4. Best ARIMA model: ARIMA(0,1,2)
5. GARCH model fitting to capture volatility
6. Best model found: **ARIMA(0,1,2)-GARCH(1,2)**
7. Forecasting and visualization

---
