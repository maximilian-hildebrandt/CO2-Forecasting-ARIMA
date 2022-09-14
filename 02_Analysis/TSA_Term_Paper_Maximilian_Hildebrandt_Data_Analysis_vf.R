###################################################
# Time Series Analysis, Summer term 2021
# CO2 Emission Forecasting 
###################################################

# 1. Loading packages & data
# Install & load relevant packages
#install.packages("forecast") 
#install.packages("stats") 
#install.packages("urca") 
#install.packages("tseries") 
#install.packages("ggplot2") 

library(forecast)
library(stats)
library(urca)
library(tseries)
library(ggplot2)
library(lmtest)


# Import data
dataset <- read.table('C:/Users/M/Dropbox/05 Statistik-Master/2. Semester/Time Series Analysis/04 Hausarbeit Time Series Analysis/Data Analysis/Worldbank CO2 Emissions Processed vf.csv', header=TRUE, sep=';')  # load the data
ts <- dataset[,2:4]
head(dataset)

# 2. Define parameters
# Define parameter settings: Set maximum number of lags of AR and MA polynomials to
pmax <- 6
qmax <- 6
pmax_ac <- 6 # and max number of lags in Ljung-Box test

# Define parameters for train-test-split, test set size equal forecasting time period (i.e., 2030-2017 = 13 years)
train_start_year <- 1960
train_end_year <- 2002
test_start_year <- 2003
test_end_year <- 2016

# 3. Preprocess data
# Full range (for plotting)
date <- ts(dataset$ï..Jahr)
EU <- ts(dataset$European.Union, frequency=1, start=c(1960,1), end=c(2016,1)) # Set dependent variable
CN <- ts(dataset$China, frequency=1, start=c(1960,1), end=c(2016,1)) 
US <- ts(dataset$United.States, frequency=1, start=c(1960,1), end=c(2016,1))

# Define training set (in-sample) to estimate model parameters
# Apply log transformation to stabilize variance for EU and China based on plots and residual diagnostics
EU_train_set <- window(log(EU),start=c(train_start_year,1),end=c(train_end_year,1))
CN_train_set <- window(log(CN),start=c(train_start_year,1),end=c(train_end_year,1))
US_train_set <- window(US,start=c(train_start_year,1),end=c(train_end_year,1))

# Define test set (out-of-sample) to assess forecast accuracy
EU_test_set <- window(log(EU), start=c(test_start_year,1),end=c(test_end_year,1))
CN_test_set <- window(log(CN),start=c(test_start_year,1),end=c(test_end_year,1))
US_test_set <- window(US,start=c(test_start_year,1),end=c(test_end_year,1))

# 4. Conduct descriptive analyses (to full dataset)
# EU
summary(EU)
M <- mean(EU)
SD <- sd(EU)
CV <- SD/M*100 # Coefficient of variation
print(SD)
print(CV)

# CN
summary(CN)
M <- mean(CN)
SD <- sd(CN)
CV <- SD/M*100 # Coefficient of variation
print(SD)
print(CV)

# US
summary(US)
M <- mean(US)
SD <- sd(US)
CV <- SD/M*100 # Coefficient of variation
print(SD)
print(CV)

# Plot integrated time series plot
China <- CN
dat_int <- ts.intersect(EU, China, US)
plot.ts(dat_int, cex.lab=1.5, cex.axis=1.5, main="")

# 5. Assess stationarity of time series as a foundation for ARIMA modeling
# Plot specific plots to assess stationarity of training set
options(scipen=10) # reformat axis numbers in order to ensure that full numbers are shown
par(font.lab=2, font.axis=1) # Create bold font titles
plot(EU_train_set, ylab="EU CO2 emissions (kt)") 
plot(CN_train_set, ylab="Logarithmized China CO2 emissions (kt)") 
plot(US_train_set, ylab="US CO2 emissions (kt)") 
plot(diff(EU_train_set), ylab="Annual changes in EU CO2 emissions (kt)") 
plot(diff(CN_train_set), ylab="Annual changes in China CO2 emissions (%)") 
plot(diff(US_train_set), ylab="Annual changes in US CO2 emissions (kt)") 
plot(diff(diff(EU_train_set)), ylab="2nd order differences for EU CO2 emissions (kt)") 

# Compare different orders for EU data to decide on stationarity
layout(matrix(c(1,2, 3), nrow=3, byrow=TRUE))
plot(EU_train_set, ylab="EU CO2 emissions (kt)") 
plot(diff(EU_train_set), ylab="Annual changes in EU CO2 emissions (kt)") 
plot(diff(diff(EU_train_set)), ylab="2nd order differences for EU CO2 emissions (kt)") 

# Test for stationarity using ADF
# Determine number of differences required
print("Number of differences required based on trend (+intercept for ADF) component:")
ndiffs(EU_train_set, alpha = 0.05, test = "adf", type ="level")
ndiffs(CN_train_set, alpha = 0.05, test = "adf", type ="level")
ndiffs(US_train_set, alpha = 0.05, test = "adf", type ="level")
ndiffs(EU_train_set, alpha = 0.05, test = "adf", type ="trend")
ndiffs(CN_train_set, alpha = 0.05, test = "adf", type ="trend")
ndiffs(US_train_set, alpha = 0.05, test = "adf", type ="trend")

# Compute test statistics for ADF test
# For EU emissions:
pmax_adf = round(12*(length(EU_train_set)/100)^0.25, 0)  # truncation lag according to Schwert (1989); length equal across countries
adf_drift_EU = ur.df (EU_train_set, type="drift" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with drift) for the EU emission data:")
summary (adf_drift_EU)
punitroot(q=adf_drift_EU@teststat[1], N=length(EU_train_set), trend="c")
length(adf_drift_EU@testreg$coefficients[,1])-2 #lag length
adf_drift_EU_diff = ur.df (diff(EU_train_set), type="drift" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with drift) for the EU emission data (first order differences):")
summary (adf_drift_EU_diff)
punitroot(q=adf_drift_EU_diff@teststat[1], N=length(diff(EU_train_set)), trend="c")
length(adf_drift_EU_diff@testreg$coefficients[,1])-2 #lag length
adf_drift_EU_diff_diff = ur.df (diff(diff(EU_train_set)), type="drift" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with drift) for the EU emission data (second order differences):")
summary (adf_drift_EU_diff_diff)
punitroot(q=adf_drift_EU_diff_diff@teststat[1], N=length(diff(diff(EU_train_set))), trend="c")
length(adf_drift_EU_diff_diff@testreg$coefficients[,1])-2 #lag length
adf_trend_EU = ur.df (EU_train_set, type="trend" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with trend) for the EU emission data:")
summary (adf_trend_EU)
punitroot(q=adf_trend_EU@teststat[1], N=length(EU_train_set), trend="ct")
length(adf_drift_EU@testreg$coefficients[,1])-3 #lag length, due to trend test there is one additional parameter that needs to be subtracted
adf_trend_EU_diff = ur.df (diff(EU_train_set), type="trend" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with trend) for the EU emission data (first order differences):")
summary (adf_trend_EU_diff)
punitroot(q=adf_trend_EU_diff@teststat[1], N=length(diff(EU_train_set)), trend="ct")
length(adf_drift_EU@testreg$coefficients[,1])-3 #lag length, due to trend test there is one additional parameter that needs to be subtracted
adf_drift_EU_diff_diff = ur.df (diff(diff(EU_train_set)), type="trend" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with trend) for the EU emission data (second order differences):")
summary (adf_drift_EU_diff_diff)
punitroot(q=adf_drift_EU_diff_diff@teststat[1], N=length(diff(diff(EU_train_set))), trend="ct")
length(adf_drift_EU_diff_diff@testreg$coefficients[,1])-3 #lag length

#China
adf_drift_CN = ur.df (CN_train_set, type="drift" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with drift) for the China emission data:")
summary (adf_drift_CN)
punitroot(q=adf_drift_CN@teststat[1], N=length(CN_train_set), trend="c") 
length(adf_drift_CN@testreg$coefficients[,1])-2 #lag length
adf_drift_CN_diff = ur.df (diff(CN_train_set), type="drift" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with drift) for the China emission data (first order differences):")
summary (adf_drift_CN_diff)
punitroot(q=adf_drift_CN_diff@teststat[1], N=length(diff(CN_train_set)), trend="c")
length(adf_drift_CN@testreg$coefficients[,1])-2 #lag length
adf_trend_CN = ur.df (CN_train_set, type="trend" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with trend) for the China emission data:")
summary (adf_trend_CN)
punitroot(q=adf_trend_CN@teststat[1], N=length(CN_train_set), trend="ct")
length(adf_drift_CN@testreg$coefficients[,1])-3 #lag length, due to trend test there is one additional parameter that needs to be subtracted
adf_trend_CN_diff = ur.df (diff(CN_train_set), type="trend" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with trend) for the China emission data (first order differences):")
summary (adf_trend_CN_diff)
punitroot(q=adf_trend_CN_diff@teststat[1], N=length(diff(CN_train_set)), trend="ct")
length(adf_drift_CN@testreg$coefficients[,1])-3 #lag length, due to trend test there is one additional parameter that needs to be subtracted

#US
adf_drift_US = ur.df (US_train_set, type="drift" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with drift) for the US emission data:")
summary (adf_drift_US)
punitroot(q=adf_drift_US@teststat[1], N=length(US_train_set), trend="c")
length(adf_drift_US@testreg$coefficients[,1])-2 #lag length
adf_drift_US_diff = ur.df (diff(US_train_set), type="drift" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with drift) for the US emission data (first order differences):")
summary (adf_drift_US_diff)
punitroot(q=adf_drift_US_diff@teststat[1], N=length(diff(US_train_set)), trend="c")
length(adf_drift_US_diff@testreg$coefficients[,1])-2 #lag length
adf_trend_US = ur.df (US_train_set, type="trend" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with trend) for the US emission data:")
summary (adf_trend_US)
punitroot(q=adf_trend_US@teststat[1], N=length(US_train_set), trend="ct")
length(adf_trend_US@testreg$coefficients[,1])-3 #lag length, due to trend test there is one additional parameter that needs to be subtracted
adf_trend_US_diff = ur.df (diff(US_train_set), type="trend" , selectlags="BIC", lags=pmax_adf)
print("ADF test results (with trend) for the US emission data (first order differences):")
summary (adf_trend_US_diff)
punitroot(q=adf_trend_US_diff@teststat[1], N=length(diff(US_train_set)), trend="ct")
length(adf_trend_US_diff@testreg$coefficients[,1])-3 #lag length, due to trend test there is one additional parameter that needs to be subtracted

# Conduct diagnostic (partial) autocorrelation tests to assess non-stationarity and support decision for best model
par(mfrow=c(3,2))
acf_EU <- acf(EU_train_set, plot = FALSE)
plot(acf_EU, ylab="Annual Total EU CO2 Emissions ACF", main="", cex.lab=1.5, cex.axis=1.5)
pacf_EU <- pacf(EU_train_set, plot = FALSE)
plot(pacf_EU, ylab="Annual Total EU CO2 Emissions PACF", main="", cex.lab=1.5, cex.axis=1.5)
acf_EU_diff <- acf(diff(EU_train_set), plot = FALSE)
plot(acf_EU_diff, ylab="Annual Changes in EU CO2 Emissions ACF", main="", cex.lab=1.5, cex.axis=1.5)
pacf_EU_diff <- pacf(diff(EU_train_set), plot = FALSE)
plot(pacf_EU_diff, ylab="Annual Changes in EU CO2 Emissions PACF", main="", cex.lab=1.5, cex.axis=1.5)
acf_EU_diff_diff <- acf(diff(diff(EU_train_set)), plot = FALSE)
plot(acf_EU_diff_diff, ylab="2nd Order Differences of EU CO2 Emissions ACF", main="", cex.lab=1.5, cex.axis=1.5)
pacf_EU_diff_diff <- pacf(diff(diff(EU_train_set)), plot = FALSE)
plot(pacf_EU_diff_diff, ylab="2nd Order Differences of EU CO2 Emissions PACF", main="", cex.lab=1.5, cex.axis=1.5)
par(mfrow=c(2,2))
acf_CN <- acf(CN_train_set, plot = FALSE)
plot(acf_CN, ylab="Annual Total Chinese CO2 Emissions (log) ACF", main="", cex.lab=1.5, cex.axis=1.5)
pacf_CN <- pacf(CN_train_set, plot = FALSE)
plot(pacf_CN, ylab="Annual Total Chinese CO2 Emissions  (log) PACF", main="", cex.lab=1.5, cex.axis=1.5)
acf_CN_diff <- acf(diff(CN_train_set), plot = FALSE)
plot(acf_CN_diff, ylab="Annual Changes in Chinese CO2 Emissions  (log) ACF", main="", cex.lab=1.5, cex.axis=1.5)
pacf_CN_diff <- pacf(diff(CN_train_set), plot = FALSE)
plot(pacf_CN_diff, ylab="Annual Changes in Chinese CO2 Emissions  (log) PACF", main="", cex.lab=1.5, cex.axis=1.5)
par(mfrow=c(2,2))
acf_US <- acf(US_train_set, plot = FALSE)
plot(acf_US, ylab="Annual Total US CO2 Emissions ACF", main="", cex.lab=1.5, cex.axis=1.5)
pacf_US <- pacf(US_train_set, plot = FALSE)
plot(pacf_US, ylab="Annual Total US CO2 Emissions PACF", main="", cex.lab=1.5, cex.axis=1.5)
acf_US_diff <- acf(diff(US_train_set), plot = FALSE)
plot(acf_US_diff, ylab="Annual Changes in US CO2 Emissions ACF", main="", cex.lab=1.5, cex.axis=1.5)
pacf_US_diff <- pacf(diff(US_train_set), plot = FALSE)
plot(pacf_US_diff, ylab="Annual Changes in US CO2 Emissions PACF", main="", cex.lab=1.5, cex.axis=1.5)

# 6. Model selection based on statistical criteria
# Identify the best ARMA(p,q) model through computation
modelEU.aic <- auto.arima(diff(diff(EU_train_set)), ic="aic", d=0, max.p=pmax, max.q=qmax, seasonal=FALSE, trace=TRUE, allowmean=FALSE)  # AIC
modelEU.bic <- auto.arima(diff(diff(EU_train_set)), ic="bic", d=0, max.p=pmax, max.q=qmax, seasonal=FALSE, trace=TRUE, allowmean=FALSE)  # BIC
modelCN.aic <- auto.arima(diff(CN_train_set), ic="aic", d=0, max.p=pmax, max.q=qmax, seasonal=FALSE, trace=TRUE, allowmean=FALSE)  # AIC
modelCN.bic <- auto.arima(diff(CN_train_set), ic="bic", d=0, max.p=pmax, max.q=qmax, seasonal=FALSE, trace=TRUE, allowmean=FALSE)  # BIC
modelUS.aic <- auto.arima(diff(US_train_set), ic="aic", d=0, max.p=pmax, max.q=qmax, seasonal=FALSE, trace=TRUE, allowmean=TRUE)  # AIC
modelUS.bic <- auto.arima(diff(US_train_set), ic="bic", d=0, max.p=pmax, max.q=qmax, seasonal=FALSE, trace=TRUE, allowmean=TRUE)  # BIC

# Select and store the most relevant models
EU_p <- modelEU.aic$arma[1]
EU_q <- modelEU.aic$arma[2]
CN_p <- modelEU.aic$arma[1]
CN_q <- modelEU.aic$arma[2]
US_p <- modelEU.aic$arma[1]
US_q <- modelEU.aic$arma[2]

# 7. Conduct residual diagnostics & store model results:
# For EU
layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE)) # Create a compact layout for the resulting charts
EU_e <- modelEU.aic$residuals # Store residuals
plot (EU_e, type="l", ylab="EU Model Residuals") # Residual plot
plot (EU_e^2, type="l", ylab="EU Squared Model Residuals") # Squared residual plot
Acf(EU_e,12, main="", ylab="EU Model Residuals ACF") # ACF plot to test for remaining auto-correlation
hist(EU_e, main="", xlab="EU Model Residuals") # Histogram to assess normality of residuals
LB.modelEU.aic <- Box.test(EU_e, lag=pmax_ac, type="Ljung-Box", fitdf=(EU_p+EU_q)) # Ljung-box test for auto-correlation
print(LB.modelEU.aic)
MCL.modelEU.aic <- Box.test(EU_e^2, lag=pmax_ac, type="Ljung-Box")  # McLeod-Li test for conditional heteroscedasticity
print(MCL.modelEU.aic)
JB.modelEU.aic <- jarque.bera.test(EU_e) # Jarque-Bera test for normality of residuals
print(JB.modelEU.aic)
# Print model
coeftest(modelEU.aic) # Test significance of coefficients & print coefficients

# For China
layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))
CN_e <- modelCN.aic$residuals    # Compute the residuals
plot (CN_e, type="l", ylab="China Model Residuals") # Residual plot
plot (CN_e^2, type="l", ylab="China Model Squared Residuals") # Squared residual plot
Acf(CN_e,12, main="", ylab="China Model Residuals ACF") # ACF plot to test for remaining auto-correlation
hist(CN_e, main="", xlab="China Model Residuals") # Histogram to assess normality of residuals
LB.modelCN.aic <- Box.test(CN_e, lag=pmax_ac, type="Ljung-Box", fitdf=(CN_p+CN_q)) # Ljung-box test for auto-correlation
print(LB.modelCN.aic)
MCL.modelCN.aic <- Box.test(CN_e^2, lag=pmax_ac, type="Ljung-Box")  # McLeod-Li test for conditional heteroscedasticity
print(MCL.modelCN.aic)
JB.modelCN.aic <- jarque.bera.test(CN_e) # Jarque-Bera test for normality of residuals
print(JB.modelCN.aic)
# Print model
coeftest(modelCN.aic) # Test significance of coefficients & print coefficients

# For US
layout(matrix(c(1,2, 3, 4), nrow=2, byrow=TRUE))
US_e <- modelUS.aic$residuals    # Compute the residuals
plot (US_e, type="l", ylab="US Model Residuals") # Residual plot
plot (US_e^2, type="l", ylab="Squared US Model Residuals") # Squared residual plot
Acf(US_e,12, main="", ylab="US Model Residuals ACF") # ACF plot to test for remaining auto-correlation
hist(US_e, main="", xlab="US Model Residuals") # Histogram to assess normality of residuals
LB.modelUS.aic <- Box.test(US_e, lag=pmax_ac, type="Ljung-Box", fitdf=(US_p+US_q)) # Ljung-box test for auto-correlation
print(LB.modelUS.aic)
MCL.modelUS.aic <- Box.test(US_e^2, lag=pmax_ac, type="Ljung-Box")  # McLeod-Li test for conditional heteroscedasticity
print(MCL.modelUS.aic)
JB.modelUS.aic <- jarque.bera.test(US_e) # Jarque-Bera test for normality of residuals
print(JB.modelUS.aic)

# 8. Compute impulse response functions to test persistance of model
# Set parameters
max.lag <- 36
ar.coef <- 0
ma.coef <- 0

# EU
model <- modelEU.aic
p <- model$arma[1]
q <- model$arma[2]
if (p>0) {ar.coef <- model$coef[1:p]}
if (q>0) {ma.coef <- model$coef[(p+1):(p+q)]}
IRF_EU <- c(1, ARMAtoMA(ar=ar.coef, ma=ma.coef, lag.max=max.lag))

#China
model <- modelCN.aic
p <- model$arma[1]
q <- model$arma[2]
if (p>0) {ar.coef <- model$coef[1:p]}
if (q>0) {ma.coef <- model$coef[(p+1):(p+q)]}
IRF_CN <- c(1, ARMAtoMA(ar=ar.coef, ma=ma.coef, lag.max=max.lag))

#US model is not tested due to heteroscedastic error structure

# Generate plot of impulse response functions
par(mfrow=c(1,1), font.lab = 2)
plot(seq(0,max.lag,1),IRF_EU, type="l",lty=1, xlab="lag", cex.lab=1.5, cex.axis=1.5, ylab="Impulse Response Function")
lines(seq(0,max.lag,1),IRF_CN, type="l",lty=2, xlab="lag")
legend(1,legend=c("EU ARIMA (0,2,1)", "China ARIMA (1,1,0)"),lty=1:2, cex=1.2)

# 9. Conduct forecasting comparison on test set
# Set parameters
level = c(30, 60, 90)
h <- 13 # equals length of forecasting time period (Hyndman & Athanasopoulos, 2021)

# EU - Generate forecasts for test set
EU_forecast_baseline <- naive(EU_train_set, h=h, lambda=0)
EU_forecast_ARIMA <- forecast(EU_train_set, model=Arima(EU_train_set, order = c(0,2,1), lambda=0), h=h, level=level)
EU_forecast_ARIMA_residuals <- EU_forecast_ARIMA$fitted - EU_train_set

# EU - Create plot including main model, baseline model, and actuals
autoplot(window(log(EU), start=train_start_year))+
  autolayer(EU_forecast_baseline, series = "Naïve", PI=FALSE) +
  autolayer(EU_forecast_ARIMA, series = "ARIMA (0,2,1)", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast Model")) + 
  theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) +
  xlab("Time") + ylab("Log of EU CO2 Emissions (kt)")

# EU - Compute evaluation metrics and test statistics for model comparison
accuracy(EU_forecast_baseline, EU_test_set)
accuracy(EU_forecast_ARIMA, EU_test_set)
dm.test(residuals(rwf(EU_train_set), h=h, lambda=0), EU_forecast_ARIMA_residuals, h=h, alternative="greater")


# EU - Prediction: One-step ahead and 2030
EU_prediction_ARIMA <- forecast(log(EU), model=Arima(EU_train_set, order = c(0,2,1)), h=14, level=level) # Use model fitted on training data to predict future values (after test set period)
autoplot(EU_prediction_ARIMA, series = "ARIMA (0,2,1))") + # Create plot with prediction intervals
  theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) +
  xlab("Time") + ylab("Log of EU CO2 Emissions (kt)")
EU_prediction_ARIMA # log values
exp(EU_prediction_ARIMA$fitted) # back-transformed point forecasts
exp(EU_prediction_ARIMA$lower) # back-transformed lower prediction intervals
exp(EU_prediction_ARIMA$upper) # back-transformed upper prediction intervals

# China - Generate forecasts for test set
CN_forecast_baseline <- rwf(CN_train_set, h=h, lambda=0)
CN_forecast_ARIMA <- forecast(Arima(CN_train_set, order = c(1,1,0),lambda=0), h=h, level=level, bootstrap =TRUE) #due to non-normal errors, bootstrapping is used for the prediction intervals
CN_forecast_ARIMA_residuals <- CN_forecast_ARIMA$fitted - CN_train_set

# China - Create plot including main model, baseline model, and actuals
autoplot(window(log(CN), start=train_start_year)) +
  autolayer(CN_forecast_baseline, series = "Naïve", PI=FALSE) +
  autolayer(CN_forecast_ARIMA, series = "ARIMA (1,1,0))", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast Model")) + 
  theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) +
  xlab("Time") + ylab("Log of Chinese CO2 Emissions (kt)")

# China - Compute evaluation metrics and test statistics for model comparison
accuracy(CN_forecast_baseline, CN_test_set)
accuracy(CN_forecast_ARIMA, CN_test_set)
dm.test(residuals(rwf(CN_train_set, h=h, lambda=0)), CN_forecast_ARIMA_residuals, h=14, alternative="greater")

# China - Prediction: One-step ahead and 2030
CN_prediction_ARIMA <- forecast(log(CN), model=Arima(CN_train_set, order = c(1,1,0)), h=14, level=level)
autoplot(CN_forecast_ARIMA, series = "ARIMA (1,1,0)") + # Create plot with prediction intervals
  theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) +
  xlab("Time") + ylab("Log of China CO2 Emissions (kt)")
CN_prediction_ARIMA #log values
exp(CN_prediction_ARIMA$fitted) # back-transformed point forecast
exp(CN_prediction_ARIMA$lower) # back-transformed lower prediction interval
exp(CN_prediction_ARIMA$upper) # back-transformed upper prediction interval