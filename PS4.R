getwd
setwd("/Users/Meilis/Desktop/PS4")
#install.packages("urca")
#install.packages("forecast")
#install.packages('forecast', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages("rugarch")
#install.packages("expm")
#install.packages("fpp")
#install.packages("vars")
#install.packages("cointReg")
#install.packages("strucchange")
library(expm)
library(readxl)
library(urca)
library(forecast)
library(rugarch)
library(lmtest)
library(ggplot2)
library(dplyr)
library(fpp)
library(vars)
library(cointReg)
library(strucchange)

df.PS <- read_excel("PS.xls")
#nrow(df.PS)

############################################################
#Working with Euro exchange rate
############################################################

df.PS$Euro_log=log(df.PS$Euro) #add a columng of logs
df.PS$Euro_log_diff <- c(1, diff(df.PS$Euro_log))


#first 50 autocorrelations
acf(df.PS$Euro_log, lag.max = 50) #not decreasing - clearly non-stationary
acf(diff(df.PS$Euro_log), lag.max = 50) #looks more like stationary


#ADF test
# adf test: nodiff
adf0.1 <- ur.df(df.PS$Euro_log, type = "none", selectlags = c("BIC")) # urca package
summary(adf0.1)
adf0.2 <- ur.df(df.PS$Euro_log, type = "drift", selectlags = c("BIC"))
summary(adf0.2)
adf0.3 <- ur.df(df.PS$Euro_log, type = "trend", selectlags = c("BIC"))
summary(adf0.3)

# adf test: 1 diff
adf1.1 <- ur.df(diff(df.PS$Euro_log, 1), type = "none", selectlags = c("BIC"))
summary(adf1.1)
adf1.2 <- ur.df(diff(df.PS$Euro_log, 1), type = "drift", selectlags = c("BIC"))
summary(adf1.2)
adf1.3 <- ur.df(diff(df.PS$Euro_log, 1), type = "trend", selectlags = c("BIC"))
summary(adf1.3)

# PP test (H_0 - unit root)
PP.test(df.PS$Euro_log, lshort = TRUE)# non-stationary
PP.test(df.PS$Euro_log, lshort = FALSE)# non-stationary

PP.test(diff(df.PS$Euro_log), lshort = TRUE)# non-stationary
PP.test(diff(df.PS$Euro_log), lshort = FALSE)# non-stationary

# Ljung-Box test (H_0 - independence)
Box.test(diff(df.PS$Euro_log), lag = 20, fitdf = 0, type = "Lj")
Box.test(diff(df.PS$Euro_log), lag = 40, fitdf = 0, type = "Lj")
Box.test(diff(df.PS$Euro_log), lag = 60, fitdf = 0, type = "Lj")

# Let's find the best mean model
model.arima <- auto.arima(df.PS$Euro_log_diff[2:3518], stepwise = FALSE, approximation = FALSE) #можно поиграться с stepwise, approx
model.arima1 <- auto.arima(df.PS$Euro_log_diff[2:3518], stepwise = FALSE, approximation = FALSE) #можно поиграться
summary(model.arima)

#model.arima <- auto.arima(df.PS$Euro_log_diff[2:3518], ic = 'bic')
#summary(model.arima)

options(repr.plot.width=8, repr.plot.height=3)
acf(model.arima$residuals^2)
pacf(model.arima$residuals^2)

# We have some ARCH effect. Let's check it
Box.test(model.arima$residuals^2, type = "Ljung-Box")

# ARIMA + GARCH
garch.spec <- ugarchspec(variance.model = list(model = "sGARCH", # can be different functional form
                                               garchOrder = c(1, 1)), # can be different GARCH order
                         mean.model = list(armaOrder = c(2, 3)), # can be different ARMA order
                         distribution.model = "norm") # can be different distribution 

garch.model <- ugarchfit(spec = garch.spec, data = df.PS$Euro_log_diff[2:3518])
garch.model


############################################################
#Working with S&P500
############################################################

df.PS$SP500_log=log(df.PS$SP500) #add a columng of logs
df.PS$SP500_log_diff <- c(1, diff(df.PS$SP500_log))


#first 50 autocorrelations
acf(df.PS$SP500_log, lag.max = 50) #not decreasing - clearly non-stationary
acf(diff(df.PS$SP500_log), lag.max = 50) #looks more like stationary

#ADF test
# adf test: nodiff
adf0.1 <- ur.df(df.PS$SP500_log, type = "none", selectlags = c("BIC")) # urca package
summary(adf0.1)
adf0.2 <- ur.df(df.PS$SP500_log, type = "drift", selectlags = c("BIC"))
summary(adf0.2)
adf0.3 <- ur.df(df.PS$SP500_log, type = "trend", selectlags = c("BIC"))
summary(adf0.3)

# adf test: 1 diff
adf1.1 <- ur.df(diff(df.PS$SP500_log, 1), type = "none", selectlags = c("BIC"))
summary(adf1.1)
adf1.2 <- ur.df(diff(df.PS$SP500_log, 1), type = "drift", selectlags = c("BIC"))
summary(adf1.2)
adf1.3 <- ur.df(diff(df.PS$SP500_log, 1), type = "trend", selectlags = c("BIC"))
summary(adf1.3)

# PP test (H_0 - unit root)
PP.test(df.PS$SP500_log, lshort = TRUE)# non-stationary
PP.test(df.PS$SP500_log, lshort = FALSE)# non-stationary

PP.test(diff(df.PS$SP500_log), lshort = TRUE)# non-stationary
PP.test(diff(df.PS$SP500_log), lshort = FALSE)# non-stationary

# Ljung-Box test (H_0 - independence)
Box.test(diff(df.PS$SP500_log), lag = 20, fitdf = 0, type = "Lj")
Box.test(diff(df.PS$SP500_log), lag = 40, fitdf = 0, type = "Lj")
Box.test(diff(df.PS$SP500_log), lag = 60, fitdf = 0, type = "Lj")

# Let's find the best mean model
model.arima <- auto.arima(df.PS$SP500_log_diff[2:3518], stepwise = FALSE, approximation = FALSE) #можно поиграться с stepwise, approx
model.arima2 <- auto.arima(df.PS$SP500_log_diff[2:3518], stepwise = FALSE, approximation = FALSE) 
summary(model.arima)

#model.arima <- auto.arima(df.PS$Euro_log_diff[2:3518], ic = 'bic')
#summary(model.arima)

options(repr.plot.width=8, repr.plot.height=3)
acf(model.arima$residuals^2)
pacf(model.arima$residuals^2)

# We have some ARCH effect. Let's check it
Box.test(model.arima$residuals^2, type = "Ljung-Box")

# ARIMA + GARCH
garch.spec <- ugarchspec(variance.model = list(model = "sGARCH", # can be different functional form
                                               garchOrder = c(1, 1)), # can be different GARCH order
                         mean.model = list(armaOrder = c(0, 5)), # can be different ARMA order
                         distribution.model = "norm") # can be different distribution 

garch.model <- ugarchfit(spec = garch.spec, data = df.PS$Euro_log_diff[2:3518])
garch.model

#par(mfrow=c(2,1))
#plot(garch.model, which=2)
#plot(garch.model, which=1)
#par(mfrow=c(1,1))

############################################################
#Test for a break. Unknown break date.
############################################################

############################################################
# 1. Logs of exchange rate
############################################################

bp_ts1 <- breakpoints(ts(df.PS$Euro_log) ~ 1)
summary(bp_ts1)
bp_ts1
plot(bp_ts1)
plot(ts(df.PS$Euro_log))
lines(bp_ts1)


#VARINCE
m1 <- gefp(ts(model.arima1$residuals^2) ~ 1, fit = glm, vcov = meatHAC, sandwich = FALSE)
plot(m1, aggregate = FALSE)

############################################################
# 1. Logs of SP500
############################################################

bp_ts2 <- breakpoints(ts(df.PS$SP500_log) ~ 1)
summary(bp_ts2)
bp_ts2
plot(bp_ts2)
plot(ts(df.PS$SP500_log))
lines(bp_ts2)

#VARINCE
m2 <- gefp(ts(model.arima2$residuals^2) ~ 1, fit = glm, vcov = meatHAC, sandwich = FALSE)
plot(m2, aggregate = FALSE)

############################################################
#Granger Causality
############################################################
grangertest(df.PS$Euro_log, df.PS$SP500_log, order=5)
grangertest(df.PS$SP500_log, df.PS$Euro_log, order=5)



############################################################
#Cointegration
############################################################

#Checking cointegration: expect no cointegration. Refer to tests for unit roots.
par(mfrow=c(2,1))
plot(ts(df.PS$SP500_log), which=1)
plot(ts(df.PS$Euro_log), which=2)

df.PS_2 <- data.frame(df.PS$Euro_log, stringsAsFactors = TRUE)
df.PS_2$SP500_log <- data.frame(df.PS$SP500_log, stringsAsFactors = TRUE)#creating new data file with only logs
colnames(df.PS_2) <- c("Euro_log","SP500_log")
############################################################
#VAR specification
############################################################
#autoplot(ts(df.PS_2), facets = TRUE) + ylab('')

# select p order
VARselect(df.PS_2, lag.max=7, type = "const")
VARselect(df.PS_2, lag.max = 7, type = "const")$selection
# estimated 3 lags by AIC

# estimate
model <- VAR(df.PS_2, p = 3, type = "const")
summary(model)

############################################################
#VECM specification
############################################################


#Perform maximum eigenvalue test
coint_h11 <- ca.jo(df.PS_2, type = "eigen", K = 5, spec = "transitory")
summary(coint_h11) #r=0 => no cointegration

coint_h12 <- ca.jo(df.PS_2, type = "eigen", K = 5, spec = "longrun")
summary(coint_h12) #r=0 => no cointegration

# eigenvectors and cointegration relations
coint_h12@V

# error correction term (through VECM)
ec <- df.PS_2[, 'SP500_log'] + coint_h12@V[2,1]*df.PS_2[, 'Euro_log']
#ec <- coint_h12@V[2,1]*df.PS_2[, 'SP500_log'] + df.PS_2[, 'Euro_log']
autoplot(ts(ec)) + ggtitle("Error correction term")
tsdisplay(ec)

############################################################
#Estimate dynamic OLS
############################################################
DOLS<-cointRegD(df.PS_2$Euro_log, df.PS_2$SP500_log, n.lead = 5, n.lags = 5)
#summary(DOLS)
DOLS


#Test for significance of 
