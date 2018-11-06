# load data ##########
datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

load(paste0(datadir,"lg2.list.rda"))

# function to grab time series data of log growth
grabts <- function(x){
  ts(x$Patrons, frequency = 12,
     start = c(2016, 07))
}

# fetch data in list format
tsdata <- lapply(lg2.list, FUN = grabts)
n <- length(tsdata[[1]])

# dummy arima data ###########
testdata <- tsdata[[1]] + 1.2*(1:n)

# Stationarity Tests
# don't know how to test confidence interval
# anyway need to fit the same model to all
# and can use auto.arima

## acf object

(acf.test <- acf(testdata, plot = FALSE))

(pacf.test <- pacf(testdata, plot = FALSE))

# confidence interval for acf and pacf
ci <- qnorm((1+0.95)/2)/sqrt(acf.test$n.used)

(lagacf.significant <- acf.test$lag[abs(acf.test$acf) > ci] * 12)

(lagpacf.significant <- pacf.test$lag[abs(pacf.test$acf) > ci]* 12)


# test model objects
(ar.object <- ar(testdata))

# get ar order selected
ar.object$order

# doesn't give any ma terms. just fits smoother
# hence use auto.arima

(ma.object <- ma(tsdata[[1]], order = 3))


# arima model
# difficult to check significance of terms
(arima.object <- arima(tsdata[[1]], order = c(2, 0, 2)))

library(forecast)

(auto.object <- auto.arima(testdata))

(armalags <- auto.object$arma)

(arlags <- armalags[1])

(malags <- armalags[3])






