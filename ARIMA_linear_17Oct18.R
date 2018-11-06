# source data #####

datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

load(paste0(datadir,"traindnest4.rda"))

library(forecast)

# function to grab time series data of log growth
grabts <- function(x){
  ts(x, frequency = 12,
     start = c(2016, 05))
  }

# fetch data in list format
tsdata <- lapply(traindnest4, FUN = grabts)

save(tsdata, file = paste0(datadir,"tsdata_lineartrend.rda"))



# create function for time series analysis

tslinear.fun <- function(x){
  
  # create list object for output
  
  mylist.names <- c("acf.lags", "pacf.lags", 
                    "arma.terms", "ar.p", "ma.q", 
                    "tspred.df",
                    "width.percent", "tsmodel")
  
  ts.output <- vector("list", length = length(mylist.names))
  names(ts.output) <- mylist.names
  
  rm(mylist.names)
  

# Stationarity tests

  ts.output$acf.object <- acf(x, plot = FALSE)

  ts.output$pacf.object <- pacf(x, plot = FALSE)

# confidence interval for acf and pacf is the same
ci <- qnorm((1+0.95)/2)/sqrt(ts.output$acf.object$n.used)

ts.output$acf.lags <- 12 * ts.output$acf.object$lag[abs(ts.output$acf.object$acf) > ci]

ts.output$pacf.lags <- 12 * ts.output$pacf.object$lag[abs(ts.output$pacf.object$acf) > ci]

# output auto arima model
ts.output$tsmodel <- auto.arima(x)

# significant lags
# arma gives the number of AR, MA coefficients i.e. p and q 
# seasonal AR and seasonal MA coefficients i.e. P and Q
# period and
# number of non-seasonal and seasonal differences i.e. d and D

# output all terms
ts.output$arma.terms <- ts.output$tsmodel$arma

names(ts.output$arma.terms) <- c("p", "q", "P", "Q", 
                       "period", "d", "D")
# output p and q
ts.output$ar.p <- ts.output$arma.terms[1] 

ts.output$ma.q <- ts.output$arma.terms[2]

# output forecast object

ts.output$tsforecast <- forecast(ts.output$tsmodel, h = 12, level = 95)

# predictions
tspred <- as.vector(ts.output$tsforecast$mean)

low95 <- as.vector(ts.output$tsforecast$lower)

up95 <- as.vector(ts.output$tsforecast$upper)

ci.width <- as.vector(round(abs(up95 - low95), 5))

# output CI as percent of mean
ts.output$width.percent <- round(mean(ci.width) / 
                          (abs(mean(as.vector(tspred)))+0.00001), 4)

# output this data frame
ts.output$tspred.df <- cbind.data.frame(tspred, low95, up95, ci.width)

return(ts.output)
}

#save(tsmodel.fun, file = paste0(datadir,"tsmodel.fun.rda"))


#save(tsmodel.fun, file = "tsmodel.fun.rda")

# apply ts function
tsanalysis <- lapply(tsdata, FUN = tslinear.fun)

# save the analysis
save(tsanalysis, file = "tsanalysis_lineartrend.rda")

# remove
# rm(tsanalysis)


