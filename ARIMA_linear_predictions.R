# ARIMA predictions

# load data #####################################

# data directory
datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

# test data
# it has patrons for time t onwards
load(paste0(datadir,"testdnest4.rda"))

testdnest4$Amanda_1

# actual growth not required
# load(paste0(datadir,"actualgrowth.rda"))
# it has monthly growth for time t+1


# train data and predictions
load("tsanalysis_lineartrend.rda")

# predictions are in $tspred.df
# they are in terms of monthly growth
# need to put in terms of Patrons
# data for time t is in testdnest4

tsanalysis[[1]]$tspred.df

tspred.df <- lapply(tsanalysis, FUN = function(x){
                  x$tspred.df
                  })

preds <- lapply(tsanalysis, FUN = function(x){
                x$tspred.df$tspred
                })

low95 <- lapply(tsanalysis, FUN = function(x){
                   x$tspred.df$low95
                  })

up95 <- lapply(tsanalysis, FUN = function(x){
                  x$tspred.df$up95
                })


# predictions ####################
# no. of patrons
# changed to use data as it is and
# return the same shape as done for log growth

preds.fun <- function(x = testdnest4, y = preds){
  
  n <- length(y)
  pred.patrons <- vector(length = n)
  
  for(i in 1:n){
  pred.patrons[i] <- y[i]
  }
  return(pred.patrons)
}
  
# creates a matrix
yhat.lgpatrons <- t(mapply(FUN = preds.fun, x = testdnest4, y = preds))
dim(yhat.lgpatrons)

yhat.lgpatrons[4, ]

low95.lgpatrons <- t(mapply(FUN = preds.fun, x = testdnest4, y = low95))
dim(low95.lgpatrons)

low95.lgpatrons[4, ]

up95.lgpatrons <- t(mapply(FUN = preds.fun, x = testdnest4, y = up95))
dim(up95.lgpatrons)

up95.lgpatrons[4, ]

# confidence interval #########
# wrt to Patrons at time t+1

ci_width1 <- (up95.lgpatrons[,1] - low95.lgpatrons[,1])/yhat.lgpatrons[,1]

length(ci_width1)

# histogram CI width for t+1 as proportion of Patrons t+1
hist(ci_width1)

# histogram CI width for t+4 as proportion of Patrons t+4

ci_width4 <- (up95.lgpatrons[,4] - low95.lgpatrons[,4])/yhat.lgpatrons[,4]
length(ci_width4)

hist(ci_width4, breaks = 20)

y.patrons <- do.call(rbind, testdnest4)
# leave data for april
dim(y.patrons[, -1])
# predict for 4 months
dim(yhat.lgpatrons[,1:4])

n <- nrow(y.patrons)

# log mean rss #################
# used for latex 
# in terms of number of patrons

mrss.arima <- vector(length = n)

for (i in 1:n){
  mrss.arima[i] <-  sum((y.patrons[ i , -1] - 
                       yhat.lgpatrons[ i ,1:4])^2)/4
            }

hist(mrss.arima, breaks = 20)

# RSS is skewed so use log(mean RSS) for all models

hist(log(mrss.arima))

# RMSE ###############
# RSS as a proportion of squared actual values

n <- nrow(y.patrons)
rssprop.arima <- r2.arima <- rmse.arima <- vector(length = n)

for (i in 1:n){
  
  actual <- y.patrons[ i , -1]
  
  predicted <- yhat.lgpatrons[ i ,1:4]
  
  rss <- sum( (actual - predicted )^2 )
  
  rmse <- sqrt(rss/4)
  
  y2 <- sum( actual^2 )
  
  tss <- sum((actual - mean(actual))^2 )
  
  rssprop.arima[i] <-  rss/y2
  
  r2.arima[i] <- (tss - rss)/tss
  
  rmse.arima[i] <- rmse
}

hist(1 - rssprop.arima, breaks = 20)

## save objects

forecasts.arima <- list(rssprop.arima = rssprop.arima, 
                        rmse.arima = rmse.arima,
                     r2.arima = r2.arima,
                     ci_width1.arima = ci_width1,
                     ci_width4.arima = ci_width4)

save(forecasts.arima , file = "forecasts.linear.rda")

# RMSE latex plots ##########
# produced perfect plots on 30th Sep 18
# no dots in latex filename

load("forecasts.linear.rda")

rssprop.arima <- forecasts.arima$rssprop.arima
rmse.arima <- forecasts.arima$rmse.arima
ci_width1 <- forecasts.arima$ci_width1.arima
ci_width4 <- forecasts.arima$ci_width4.arima

  
pdf(file = "rmse_arima_trend.pdf", height = 10, paper = "a4")

par(mfrow = c(4,1))


# RSS as prop of y^2
hist( (1 - rssprop.arima)*100, breaks = 30,
      main = "auto.arima - Forecast Accuracy", 
      xlab = "Accuracy (% similarity to data)",
      sub = "period = 4 months" )

# log mean RSS for 4 months of data
hist(log(rmse.arima), breaks = 30,
     main = "Forecast Error",
     xlab = "Log Root Mean Square Error (RMSE)" )

# CI width for t+1 as proportion of yhat Patrons t+1
hist(ci_width1*100, breaks = 30,
     main = expression(paste("Confidence Interval (", CI[t+1] , ")")),
     xlab = expression(paste("Width of CI as % of ", hat(y) )))

# CI width for t+4 as proportion of yhat Patrons t+4
hist(ci_width4*100, breaks = 30,
     main = expression(paste("Confidence Interval (", CI[t+4] , ")")),
     xlab = expression(paste("Width of CI as % of ", hat(y) )))

dev.off()




# RMSE Summary ###########

library(xtable)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")


load("forecasts.linear.rda")

rssprop.arima <- forecasts.arima$rssprop.arima
rmse.arima <- forecasts.arima$rmse.arima
ci_width1 <- forecasts.arima$ci_width1.arima
ci_width4 <- forecasts.arima$ci_width4.arima

# quantiles
myprobs <- c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1)

Accuracy <- 100 * as.vector(quantile(1-rssprop.arima, myprobs))
RMSE <- as.vector(quantile(rmse.arima, myprobs))
CI_t1 <- 100 * as.vector(quantile(ci_width1, myprobs))
CI_t4 <- 100 * as.vector(quantile(ci_width4, myprobs))

names1 <- names(quantile(ci_width4, myprobs))

summary.df <- cbind.data.frame(Accuracy, RMSE, CI_t1, CI_t4)
rownames(summary.df) <- names1

xtable(summary.df)

summary.arimalinear <- summary.df

save(summary.arimalinear, file = "summary.arimalinear.rda")

# Ratio RMSE #########

load("forecasts.linear.rda")

load("forecasts.linear.dnest4.rda")

n <- length(forecasts.arima)

# not changing name for convenience
ratio.lg.arima <- vector("list", n)
forecasts.lg <- forecasts.linear.dnest4

for(i in 1:n){
  if(i == 1){
  ratio.lg.arima[[i]] <- (1 - forecasts.lg[[i]]) / (1 - forecasts.arima[[i]])
  
  }else{
    ratio.lg.arima[[i]] <- forecasts.lg[[i]] / forecasts.arima[[i]]
  }
}

names(ratio.lg.arima) <- names(forecasts.lg)

# Ratio Summary ############

# quantiles
myprobs <- c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1)

# function for ratios only
quantile.ratio.fun <- function(x.forecasts){
  
  Accuracy <- as.vector(quantile(x.forecasts[[1]], myprobs))
  RMSE <- as.vector(quantile(x.forecasts[[2]], myprobs))
  CI_t1 <- as.vector(quantile(x.forecasts[[4]], myprobs))
  CI_t4 <- as.vector(quantile(x.forecasts[[5]], myprobs))
  
  names1 <- names(quantile(x.forecasts[[1]], myprobs))
  
  summary.df <- cbind.data.frame(Accuracy, RMSE, CI_t1, CI_t4)
  rownames(summary.df) <- names1
  
  return(summary.df)
  
  }

summary.df <- quantile.ratio.fun(ratio.lg.arima)

xtable(summary.df)


# Ratio latex plots ##########
# produced perfect plots on 30th Sep 18
# no dots in latex filename

pdf(file = "ratio_lg_arima.pdf", height = 10, paper = "a4")

par(mfrow = c(4,1))


# RSS as prop of y^2
hist( ratio.lg.arima[[1]], breaks = 20,
      main = "Forecast Accuracy - Log Growth vs auto.arima", 
      xlab = "Ratio of % data explained" )

# log mean RSS for 4 months of data
hist(ratio.lg.arima[[2]],
     main = "Forecast Error",
     xlab = "Ratio of Root Mean Square Error" )

# 3rd item in list not to be used. It is R2

# CI width for t+1 as proportion of yhat Patrons t+1
hist(ratio.lg.arima[[4]], breaks = 20,
     main = "Confidence Interval (CI) for t+1",
     xlab = "Ratio of CI Width" )

# CI width for t+4 as proportion of yhat Patrons t+4
hist(ratio.lg.arima[[5]], breaks = 20,
     main = "Confidence Interval (CI) for t+4",
     xlab = "Ratio of CI Width" )

dev.off()





# THE END ########
