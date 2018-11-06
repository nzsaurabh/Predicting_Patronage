# predictions from DNEST4 for linear trend

# data and specifications ################

# load data 

# data directory
datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

# test data
# it has patrons for time t onwards
load(paste0(datadir,"testdnest4.rda"))

testdnest4$Amanda_1

# load predictions
load("dnest4quad.preds.rda")

# rename for convenience
dnest4ar1.preds <- dnest4quad.preds

str(dnest4ar1.preds[[1]])

# yhat are in dnest4ar1.preds$growth.pred
dnest4ar1.preds[[1]]$pred.mean

# CI is in 
dnest4ar1.preds[[1]]$pred.quantiles

dnest4ar1.preds[[1]]$pred.quantiles[2, ]

str(dnest4ar1.preds[[1]]$pred.quantiles)

# predictions ##########################

# yhat use mean
# if it doesn't work use median 
# preds.fun <- function(x){x$pred.quantiles[ 2, ]}
# mean
preds.fun <- function(x){x$pred.mean}

preds <- lapply(dnest4ar1.preds, FUN = preds.fun)

# CI low from first row

low95 <- lapply(dnest4ar1.preds, FUN = function(x){
  x$pred.quantiles[1,]
})

# CI upper from row 3
up95 <- lapply(dnest4ar1.preds, FUN = function(x){
  x$pred.quantiles[3,]
})


# create a matrix
yhat.dnest4 <- do.call(rbind, preds)
dim(yhat.dnest4)

yhat.dnest4[1,]

low95.dnest4 <- do.call(rbind, low95)
dim(low95.dnest4)

low95.dnest4[1, ]

up95.dnest4 <- do.call(rbind, up95)
dim(up95.dnest4)

up95.dnest4[1, ]

# confidence interval #########
# wrt to Patrons at time t+1

ci_width1 <- (up95.dnest4[,1] - low95.dnest4[,1])/yhat.dnest4[,1]

length(ci_width1)

# histogram CI width for t+1 as proportion of Patrons t+1
# x axis on log scale for outliers
# warnings can be ignored

hist(ci_width1, breaks = 20, log = "x")

# histogram CI width for t+4 as proportion of Patrons t+4

ci_width4 <- (up95.dnest4[,4] - low95.dnest4[,4])/ yhat.dnest4[,4]
length(ci_width4)

hist(ci_width4, breaks = 20, log = "x")

# no major outliers
summary(ci_width1)

tail(sort(ci_width1))

summary(ci_width4)

tail(sort(ci_width4))

# n for labels
nci1 <- length(ci_width1)
nci4 <- length(ci_width4)


y.patrons <- do.call(rbind, testdnest4)
# leave data for april
dim(y.patrons[, -1])
# predict for 4 months
dim(yhat.dnest4[,1:4])

# RMSE ###############
# RSS as a proportion of squared actual values
# not renaming the vectors at this stage for

n <- nrow(y.patrons)
rssprop.arima <- r2.arima <- rmse.arima <- vector(length = n)

for (i in 1:n){
  
  actual <- y.patrons[ i , -1]
  
  predicted <- yhat.dnest4[ i ,1:4]
  
  rss <- sum( (actual - predicted )^2 )
  
  rmse <- sqrt(rss/4)
  
  y2 <- sum( actual^2 )
  
  tss <- sum((actual - mean(actual))^2 )
  
  rssprop.arima[i] <-  rss/y2
  
  r2.arima[i] <- (tss - rss)/tss
  
  rmse.arima[i] <- rmse
}

hist(1 - rssprop.arima, breaks = 20)

hist(r2.arima, breaks = 20)

## save objects

forecasts.quad.dnest4 <- list(rssprop.quad = rssprop.arima, 
                        rmse.quad = rmse.arima,
                        r2.quad = r2.arima,
                        ci_width1.quad = ci_width1,
                        ci_width4.quad = ci_width4)

save(forecasts.quad.dnest4 , file = "forecasts.quad.dnest4.rda")

# RMSE latex plots ##########
# produced perfect plots on 30th Sep 18
# no dots in latex filename

load("forecasts.quad.dnest4.rda")

rssprop.quad <- forecasts.quad.dnest4$rssprop.quad
rmse.quad <- forecasts.quad.dnest4$rmse.quad
ci_width1.quad <- forecasts.quad.dnest4$ci_width1.quad
ci_width4.quad <- forecasts.quad.dnest4$ci_width4.quad


pdf(file = "rmse_quad_dnest4.pdf", height = 10, paper = "a4")

par(mfrow = c(4,1))


# RSS as prop of y^2
hist( (1 - rssprop.quad)*100, breaks = 30,
      main = "DNest4 - Forecast Accuracy for Quadratic Trend", 
      xlab = "Accuracy (% similarity to data)",
      sub = "period = 4 months" )

# log mean RMSE for 4 months of data
hist(log(rmse.quad), breaks = 30,
     main = "Forecast Error",
     xlab = "Log Root Mean Square Error (RMSE)" )

# CI width for t+1 as proportion of yhat Patrons t+1
hist(ci_width1.quad*100, breaks = 30,
     main = expression(paste("Confidence Interval (", CI[t+1] , ")")),
     xlab = expression(paste("Width of CI as % of ", hat(y) )))

# CI width for t+4 as proportion of yhat Patrons t+4
hist(ci_width4.quad*100, breaks = 30,
     main = expression(paste("Confidence Interval (", CI[t+4] , ")")),
     xlab = expression(paste("Width of CI as % of ", hat(y) )))

dev.off()




# RMSE Summary ###########

library(xtable)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")


load("forecasts.quad.dnest4.rda")

rssprop.quad <- forecasts.quad.dnest4$rssprop.quad
rmse.quad <- forecasts.quad.dnest4$rmse.quad
ci_width1.quad <- forecasts.quad.dnest4$ci_width1.quad
ci_width4.quad <- forecasts.quad.dnest4$ci_width4.quad

# quantiles
myprobs <- c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1)

Accuracy <- 100 * as.vector(quantile(1-rssprop.quad, myprobs))
RMSE <- as.vector(quantile(rmse.quad, myprobs))
CI_t1 <- 100 * as.vector(quantile(ci_width1.quad, myprobs))
CI_t4 <- 100 * as.vector(quantile(ci_width4.quad, myprobs))

names1 <- names(quantile(ci_width4.quad, myprobs))

summary.df <- cbind.data.frame(Accuracy, RMSE, CI_t1, CI_t4)
rownames(summary.df) <- names1

xtable(summary.df)

# Ratio quad vs arima trend ########

load("forecasts.quad.dnest4.rda")

load("forecasts.linear.rda")

n <- length(forecasts.quad.dnest4)

ratio.quad.arima <- vector("list", n)

for(i in 1:n){
  if(i == 1){
    ratio.quad.arima[[i]] <- (1 - forecasts.quad.dnest4[[i]]) / (1 - forecasts.arima[[i]])
    
  }else{
    ratio.quad.arima[[i]] <- forecasts.quad.dnest4[[i]] / forecasts.arima[[i]]
  }
}

names(ratio.quad.arima) <- names(forecasts.quad.dnest4)

save(ratio.quad.arima, file = "ratio.quad.arima.rda")

## summary 

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

summary.df <- quantile.ratio.fun(ratio.quad.arima)

xtable(summary.df)


# Ratio quad vs linear ########
# Both dnest4

load("forecasts.quad.dnest4.rda")

load("forecasts.linear.dnest4.rda")

n <- length(forecasts.quad.dnest4)

ratio.quad.linear <- vector("list", n)

for(i in 1:n){
  if(i == 1){
    ratio.quad.linear[[i]] <- (1 - forecasts.quad.dnest4[[i]]) / (1 - forecasts.linear.dnest4[[i]])
    
  }else{
    ratio.quad.linear[[i]] <- forecasts.quad.dnest4[[i]] / forecasts.linear.dnest4[[i]]
  }
}

names(ratio.quad.linear) <- names(forecasts.quad.dnest4)

save(ratio.quad.linear, file = "ratio.quad.linear.rda")

# use ratio function from previous section

summary.df <- quantile.ratio.fun(ratio.quad.linear)

xtable(summary.df)

## end ##########