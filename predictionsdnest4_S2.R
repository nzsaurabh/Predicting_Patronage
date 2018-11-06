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
load("dnest4linear.preds.rda")

# rename for convenience
dnest4ar1.preds <- dnest4linear.preds

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

## save objects

forecasts.linear.dnest4 <- list(rssprop.linear = rssprop.arima, 
                        rmse.linear = rmse.arima,
                        r2.linear = r2.arima,
                        ci_width1.linear = ci_width1,
                        ci_width4.linear = ci_width4)

save(forecasts.linear.dnest4 , file = "forecasts.linear.dnest4.rda")

# RMSE latex plots ##########
# produced perfect plots on 30th Sep 18
# no dots in latex filename

load("forecasts.linear.dnest4.rda")

rssprop.linear <- forecasts.linear.dnest4$rssprop.linear
rmse.linear <- forecasts.linear.dnest4$rmse.linear
ci_width1.linear <- forecasts.linear.dnest4$ci_width1.linear
ci_width4.linear <- forecasts.linear.dnest4$ci_width4.linear


pdf(file = "rmse_linear_dnest4.pdf", height = 10, paper = "a4")

par(mfrow = c(4,1))


# RSS as prop of y^2
hist( (1 - rssprop.linear)*100, breaks = 30,
      main = "DNest4 - Forecast Accuracy", 
      xlab = "Accuracy (% similarity to data)",
      sub = "period = 4 months" )

# log mean RMSE for 4 months of data
hist(log(rmse.linear), breaks = 30,
     main = "Forecast Error",
     xlab = "Log Root Mean Square Error (RMSE)" )

# CI width for t+1 as proportion of yhat Patrons t+1
hist(ci_width1.linear*100, breaks = 30,
     main = expression(paste("Confidence Interval (", CI[t+1] , ")")),
     xlab = expression(paste("Width of CI as % of ", hat(y) )))

# CI width for t+4 as proportion of yhat Patrons t+4
hist(ci_width4.linear*100, breaks = 30,
     main = expression(paste("Confidence Interval (", CI[t+4] , ")")),
     xlab = expression(paste("Width of CI as % of ", hat(y) )))

dev.off()




# RMSE Summary ###########

library(xtable)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")


load("forecasts.linear.dnest4.rda")

rssprop.linear <- forecasts.linear.dnest4$rssprop.linear
rmse.linear <- forecasts.linear.dnest4$rmse.linear
ci_width1.linear <- forecasts.linear.dnest4$ci_width1.linear
ci_width4.linear <- forecasts.linear.dnest4$ci_width4.linear

# quantiles
myprobs <- c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1)

Accuracy <- 100 * as.vector(quantile(1-rssprop.linear, myprobs))
RMSE <- as.vector(quantile(rmse.linear, myprobs))
CI_t1 <- 100 * as.vector(quantile(ci_width1.linear, myprobs))
CI_t4 <- 100 * as.vector(quantile(ci_width4.linear, myprobs))

names1 <- names(quantile(ci_width4.linear, myprobs))

summary.df <- cbind.data.frame(Accuracy, RMSE, CI_t1, CI_t4)
rownames(summary.df) <- names1

xtable(summary.df)

# summary

names1 <- names(summary(rmse.linear))

summary.linear.dnest4 <- cbind.data.frame(Accuracy, RMSE, CI_t1, CI_t4)
rownames(summary.linear.dnest4) <- names1

xtable(summary.linear.dnest4)





# log mean rss #################
# used for latex 
# in terms of number of patrons

y.patrons <- do.call(rbind, testdnest4)
# leave data for april
dim(y.patrons[, -1])

# predictions for 4 months
dim(yhat.dnest4[,1:4])

n <- nrow(y.patrons)
mrss.dnest4 <- vector(length = n)

for (i in 1:n){
  mrss.dnest4[i] <-  sum((y.patrons[ i , -1] - 
                           yhat.dnest4[ i ,1:4])^2)/4
}

# histogram working best on log10 scale
hist(log10(mrss.dnest4), breaks = "fd")

# buckets <- c(0, 1e3, 1e4, 1e5, 1e6, 1e7)

# myhist <- hist(mrss.dnest4, breaks = buckets)
# bp <- barplot(myhist$counts, 
#              axes = FALSE, space = 0)
# text(bp-0.5, y = -1, labels = format(buckets), xpd = TRUE)



# RSS is skewed so use log(mean RSS) for all models

hist(log(mrss.dnest4))
summary(mrss.dnest4)

# RSS/y ###############
# RSS as a proportion of squared actual values

n <- nrow(y.patrons)
rssprop.dnest4 <- vector(length = n)

for (i in 1:n){
  
  rss <- sum((y.patrons[ i , -1] - 
                yhat.dnest4[ i ,1:4])^2)
  
  y2 <- sum( (y.patrons[ i , -1])^2 )
  
  
  rssprop.dnest4[i] <-  rss/y2
}

hist(1 - rssprop.dnest4, breaks = 40)


# latex plots ##########
# produced perfect plots on 30th Sep 18
# no dots in latex filename

pdf(file = "rss_linear.pdf", height = 10, paper = "a4")

par(mfrow = c(4,1))


# RSS as prop of y^2
hist( (1 - rssprop.dnest4)*100, breaks = 40,
      main = "Linear Trend - Forecast Accuracy using DNEST4", 
      xlab = expression(paste("% data explained = 100*(1 - RSS/" ,
                              {sum(y[i]^2, i)}, ")")),
      
      sub = "period = 4 months" )

# log mean RSS for 4 months of data
hist(log(mrss.dnest4),
     main = "Forecast Error",
     xlab = "Log Mean RSS" )

# CI width for t+1 as proportion of yhat Patrons t+1
hist(ci_width1*100, breaks = 40,
     main = "Confidence Interval (CI) for t+1",
     xlab = "Width of CI as % of y" 
     #sub = paste0("Excludes ", nci1, " outliers")
     )

# CI width for t+4 as proportion of yhat Patrons t+4
hist(ci_width4*100, breaks = 40,
     main = "Confidence Interval (CI) for t+4",
     xlab = "Width of CI as % of y" 
     #sub = paste0("Excludes ", nci4, " outliers") 
     )

dev.off()


## end ##########