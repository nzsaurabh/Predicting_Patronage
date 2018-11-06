# predictions from DNEST4
# log growth is in ypred columns
# hence growth = exp(ypred0) and so on

# data and specifications ################



# load data #####################################

# data directory
datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

# test data
# it has patrons for time t onwards
load(paste0(datadir,"testdnest4.rda"))

testdnest4$Amanda_1

# actual growth not required

# load predictions

load("dnest4ar1.preds.rda")

str(dnest4ar1.preds[[1]])

# yhat are in dnest4ar1.preds$growth.pred
dnest4ar1.preds[[1]]$growth.pred

# CI is in 
dnest4ar1.preds[[1]]$pred.quantiles

str(dnest4ar1.preds[[1]]$pred.quantiles)

# predictions ##########################

# yhat
preds.fun <- function(x){x$growth.pred[1:4]}

preds <- lapply(dnest4ar1.preds, FUN = preds.fun)

# CI low from first row

low95 <- lapply(dnest4ar1.preds, FUN = function(x){
  x$pred.quantiles[1,]
})

# CI upper from row 3
up95 <- lapply(dnest4ar1.preds, FUN = function(x){
  x$pred.quantiles[3,]
})


# no. of patrons
preds.fun <- function(x = testdnest4, y = preds){
  n <- length(y)
  pred.patrons <- vector(length = n)
  pred.patrons[1] <- x[1]*y[1]
  for(i in 2:n){
    pred.patrons[i] <- pred.patrons[i-1] * y[i]
  }
  return(pred.patrons)
}

# creates a matrix
yhat.dnest4 <- t(mapply(FUN = preds.fun, x = testdnest4, y = preds))
dim(yhat.dnest4)

yhat.dnest4[1,]

low95.dnest4 <- t(mapply(FUN = preds.fun, x = testdnest4, y = low95))
dim(low95.dnest4)

low95.dnest4[1, ]

up95.dnest4 <- t(mapply(FUN = preds.fun, x = testdnest4, y = up95))
dim(up95.dnest4)

up95.dnest4[1, ]

# confidence interval #########
# wrt to Patrons at time t+1

ci_width1 <- (up95.dnest4[,1] - low95.dnest4[,1])/yhat.dnest4[,1]

length(ci_width1)

# histogram CI width for t+1 as proportion of Patrons t+1
hist(ci_width1, breaks = 20)

# histogram CI width for t+4 as proportion of Patrons t+4

ci_width4 <- (up95.dnest4[,4] - low95.dnest4[,4])/yhat.dnest4[,4]
length(ci_width4)

hist(ci_width4, breaks = 20)

# need to get rid of outliers for plotting

summary(ci_width1)

tail(sort(ci_width1))

summary(ci_width4)

tail(sort(ci_width4))

# histogram after removing outliers

nci1 <- length(ci_width1[ci_width1 < 5])
nci1 <- length(ci_width1) - nci1

hist(ci_width1[ci_width1 < 5], breaks = 20)

nci4 <- length(ci_width4[ci_width4 < 20])
nci4 <- length(ci_width4) - nci4

hist(ci_width4[ci_width4 < 20], breaks = 20)

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

hist(mrss.dnest4, breaks = 20)

# RSS is skewed so use log(mean RSS) for all models

hist(log(mrss.dnest4))


# RSS/y ###############
# RSS as a proportion of squared actual values

n <- nrow(y.patrons)
rssprop.dnest4 <- r2.dnest4 <- rmse.dnest4 <- vector(length = n)

for (i in 1:n){
  
  actual <- y.patrons[ i , -1]
  
  predicted <- yhat.dnest4[ i ,1:4]
  
  rss <- sum( (actual - predicted )^2 )
  
  rmse <- sqrt(rss/4)
  
  y2 <- sum( actual^2 )
  
  tss <- sum((actual - mean(actual))^2 )
  
  
  rssprop.dnest4[i] <-  rss/y2
  
  r2.dnest4[i] <- (tss - rss)/tss
  
  rmse.dnest4[i] <- rmse
  
}

hist(1 - rssprop.dnest4, breaks = 20)

hist(log(rmse.dnest4), breaks = 20)

plot(rmse.dnest4)

## save objects ###

forecasts.lg <- list(rssprop.dnest4 = rssprop.dnest4, 
                        rmse.dnest4 = rmse.dnest4,
                        r2.dnest4 = r2.dnest4,
                        ci_width1 = ci_width1,
                        ci_width4 = ci_width4)

save(forecasts.lg , file = "forecasts.lg.rda")

load("forecasts.lg.rda")

# RMSE latex plots ##########
# produced perfect plots on 30th Sep 18
# no dots in latex filename


rssprop.dnest4 <- forecasts.lg$rssprop.dnest4
rmse.dnest4 <- forecasts.lg$rmse.dnest4
ci_width1 <- forecasts.lg$ci_width1
ci_width4 <- forecasts.lg$ci_width4


nci1 <- length(ci_width1[ci_width1 < 5])
nci1 <- length(ci_width1) - nci1

hist(ci_width1[ci_width1 < 5], breaks = 20)

nci4 <- length(ci_width4[ci_width4 < 20])
nci4 <- length(ci_width4) - nci4

pdf(file = "rmse_lg.pdf", height = 10, paper = "a4")

par(mfrow = c(4,1))


# RSS as prop of y^2
hist( (1 - rssprop.dnest4)*100, breaks = 30,
      main = "DNEST4 - Forecast Accuracy", 
      xlab = "Accuracy (% similarity to data)",
      sub = "period = 4 months" )

# log mean RSS for 4 months of data
hist(log(rmse.dnest4),  breaks = 30,
     main = "Forecast Error",
     xlab = "Log Root Mean Square Error (RMSE)" )

# CI width for t+1 as proportion of yhat Patrons t+1
hist(ci_width1[ci_width1 < 5]*100,  breaks = 30,
     main = expression(paste("Confidence Interval (", CI[t+1] , ")")),
     xlab = expression(paste("Width of CI as % of ", hat(y) )),
     sub = paste0("Excludes ", nci1, " outliers"))

# CI width for t+4 as proportion of yhat Patrons t+4
hist(ci_width4[ci_width4 < 20]*100,  breaks = 30,
     main = expression(paste("Confidence Interval (", CI[t+4] , ")")),
     xlab = expression(paste("Width of CI as % of ", hat(y) )),
     sub = paste0("Excludes ", nci4, " outliers"))

dev.off()




# RMSE Summary ###########

load("forecasts.lg.rda")


rssprop.dnest4 <- forecasts.lg$rssprop.dnest4
rmse.dnest4 <- forecasts.lg$rmse.dnest4
ci_width1 <- forecasts.lg$ci_width1
ci_width4 <- forecasts.lg$ci_width4

library(xtable)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# quantiles
myprobs <- c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1)
Accuracy <- 100 * as.vector(quantile(1-rssprop.dnest4, myprobs))
RMSE <- as.vector(quantile(rmse.dnest4, myprobs))
CI_t1 <- 100 * as.vector(quantile(ci_width1, myprobs))
CI_t4 <- 100 * as.vector(quantile(ci_width4, myprobs))

summary.df <- cbind.data.frame(Accuracy, RMSE, CI_t1, CI_t4)
names1 <- names(quantile(rmse.dnest4, myprobs))
rownames(summary.df) <- names1

xtable(summary.df)

# summary

Accuracy <- 100 * as.vector(summary(1-rssprop.dnest4))
RMSE <- as.vector(summary(rmse.dnest4))
CI_t1 <- 100 * as.vector(summary(ci_width1))
CI_t4 <- 100 * as.vector(summary(ci_width4))

names1 <- names(summary(rmse.dnest4))

summary.df <- cbind.data.frame(Accuracy, RMSE, CI_t1, CI_t4)
rownames(summary.df) <- names1

xtable(summary.df)


# THE END
# old latex plots ##########
# produced perfect plots on 30th Sep 18
# no dots in latex filename

pdf(file = "rssdnest4.pdf", height = 10, paper = "a4")

par(mfrow = c(4,1))


# RSS as prop of y^2
hist( (1 - rssprop.dnest4)*100, breaks = 20,
      main = "DNEST4 - Forecast Accuracy", 
      xlab = expression(paste("% data explained = 100*(1 - RSS/" ,
                              {sum(y[i]^2, i)}, ")")),
      
      sub = "period = 4 months" )

# log mean RSS for 4 months of data
hist(log(mrss.dnest4),
     main = "Forecast Error",
     xlab = "Log Mean RSS" )

# CI width for t+1 as proportion of yhat Patrons t+1
hist(ci_width1[ci_width1 < 5]*100, breaks = 20,
     main = "Confidence Interval (CI) for t+1",
     xlab = "Width of CI as % of y" ,
     sub = paste0("Excludes ", nci1, " outliers"))

# CI width for t+4 as proportion of yhat Patrons t+4
hist(ci_width4[ci_width4 < 20]*100, breaks = 20,
     main = "Confidence Interval (CI) for t+4",
     xlab = "Width of CI as % of y" ,
     sub = paste0("Excludes ", nci4, " outliers"))

dev.off()


# END #########