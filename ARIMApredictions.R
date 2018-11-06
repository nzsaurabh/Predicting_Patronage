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
load("tsanalysis.rda")

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
yhat.lgpatrons <- t(mapply(FUN = preds.fun, x = testdnest4, y = preds))
dim(yhat.lgpatrons)

yhat.lgpatrons[1, ]

low95.lgpatrons <- t(mapply(FUN = preds.fun, x = testdnest4, y = low95))
dim(low95.lgpatrons)

low95.lgpatrons[1, ]

up95.lgpatrons <- t(mapply(FUN = preds.fun, x = testdnest4, y = up95))
dim(up95.lgpatrons)

up95.lgpatrons[1, ]

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

# log mean rss #################
# used for latex 
# in terms of number of patrons

y.patrons <- do.call(rbind, testdnest4)
# leave data for april
dim(y.patrons[, -1])
# predict for 4 months
dim(yhat.lgpatrons[,1:4])

n <- nrow(y.patrons)
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

save(forecasts.arima , file = "forecasts.arima.rda")

# latex plots ##########
# produced perfect plots on 30th Sep 18
# no dots in latex filename

pdf(file = "rssarma00.pdf", height = 10, paper = "a4")

par(mfrow = c(4,1))


# RSS as prop of y^2
hist( (1 - rssprop.arima)*100, breaks = 20,
      main = "auto.arima - Forecast Accuracy",
      xlab = expression(paste("% data explained = 100*(1 - RSS/" ,
                              {sum(y[i]^2, i)}, ")"))
)

# log mean RSS for 4 months of data
hist(log(mrss.arima),
     main = "Forecast Error",
     xlab = "Log Mean RSS")

# CI width for t+1 as proportion of yhat Patrons t+1
hist(ci_width1*100, breaks = 20,
     main = "Confidence Interval (CI) for t+1",
     xlab = "Width of CI as % of y")

# CI width for t+4 as proportion of yhat Patrons t+4
hist(ci_width4*100, breaks = 20,
     main = "Confidence Interval (CI) for t+4",
     xlab = "Width of CI as % of y")

dev.off()

# RMSE latex plots ##########
# produced perfect plots on 30th Sep 18
# no dots in latex filename

load("forecasts.arima.rda")

rssprop.arima <- forecasts.arima$rssprop.arima
rmse.arima <- forecasts.arima$rmse.arima
ci_width1 <- forecasts.arima$ci_width1.arima
ci_width4 <- forecasts.arima$ci_width4.arima
  
pdf(file = "rmse_arima.pdf", height = 10, paper = "a4")

par(mfrow = c(4,1))


# RSS as prop of y^2
hist( (1 - rssprop.arima)*100, breaks = 20,
      main = "auto.arima - Forecast Accuracy", 
      xlab = "Accuracy (% similarity to data)",
      sub = "period = 4 months" )

# log mean RSS for 4 months of data
hist(log(rmse.arima),
     main = "Forecast Error",
     xlab = "Log Root Mean Square Error (RMSE)" )

# CI width for t+1 as proportion of yhat Patrons t+1
hist(ci_width1*100, breaks = 20,
     main = expression(paste("Confidence Interval (", CI[t+1] , ")")),
     xlab = expression(paste("Width of CI as % of ", hat(y) )))

# CI width for t+4 as proportion of yhat Patrons t+4
hist(ci_width4*100, breaks = 20,
     main = expression(paste("Confidence Interval (", CI[t+4] , ")")),
     xlab = expression(paste("Width of CI as % of ", hat(y) )))

dev.off()




# RMSE Summary ###########

library(xtable)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")


Accuracy <- 100 * as.vector(summary(1-rssprop.arima))
RMSE <- as.vector(summary(rmse.arima))
CI_t1 <- 100 * as.vector(summary(ci_width1))
CI_t4 <- 100 * as.vector(summary(ci_width4))

names1 <- names(summary(rmse.arima))

summary.df <- cbind.data.frame(Accuracy, RMSE, CI_t1, CI_t4)
rownames(summary.df) <- names1

xtable(summary.df)



# Ratio RMSE #########

load("forecasts.arima.rda")

load("forecasts.lg.rda")

n <- length(forecasts.arima)

ratio.lg.arima <- vector("list", n)

for(i in 1:n){
  if(i == 1){
  ratio.lg.arima[[i]] <- (1 - forecasts.lg[[i]]) / (1 - forecasts.arima[[i]])
  
  }else{
    ratio.lg.arima[[i]] <- forecasts.lg[[i]] / forecasts.arima[[i]]
  }
}

names(ratio.lg.arima) <- names(forecasts.lg)

# Ratio Summary ############

# function for ratios only
summary.ratio.fun <- function(x.forecasts){
  
  Accuracy <- as.vector(summary(x.forecasts[[1]]))
  RMSE <- as.vector(summary(x.forecasts[[2]]))
  CI_t1 <- as.vector(summary(x.forecasts[[4]]))
  CI_t4 <- as.vector(summary(x.forecasts[[5]]))
  
  names1 <- names(summary(x.forecasts[[2]]))
  
  summary.df <- cbind.data.frame(Accuracy, RMSE, CI_t1, CI_t4)
  rownames(summary.df) <- names1
  
  return(summary.df)
  
  }

summary.df <- summary.ratio.fun(ratio.lg.arima)

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

# rss  ############
# not required now

rss.fun <- function(preds, yvals){
  rss <- sum((yvals - preds)^2)
}

rss.arima <- mapply(FUN = rss.fun , 
                    preds = preds ,
                    yvals = actualgrowth)


# add names to preds for easy reference
names(preds) <- names(actualgrowth)
names(rss.arima) <- names(actualgrowth)

# check
actualgrowth[[1]]
preds[[1]]

sum((actualgrowth[[1]] - preds[[1]])^2)

rss.arima[[1]]

# looks good to me

# sum over 103 artists
sum(rss.arima)

# save the objects
save(rss.arima, file = "rss.arima.rda")
save(actualgrowth, file = "actualgrowth.rda")
save(tspred.df, file = "tspred.df.rda")

# plot of RSS
pdf(file = "RSSarima.pdf")
hist(rss.arima, xlab = "Residual SS over 4 months",
     main = "Residual Sum of Squares - auto.arima",
     sub = "n = 103 creators")

dev.off()

# plot of ci widths

load("tspred.df.rda")



