# source data #####

datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

load(paste0(datadir,"traindnest4.rda"))

# destination folder
# dest.folder <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Dissertation Latex/figures"


dest.folder <- getwd()

# detrend the data #######

detrend.fun <- function(x){
  
  data <- cbind.data.frame(Patrons = x, t = 1:length(x))
  notrend.lm <- lm(Patrons ~ t + I(t^2), data = data)
  
  list (summary = summary(notrend.lm) ,
        coef = coefficients(notrend.lm) ,
        residuals = residuals(notrend.lm)
  )
}

# apply function


detrend.summary <- lapply(traindnest4, FUN = detrend.fun)

# check summaries of some of the creators
# they are all significant
detrend.summary[[1]]$summary

detrend.summary[[3]]$summary

detrend.summary[[5]]$summary

detrend.summary[[8]]$summary

detrend.summary[[23]]$summary

str(detrend.summary[[3]])

# significance level is in this object
detrend.summary[[3]]$summary$coefficients[2,4]

# fetch residuals and coef
# if trend2 is not significant, trend2 = 0

# significance level
alpha = 0.1

detrend.data <- lapply(detrend.summary, FUN = 
                         function(x){
                           
                           if(x$summary$coefficients[2,4] <= alpha){
                             trend2 =  x$coef[3]
                           }else{trend2 = 0}
                           
                           list( Patrons.detrend = x$residuals, 
                              intercept = x$coef[1],
                              trend = x$coef[2], trend2 = trend2
                               )
                         })


# function to grab data in time series format
grabts <- function(x){
  ts(x$Patrons, frequency = 12,
     start = c(2016, 05))
}

# fetch data in list format
detrend.tsdata <- lapply(detrend.data, FUN = grabts)

# save objects #####

# save(detrend.summary, file = "quad.summary.rda")

# save(detrend.data, file = "quad.data.rda")

# save(detrend.tsdata, file = "quad.tsdata.rda")

# quadratic trend #############

load("quad.data.rda")

detrend.data[[1]]

trend2 <- unlist(lapply(detrend.data, FUN = function(x){x$trend2}))

# quadratic trend is insignificant or < 1 in only 2 of 103 creators 
sum(trend2 == 0)
sum(abs(trend2) < 1)

sum(abs(trend2) < 10)

# inputs for plot labels
n <- length(trend2)
notsignif <- sum(trend2 == 0)
 
pdf(file = "quadratic.trend.pdf")
hist(trend2, breaks = 40,
     main = "Quadratic Trend",
     sub = paste0("n = ", n, "; significant = ", n - notsignif ),
     xlab = expression(paste("Coefficient for ", {t^2}))
     )
dev.off()


# write summary to text file
sink("summary.trend2.txt")
cat("Summary - Coefficient for quadratic trend (t^2)\n")
summary(trend2)
cat("\nNo. of coefficients insignificant\n")
sum(trend2 == 0)
cat("\nNo. of coefficients that have absolute value < 1\n")
sum(trend2 < 1)
sink()

file.show("summary.trend2.txt")

# section break #################


# TS Analysis ###############

# not required to run everytime

load("quad.tsdata.rda")

# Load function
# has also been saved in the working directory

load("detrend.tsmodel.fun.rda")

library(forecast)

# apply ts function
quad.tsanalysis <- lapply(detrend.tsdata, FUN = detrend.tsmodel.fun)

# save the analysis
save(quad.tsanalysis, file = "quad.tsanalysis.rda")


## ACF ##########

load("quad.tsanalysis.rda")

detrend.tsanalysis <- quad.tsanalysis

# data for plots
{
acf.lags <- lapply(detrend.tsanalysis, FUN =  function(x){x$acf.lags})

acf.lags2 <- unlist(acf.lags)

table(acf.lags2)

# number of creators for which significant

length.lags <- unlist(lapply(acf.lags, FUN = length))

# length 1 are zeros
table(length.lags)

}

# plots

pdf(file = "acf_quad.pdf")
barplot(table(acf.lags2), 
        main = "Autocorrelation after quadratic detrending", xlab = "Significant lags",
        ylab = "Number of creators",
        sub = "n = 103")

barplot(table(length.lags), 
        main = "Distribution of Significant lags", xlab = "Number of Lags (incl. lag 0)",
        ylab = "Percent of creators",
        sub = "n = 103")

dev.off()


## PACF ###############


# data for plots
{
  pacf.lags <- lapply(detrend.tsanalysis, FUN =  function(x){x$pacf.lags})
  
  pacf.lags2 <- unlist(pacf.lags)
  
  table(pacf.lags2)
  
  # number of creators for which significant
  
  length.lags <- unlist(lapply(pacf.lags, FUN = length))
  
  # length 1 are lag 0
  # length 0 implies none ar significant
  table(length.lags)
  
  # percent significant
  round(100 * table(length.lags)/sum(table(length.lags)), 2)
}

# plots

pdf(file = "pacf_quad.pdf")
barplot(table(pacf.lags2), 
        main = "Partial Autocorrelation after quadratic detrending", xlab = "Significant lags",
        ylab = "Number of creators",
        sub = "n = 103")

barplot(table(length.lags), 
        main = "Distribution of Significant lags", xlab = "Number of Lags",
        ylab = "Percent of creators",
        sub = "n = 103")

dev.off()


# auto.arima order ############

# AR order selected by auto.arima model
# need to present as (p, q)

{
ar.p <- lapply(detrend.tsanalysis, FUN =  function(x){x$ar.p})

table(unlist(ar.p))

ma.q <- lapply(detrend.tsanalysis, FUN =  function(x){x$ma.q})
table(unlist(ma.q))

# mapply returns a matrix of 103 columns
# transpose it to get the correct shape

arma.order <- mapply(function(x, y){paste(x, y, sep = ",")}, x = ar.p, y = ma.q )

table(arma.order)
}

# plots

pdf(file = "arma_quad.pdf")
barplot(table(arma.order) , 
        main = "AR, MA order after quadratic detrending", xlab = "AR,MA order by auto.arima",
        ylab = "Number of creators",
        sub = "n = 103")
dev.off()


## section break ##########

# plots for data exploration ###########

# setup ######
# plot data of the 10 creators used in data exploration
# to be completed

load(paste0(datadir,"plot.patrons.rda"))

# object name is detrend.data
load("quad.data.rda")

# need to get the same sample everytime
set.seed(1234)

# create index for random artists
art.index <- length(traindnest4)

# number of artists for which data is required
n = 10
rand.index <- sample(art.index, n)

# check data
traindnest4[rand.index]

# check the original plot data
names(plot.patrons)

# use the same cutoffs for 2 plots

n <- length(plot.patrons)

# Seperate artists depending on min and max values

ymax <- sapply(plot.patrons, FUN = max)

ymin <- sapply(plot.patrons, FUN = min)

# plot ymax and ymin
plot(ymax, ymin, type = "n")
text(ymax, ymin, labels = 1:10, cex = 0.8)

# subset creators with patrons < 3000

cutoff <- 3000
over3k <- c(1, 7, 8, 10)
(under3k <- (1:n)[-over3k])

# data for plots ########

plot.list <- vector("list", 10)
j = 0
for(i in rand.index){
  j = j + 1
  plot.list[[j]] <- detrend.data[[i]]$Patrons.detrend
  names(plot.list)[j] <- names(detrend.data)[i]
}

# compare name indices with train data and rand index
names(plot.list)
names(traindnest4)[rand.index]
rand.index
# all match perfectly. Amanda's data also matches plot.patrons

# save(plot.list, file = "plot.quadlist.rda")

# load plot.list #########
load("plot.quadlist.rda")

# plot using plot.list

# inputs for plot

month <- 1:24

# y limits for patrons < 3000

(ymax1 <- ceiling(max(unlist(plot.list[under3k]))))
(ymin1 <- floor(min(unlist(plot.list[under3k]))))

# y limits for greater than

(ymax2 <- ceiling(max(unlist(plot.list[over3k]))))
(ymin2 <- floor(min(unlist(plot.list[over3k]))))

## Plots acf and pacf  ###########
## Make 2 plots - above and below 3k 

fontsize = 0.8
ylab = "Residuals after quadratic detrending"

{
pdf(file = "Patrons_quad.pdf")

par(mfrow = c(2,1))
par(mar = par("mar") + c(0,1,0,0), cex = fontsize)

# creators > 3k
index <- over3k

for(j in 1:length(index)){
  if(j == 1){
    plot(x = month, y = plot.list[[index[j]]],
         type = "p" ,  pch = j+1,
         cex = fontsize,
         main = paste0("Random Creators (with > ", cutoff, " patrons)" ),
         ylab = ylab,
         xlab = "Month (day 1)",
         ylim = c(ymin2, ymax2)
    )
  }else{
    points(x = month, plot.list[[index[j]]],
           pch = j+1, cex = fontsize)
  }
  lines(x = month, y = plot.list[[index[j]]])
}

# creators < 3k
index <- under3k

for(j in 1:length(index)){
  if(j == 1){
    plot(x = month, y = plot.list[[index[j]]],
         type = "p" ,  pch = j+1,
         cex = fontsize,
         main = paste0("Random Creators (with < ", cutoff, " patrons)" ),
         ylab = ylab,
         xlab = "Month (day 1)",
         ylim = c(ymin1, ymax1)
    )
  }else{
    points(x = month, plot.list[[index[j]]],
           pch = j+1, cex = fontsize)
  }
  lines(x = month, y = plot.list[[index[j]]])
}

dev.off()

rm(fontsize, ylab)

}

# plots adf test ##########

library(tseries)

# data after quadratic detrending
load("plot.quadlist.rda")

adf.test <- unlist(lapply(plot.list, FUN = 
                            function(x){
                              adf.test(x)$p.value}
))

names(adf.test) <- 1:10

pdf(file = "adftest_quad10.pdf")
barplot(adf.test, main = "Augmented Dickey Fuller Test", 
        ylab = "p.value", xlab = "Creator",
        sub = "Tested on residuals after quadratic detrending")
dev.off()


# test on all creators

# data after quadratic detrending

load("quad.data.rda")

adf.test <- unlist(lapply(detrend.data, FUN = 
                            function(x){
                              adf.test(x$Patrons.detrend)$p.value}
))


pdf(file = "adftest_quadall.pdf")
hist(adf.test, 
     main = "Augmented Dickey Fuller Test", 
     xlab = "p.value",
     sub = "Tested on residuals after quadratic detrending")
dev.off()

# raw data

load(paste0(datadir,"allloggrowth.rda"))

adf.test <- unlist(lapply(allloggrowth, FUN = 
                            function(x){
                              adf.test(x[ ,"Patrons"])$p.value}
))


pdf(file = "adftest_loggrowth.pdf")
par(cex = 1.5)
hist(adf.test, 
     main = "Augmented Dickey Fuller Test", 
     xlab = "p.value",
     sub = "Tested on Log Growth"
     )
dev.off()


## section break ##########

# Predictions #############

# predictions are in $tspred.df
load("quad.tsanalysis.rda")

# need to add intercept and trend from detrend.data
load("quad.data.rda")

# data directory
datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

# test data
# it has patrons for time t onwards
# we need only from time t+1

load(paste0(datadir,"testdnest4.rda"))
testdnest4[[1]]

# this pred function is different from that used for log growth model

quad.preds.fun <- function(x, y){
  
  preds <- x$tspred.df$tspred
  
  t = length(y$Patrons.detrend)

  t = (t+1):(t + length(preds))
  
  trend <- y$intercept + (y$trend * t) + (y$trend2 * (t^2))
  
  preds <- preds + trend
  
  }

# preds has 12 rows and 103 columns
preds12 <- mapply(FUN = quad.preds.fun , x = quad.tsanalysis, y = detrend.data)

test.preds <- vector("list", length = length(detrend.data) )

# rss calculation #################
# r2 can't be calculated from mean of yvals
# but we can compare rss to another model

for(i in 1:length(test.preds)){
  preds <- preds12[1:4, i]
  
  yvals <- testdnest4[[i]][-1]
  
  rss <- sum((yvals - preds)^2)
  
  test.preds[[i]] <- list(preds = preds, yvals = yvals, rss = rss)
  
}

 
# add names to preds for easy reference
names(test.preds) <- names(testdnest4)


test.preds[[1]]

test.rss <- unlist(lapply(test.preds, FUN = function(x){x$rss} ))

sink(file="Summary_Test_rss.txt") 
summary(test.rss)
sink(NULL) 

# save(test.rss, file = "test.rss.rda")

sort.rss <- sort(test.rss)

# min values
head(sort.rss)

# max values
tail(sort.rss)

# Histogram excluding extreme obs
# RSS is not a continuous variable

n <- length(sort.rss)

plot(density(sort.rss[1:(n-10)]))

# RSS plot ##############

pdf("hist.rss.auto.pdf")
hist(sort.rss[1: (n-10)], breaks = 40,
     main = "RSS - auto.arima",
     xlab = "RSS for 4 months prediction",
     ylab = "No. of Creators",
     sub = "excluding 10 highest RSS"
)

hist(sort.rss[sort.rss < 1e5],
     main = "RSS less than 1e5", breaks = 20,
     xlab = "RSS for 4 months prediction",
     ylab = "No. of Creators",
     sub = paste0("n* = ", sum(sort.rss < 1e5)))
dev.off()



# MSE plot ##############

MSE <- sort.rss / 4

pdf("mse.auto.pdf")
hist(MSE[1:(n-10)], breaks = 40,
     main = "MSE - auto.arima",
     xlab = "MSE for 4 months prediction",
     ylab = "No. of Creators",
     sub = "excluding 10 highest RSS"
)

hist(MSE[MSE < 1e5] ,
     main = "MSE less than 1e5", 
     xlab = "MSE for 4 months prediction",
     ylab = "No. of Creators",
     sub = paste0("n* = ", sum(MSE < 1e5)))

dev.off()

