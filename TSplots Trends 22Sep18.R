# source data #####

datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

load(paste0(datadir,"traindnest4.rda"))

#load("tsanalysis.rda")

# destination folder
# dest.folder <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Dissertation Latex/figures"


dest.folder <- getwd()

# detrend the data #######

detrend.fun <- function(x){
  
  data <- cbind.data.frame(Patrons = x, t = 1:length(x))
  notrend.lm <- lm(Patrons ~ t, data = data)
  
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

detrend.summary[[3]]$coef

# fetch only the data

detrend.data <- lapply(detrend.summary, FUN = 
                         function(x){
                           list( Patrons.detrend = x$residuals, 
                              intercept = x$coef[1],
                              trend = x$coef[2]
                           )
                         })

# function to grab time series data of log growth
grabts <- function(x){
  ts(x$Patrons, frequency = 12,
     start = c(2016, 05))
}

# fetch data in list format
detrend.tsdata <- lapply(detrend.data, FUN = grabts)

# save objects #####

# save(detrend.summary, file = "detrend.summary.rda")

# save(detrend.data, file = "detrend.data.rda")

# save(detrend.tsdata, file = "detrend.tsdata.rda")

# significance ##########

load("detrend.summary.rda")

detrend.summary$Amanda_1$summary$coefficients

detrend.summary$Amanda_1$summary$coefficients[,4]

beta0.signif <- unlist(lapply(detrend.summary, FUN = function(x){
  return(x$summary$coefficients[1,4])
}))

beta1.signif <- unlist(lapply(detrend.summary, FUN = function(x){
  return(x$summary$coefficients[2,4])
}))

sum(beta0.signif < 0.05)

sum(beta1.signif < 0.05)

# summary xtable #########

# RMSE Summary ###########

library(xtable)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")


beta0 <- format.pval(as.vector(summary(beta0.signif)))
beta1 <- format.pval(as.vector(summary(beta1.signif)))

names1 <- names(summary(beta1.signif))

summary.df <- cbind.data.frame(beta0, beta1)
rownames(summary.df) <- names1

xtable(summary.df)

myprobs <- c(0, 0.10, 0.25, 0.50, 0.90, 1)
beta0 <- format.pval(as.vector(quantile(beta0.signif, myprobs)))
beta1 <- format.pval(as.vector(quantile(beta1.signif, myprobs)))

summary.df <- cbind.data.frame(beta0, beta1)
names1 <- quantile(beta1.signif, myprobs)
rownames(summary.df) <- names1

xtable(summary.df)


# latex plot ####

pdf(file = "linearsignif.pdf")

par(mfrow = c(2,1))

hist(beta0.signif, breaks = 20, 
     main = "Significance of Intercept",
     xlab = expression(paste("p-value of ", {beta[0]})),
     log = "x"
)

hist(beta1.signif, breaks = 20, 
     main = "Significance of Linear Trend",
     xlab = expression(paste("p-value of ", {beta[1]}))
)

dev.off()

# TS Analysis ###############

# not required to run everytime

load("detrend.tsdata.rda")

# Load function
# has also been saved in the working directory

load("detrend.tsmodel.fun.rda")

library(forecast)

# apply ts function
detrend.tsanalysis <- lapply(detrend.tsdata, FUN = detrend.tsmodel.fun)

# save the analysis
save(detrend.tsanalysis, file = "detrend.tsanalysis.rda")



## ACF ##########

load("detrend.tsanalysis.rda")

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

#pdf(file = "acf_detrend.pdf")
barplot(table(acf.lags2), 
        main = "Autocorrelation after detrending", xlab = "Significant lags",
        ylab = "Number of creators",
        sub = "n = 103")

barplot(table(length.lags), 
        main = "Distribution of Significant lags", xlab = "Number of Lags",
        ylab = "Percent of creators",
        sub = "n = 103")

#dev.off()


## PACF ###############


# data for plots
{
  pacf.lags <- lapply(detrend.tsanalysis, FUN =  function(x){x$pacf.lags})
  
  pacf.lags2 <- unlist(pacf.lags)
  
  table(pacf.lags2)
  
  # number of creators for which significant
  
  length.lags <- unlist(lapply(pacf.lags, FUN = length))
  
  # length 1 are zeros
  table(length.lags)
  
  # percent significant
  round(100 * table(length.lags)/sum(table(length.lags)), 2)
}

# plots

#pdf(file = "pacf_detrend.pdf")
barplot(table(pacf.lags2), 
        main = "Partial Autocorrelation after detrending", xlab = "Significant lags",
        ylab = "Number of creators",
        sub = "n = 103")

barplot(table(length.lags), 
        main = "Distribution of Significant lags", xlab = "Number of Lags (incl. lag 0)",
        ylab = "Percent of creators",
        sub = "n = 103")

#dev.off()


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

arma.order <- mapply(function(x, y){paste0("(",  x, "," , y , ")")}, x = ar.p, y = ma.q )


}

table(arma.order)

# plots


barplot(table(arma.order) , 
        main = "AR, MA order after detrending", xlab = "AR,MA order by auto.arima",
        ylab = "Number of creators",
        sub = "n = 103")



# latex plots ACF, PACF ARMA ##########

{

pdf(file = "arma_detrend.pdf", height = 9)

#layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
  
  par(mfrow = c(3,1))

# acf

barplot(table(acf.lags2), 
        main = "Autocorrelation", xlab = "Significant lags",
        ylab = "Number of creators",
        sub = "across 103 creators")

# pacf

barplot(table(pacf.lags2), 
        main = "Partial Autocorrelation", xlab = "Significant lags",
        ylab = "Number of creators",
        sub = "across 103 creators")

# arma

barplot(table(arma.order) , 
        main = "AR, MA order", xlab = "(AR,MA) order by auto.arima",
        ylab = "Number of creators",
        sub = "n = 103 creators")

dev.off()

}






## section break ##########

# plots for data exploration ###########

# plot data of the 10 creators used in data exploration
# to be completed

load(paste0(datadir,"plot.patrons.rda"))

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

# data for plots

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

# plot using plot.list

# inputs for plot

month <- 1:24

# y limits for patrons < 3000

(ymax1 <- ceiling(max(unlist(plot.list[under3k]))))
(ymin1 <- floor(min(unlist(plot.list[under3k]))))

# y limits for greater than

(ymax2 <- ceiling(max(unlist(plot.list[over3k]))))
(ymin2 <- floor(min(unlist(plot.list[over3k]))))

## Plots ###########
## Make 2 plots - above and below 3k 

fontsize = 0.7
ylab = "Residuals after detrending"

{
pdf(file = "Patrons_detrend.pdf")

par(mfrow = c(2,1))
par(mar = par("mar") + c(0,1,0,0), cex = fontsize)

# creators > 3k
index <- over3k

for(j in 1:length(index)){
  if(j == 1){
    plot(x = month, y = plot.list[[index[j]]],
         pch = j, 
         cex = fontsize,
         main = paste0("Random Creators (with > ", cutoff, " patrons)" ),
         ylab = ylab,
         xlab = "Month (day 1)",
         ylim = c(ymin2, ymax2)
    )
  }else{
    points(x = month, plot.list[[index[j]]],
           pch = j, cex = fontsize)
  }
  lines(x = month, y = plot.list[[index[j]]],
        col = j)
}

# creators < 3k
index <- under3k

for(j in 1:length(index)){
  if(j == 1){
    plot(x = month, y = plot.list[[index[j]]],
         pch = j,
         cex = fontsize,
         main = paste0("Random Creators (with < ", cutoff, " patrons)" ),
         ylab = ylab,
         xlab = "Month (day 1)",
         ylim = c(ymin1, ymax1)
    )
  }else{
    points(x = month, plot.list[[index[j]]],
           pch = j, cex = fontsize)
  }
  lines(x = month, y = plot.list[[index[j]]],
        col = 4+j)
}

dev.off()

rm(fontsize, ylab)

}

## section break ##########

# Predictions #############

# predictions are in $tspred.df
load("detrend.tsanalysis.rda")

# need to add intercept and trend from detrend.data
load("detrend.data.rda")

# data directory
datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

# test data
# it has patrons for time t onwards
# we need only from time t+1

load(paste0(datadir,"testdnest4.rda"))
testdnest4[[1]]

# this pred function is different from that used for log growth model

detrend.preds.fun <- function(x, y){
  
  preds <- x$tspred.df$tspred
  
  t = length(y$Patrons.detrend)

  t = (t+1):(t + length(preds))
  
  trend <- y$intercept + y$trend * t
  
  preds <- preds + trend
  
  }

# preds has 12 rows and 103 columns
preds12 <- mapply(FUN = detrend.preds.fun , x = detrend.tsanalysis, y = detrend.data)

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

plot(density(sort.rss[1:(n-5)]))

pdf("hist.rss.auto.pdf")
hist(sort.rss[1:(n-5)], breaks = 20,
     main = "RSS - auto.arima",
     xlab = "RSS for 4 months prediction",
     ylab = "No. of Creators")
dev.off()


