# predictions from DNEST4
# output 1: dnest4ar1.preds.rda
# output 2: RSS in log growth terms not required

# log growth is in ypred columns
# hence growth = exp(ypred0) and so on

# data and specifications ################

## posterior samples ##
# dnest4 directory
dnest4.dir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Dnest4 02Sep18"

# folder containing results from each creator
results.dir <- paste0(dnest4.dir, "/results02sep18")

# file name of posterior samples
filename <- "posterior_sample.txt"

# creator folder names
# loop to run on this
artist.names <- unlist(read.table(file = paste0(
  dnest4.dir, "/artistnames.txt"),
  stringsAsFactors = FALSE))

# test data directory
datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

# it has monthly growth from time t
# it is the actual data to be tested against
load(paste0(datadir,"actualgrowth.rda"))

# empty list to contain results
dnest4ar1.preds <- vector("list", 
                          length = length(artist.names))

names(dnest4ar1.preds) <- artist.names

# credible interval required
ci = 0.95

# create loop ######################

for(i in artist.names){
  
  # create filepath
  filepath <- paste0(results.dir, 
                     "/", i, "/", filename)
  
  # read predictions
   post.sample <- read.table(filepath,
                  stringsAsFactors = FALSE)
   
   
   colnames(post.sample) <- read.table(filepath,
                                       stringsAsFactors = FALSE,
                                       comment.char = ";",
                                       sep = ",",
                                       strip.white = TRUE,
                                       nrows = 1)
   
   colnames(post.sample)[1] <- "mu"
   
   
   # ypred_i have log growth
   # exp to get growth
   
   ypred <- exp(post.sample[ , 9:20])
   
   
   # compute quantiles and central credible intervals
   alpha = (1-ci)/2
   
   # output is a 3 x 12 matrix
   pred.quantiles <- apply(ypred, 
              MARGIN = 2, FUN = quantile,
              probs = c(alpha, 0.5, 1-alpha)
              )
   
  
   # width of credible interval
   ci_width <- pred.quantiles[3, ] - pred.quantiles[1 , ]
   
   # forecast median
   growth.pred <- pred.quantiles[2, ]
   
   dnest4ar1.preds[[i]] <- list(growth.pred = growth.pred,
                              ci_width = ci_width , 
                              pred.quantiles = pred.quantiles,
                              post.sample = post.sample )
 }
 
 
# save results ######################

save(dnest4ar1.preds, file = 
       paste0(datadir, "/dnest4ar1.preds.rda"))

save(dnest4ar1.preds, file = 
       "dnest4ar1.preds.rda")


# RSS and plots ##########################

# data directory
datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

# predictions
load("dnest4ar1.preds.rda")

str(dnest4ar1.preds[[1]])

preds.fun <- function(x){x$growth.pred[1:4]}

preds <- lapply(dnest4ar1.preds, FUN = preds.fun)


# test data
# it has monthly growth for time t+1
load(paste0(datadir,"actualgrowth.rda"))

actualgrowth[[77]]

# check if prediction is within credible interval


# log Z ###############
# no need to execute again

# file name for log Z
filename <- "output.txt"

# empty vector for results
logZ <- vector( length = length(artist.names))
names(logZ) <- colnames(artist.names)

# read logZ
j = 1
for(i in artist.names){
  
  # create filepath
  filepath <- paste0(results.dir, 
                        "/", i, "/", filename)
  
  # read output.txt
  output <- readLines(filepath)
  
  # get line number
  logZ.char <- grep("log(Z) = " , output, fixed = TRUE, value = TRUE)
  
  if(length(logZ.char) > 0){
  logZ[j] <- as.numeric(substring(logZ.char, first = 10))
  }else{
    logZ[j] <- NA
  }
  
  
  j = j+1
}



filepath
logZ[23]
sum(is.na(logZ))

# rename and save
logZ.loggrowth <- logZ
save(logZ.loggrowth, file = "logZ.loggrowth.rda")



load("logZ.loggrowth.rda")
summary(logZ.loggrowth)

pdf(file = "logZ_lg.pdf")
hist(logZ.loggrowth,
     main = "Log Marginal Likelihood",
     xlab = "log(Z)",
     sub = "Log Growth Model")
dev.off()

# TSS, RSS and R2 #######################

# rss 

rss.fun <- function(preds, yvals){
  rss <- sum((yvals - preds)^2)
}

rss.dnest4 <- mapply(FUN = rss.fun , 
                    preds = preds ,
                    yvals = actualgrowth)

# add names to preds for easy reference
names(preds) <- names(actualgrowth)
names(rss.dnest4) <- names(actualgrowth)

# check
actualgrowth[[1]]
preds[[1]]

# both are equal, means its computed correctly
sum((actualgrowth[[1]] - preds[[1]])^2)
rss.dnest4[1]

# TSS

tss.fun <- function(yvals){
  tss <- sum((yvals - mean(yvals))^2)
}

tss <- unlist(lapply(actualgrowth, FUN = tss.fun ))

# check 
tss[3]
sum((actualgrowth[[3]] - mean(actualgrowth[[3]]))^2)

# looks fine


# R2

r2.fun <- function(rss, tss){
  r2 <- (tss-rss)/tss
}

r2.dnest4 <- mapply(FUN = r2.fun , 
                     rss = rss.dnest4 ,
                     tss = tss)


# confidence intervals

# lag1 confidence interval
ci1.dnest4 <- unlist(lapply(dnest4ar1.preds, FUN =
                             function(x){
                               x$ci_width[1]
                             }))

# lag4 confidence interval
ci4.dnest4 <- unlist(lapply(dnest4ar1.preds, FUN =
                              function(x){
                                x$ci_width[4]
                              }))

hist(ci1.dnest4[ci1.dnest4 < 10], breaks = 20)

hist(ci1.dnest4, breaks = 20)

# auto.arima results ######

arima.dir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/ARIMA Model 04Sep18/"

load(paste0(arima.dir, "rss.arima.rda"))


r2.arima <- mapply(FUN = r2.fun , 
                    rss = rss.arima ,
                    tss = tss)
hist(r2.arima)


# plot of ci widths

load(paste0(arima.dir, "tspred.df.rda"))

typeof(tspred.df)

ci1.arima <- unlist(lapply(tspred.df, FUN =
              function(x){
                x$ci.width[1]
              } ))

ci4.arima <- unlist(lapply(tspred.df, FUN =
                             function(x){
                               x$ci.width[4]
                             } ))

hist(ci1.arima, breaks = 20)



# data frame ########
rss.df <- data.frame(TSS = tss,
                     RSS.Dnest4 = rss.dnest4, 
                     R2.Dnest4 = r2.dnest4,
                     CI1.Dnest4 = ci1.dnest4,
                     CI4.Dnest4 = ci4.dnest4,
                     RSS.arima = rss.arima,
                     R2.arima = r2.arima,
                     CI1.arima = ci1.arima,
                     CI4.arima = ci4.arima
                     )


save(rss.df, file = "rss.df.rda")


# plots ###############

load("rss.df.rda")

pdf(file = "RSShistograms.pdf")

par(mfrow = c(2, 2))

hist(rss.df$TSS, main = "Total Sum of Squares (TSS)",
     xlab = "TSS across 103 creators for 4 months")

plot.new()

hist(rss.df$RSS.Dnest4,
     main = "Bayesian - Dnest4 sampling",
     xlab = "Residual Sum of Squares (RSS)")

hist(rss.df$RSS.arima, 
     main = "Frequentist - auto.arima model",
     xlab = "Residual Sum of Squares (RSS)"
     )

hist(rss.df$R2.Dnest4,
     main = "Bayesian - Dnest4 sampling",
     xlab = "R squared")

hist(rss.df$R2.arima, 
     main = "Frequentist - auto.arima model",
     xlab = "R squared")


hist(rss.df$R2.Dnest4[abs(rss.df$R2.Dnest4) < 1],
     main = "Bayesian - Dnest4 sampling",
     xlab = "subset of abs(R2) < 1")

hist(rss.df$R2.arima[abs(rss.df$R2.arima) < 100], 
     main = "Frequentist - auto.arima model",
     xlab = "subset of abs(R2) < 100")


hist(rss.df$CI1.Dnest4,
     main = "Bayesian - Dnest4 sampling",
     xlab = "t+1 forecast - Width of Confidence Interval")

hist(rss.df$CI1.arima, 
     main = "Frequentist - auto.arima model",
     xlab = "t+1 forecast - Width of Confidence Interval")

hist(rss.df$CI1.Dnest4[rss.df$CI1.Dnest4 < 1],
     main = "Bayesian - Dnest4 sampling",
     xlab = "t+1 forecast - Subset of CI < 1")

hist(rss.df$CI1.arima[rss.df$CI1.arima < 1], 
     main = "Frequentist - auto.arima model",
     xlab = "t+1 forecast - Subset of CI < 1")

hist(rss.df$CI4.Dnest4,
     main = "Bayesian - Dnest4 sampling",
     xlab = "t+4 forecast - Width of Confidence Interval")

hist(rss.df$CI4.arima, 
     main = "Frequentist - auto.arima model",
     xlab = "t+4 forecast - Width of Confidence Interval")

hist(rss.df$CI4.Dnest4[rss.df$CI4.Dnest4 < 1],
     main = "Bayesian - Dnest4 sampling",
     xlab = "t+4 forecast - Subset of CI < 1")

hist(rss.df$CI4.arima[rss.df$CI4.arima < 1], 
     main = "Frequentist - auto.arima model",
     xlab = "t+4 forecast - Subset of CI < 1")

dev.off()



# write to latex ###############
# library(Hmisc)

# latex(round(summary(rss.dnest4), 4), 
#      title = "SummaryRSS",
#      label = "tab:RSSd4")


# library(xtable)


pdf("RSSd4.hist.pdf")
# histogram of RSS
hist(rss.dnest4)

dev.off()




# Testing the loop ############
# how to compute predictions, RSS etc. 
# read predictions
post.sample <- read.table(paste0(dnest4.dir, 
                                 "/posterior_sample.txt"),
                          stringsAsFactors = FALSE)


colnames(post.sample) <- read.table(paste0(dnest4.dir,
                                           "/posterior_sample.txt"),
                                    stringsAsFactors = FALSE,
                                    comment.char = ";",
                                    sep = ",",
                                    strip.white = TRUE,
                                    nrows = 1)

colnames(post.sample)[1] <- "mu"

head(post.sample)

# It should have 124 rows of 20 variables
# yes its good
# ypred_i have log growth
# exp to get growth

ypred <- exp(post.sample[ , 9:20])

# compute quantiles and central credible intervals
# specify credible interval required
ci = 0.95
alpha = (1-ci)/2
y0.quantile <- quantile(ypred$ypred0, probs = c(alpha, 0.5, 1-alpha))

# width of credible interval
ci_width <- unname(y0.quantile[3] - y0.quantile[1])
ci_width

# density plot indicates an outlier
plot(density(ypred$ypred0))

# predicted value
y0.pred <- y0.quantile[2]


 
 

