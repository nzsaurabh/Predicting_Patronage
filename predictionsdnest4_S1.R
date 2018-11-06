# predictions from DNEST4
# output 1: dnest4ar1.preds.rda

# data and specifications ################

# data directory
datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

## posterior samples ##
# dnest4 directory
dnest4.dir <- getwd()

# folder containing results from each creator
results.dir <- paste0(dnest4.dir, "/results05Oct18")

# file name of posterior samples
filename <- "posterior_sample.txt"

# creator folder names
# loop to run on this
artist.names <- unlist(read.table(file = paste0(
  dnest4.dir, "/artistnames.txt"),
  stringsAsFactors = FALSE))

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
   
   
   # no need to exp for model with trend
   # get prediction columns
   
   ypred <- subset(post.sample, select = grep('^ypred', colnames(post.sample)))
     
   # compute quantiles and central credible intervals
   alpha = (1-ci)/2
   
   # output is a 3 x 12 matrix
   pred.quantiles <- apply(ypred, 
              MARGIN = 2, FUN = quantile,
              probs = c(alpha, 0.5, 1-alpha)
              )
   
   # get mean because the data isn't logged
   pred.mean <- apply(ypred, 
                      MARGIN = 2, FUN = mean)
   
   # width of credible interval
   ci_width <- pred.quantiles[3, ] - pred.quantiles[1 , ]
   
   dnest4ar1.preds[[i]] <- list(pred.mean = pred.mean,
                              ci_width = ci_width , 
                              pred.quantiles = pred.quantiles,
                              post.sample = post.sample )
 }
 
 
# save results ######################

# rename
dnest4linear.preds <- dnest4ar1.preds

save(dnest4linear.preds, file = 
       paste0(datadir, "/dnest4linear.preds.rda"))

save(dnest4linear.preds, file = 
       "dnest4linear.preds.rda")

# log Z ###############

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

# check results
filepath
logZ[23]
sum(is.na(logZ))


# rename and save
logZ.linear <- logZ
save(logZ.linear, file = "logZ.linear.rda")

load("logZ.linear.rda")
summary(logZ.linear)

pdf(file = "logZlinear.pdf")
hist(logZ.linear,
     main = "Log Marginal Likelihood",
     xlab = "log(Z)",
     sub = "Model with Linear Trend")
dev.off()

# not required anything below this
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


