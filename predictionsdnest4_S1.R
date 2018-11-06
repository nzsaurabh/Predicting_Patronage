# predictions from DNEST4
# output 1: dnest4ar1.preds.rda

# data and specifications ################

# data directory
datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

## posterior samples ##
# dnest4 directory
dnest4.dir <- getwd()

# folder containing results from each creator
results.dir <- paste0(dnest4.dir, "/results_quad_08Oct18")

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
dnest4quad.preds <- dnest4ar1.preds

save(dnest4quad.preds, file = 
       paste0(datadir, "/dnest4quad.preds.rda"))

save(dnest4quad.preds, file = 
       "dnest4quad.preds.rda")

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
logZ.quad <- logZ
save(logZ.quad, file = "logZ.quad.rda")

# rename again for convenience

load("logZ.quad.rda")
logZ.linear <- logZ.quad

summary(logZ.linear)

pdf(file = "logZquad.pdf")
hist(logZ.linear,
     main = "Log Marginal Likelihood",
     xlab = "log(Z)",
     sub = "Model with Quadratic Trend")
dev.off()

# logZ comparison ########

# compare log Z of linear versus quadratic
# compute ratio of quantiles just like for other results

load("logZ.linear.rda")
load("logZ.quad.rda")

# ratio for ease
logZ.ratio <- logZ.quad/logZ.linear

# technically correct is the exponential of difference
logZ.diff <- logZ.quad - logZ.linear

# quantiles
myprobs <- c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1)

logZ.ratio <- as.vector(quantile(logZ.ratio, myprobs))

Z.ratio <- as.vector(quantile(exp(logZ.diff), myprobs))


names1 <- names(quantile(exp(logZ.diff), myprobs))

summary.df <- cbind.data.frame(logZ.ratio, Z.ratio)
rownames(summary.df) <- names1

library(xtable)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")

xtable(summary.df, digits = 2, display = rep("e", 3))

# only Z.ratio

summary.df <- t(summary.df)

xtable(summary.df, digits = 2, display = rep("e", 8))
