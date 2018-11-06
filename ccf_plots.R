
# source data #####

datadir <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18/"

dest.folder <- getwd()

# use all variables
# we don't want lag 1

# log growth ####
load(paste0(datadir,"allloggrowth.rda"))

# data is a matrix. 23 x 7 months for each artist
colnames(allloggrowth[[1]])
length(allloggrowth)

# The lag k value returned by ccf(x, y) estimates
# the correlation between x[t+k] and y[t]
# x = youtube videos, y = patrons
# youtube lags patrons at lags 2 and 4 i.e. we need x[t+4] to predict y[t].
# Hence, it can't predict patrons
ccf(x = allloggrowth[[1]][, 6], y = allloggrowth[[1]][, 1])


ccf.fun <- function(x){
  
  # number of columns
  n <- dim(x)[2]
  
  # don't need for patrons and earnings
  ccf.signif <- vector( "list" , length = n-2)
  
  names(ccf.signif) <- colnames(x)[-c(1,2)]
  
  for(j in 3:n){
    
    ccfvalues <- ccf(x = x[, j], y = x[, 1], na.action = na.pass, plot = FALSE)
    
    #lags
    names(ccfvalues$acf) <- ccfvalues$lag
    
    # confidence interval for ccf matches the plots at 0.4
    ci <- qnorm((1+0.95)/2)/sqrt(ccfvalues$n.used)
    
    # significant values
    
    tempvar <- ccfvalues$lag[ccfvalues$acf > ci]
    
    length(tempvar)
    
    if(length(tempvar) == 0){
      ccf.signif[[j-2]] <- NA
    }else{
      ccf.signif[[j-2]] <- tempvar
    }
    
  } # end of for loop
  
  return(ccf.signif)
} # end of function

ccf.lg <- lapply(allloggrowth, FUN = ccf.fun )

# save(ccf.lg, file = "ccf.lg.rda")

# linear trend ####

load(paste0(datadir,"trainallvars.rda"))

# need only numeric variables
data <- lapply( trainallvars, FUN = 
                  function(x)Filter(is.numeric, x))

# Detrend social metrics #########

detrend.fun <- function(y){
  
  y.res <- apply(y, MARGIN = 2, FUN = function(x){
    
    if(sum(is.na(x)) < 12){
      lmdata <- cbind.data.frame(x, t = 1:length(x))
      
      residuals <- residuals(lm(x ~ t , data = lmdata, 
                                na.action = na.exclude))
      
      return(x = residuals)
    }else{
      return(x = rep(NA,length(x) ))
    }
    })
  
  return(y.res)}

linear.residuals <- lapply(data, FUN = detrend.fun)

# The lag k value returned by ccf(x, y) estimates
# the correlation between x[t+k] and y[t]
# x = youtube videos, y = patrons
# youtube lags patrons at lag 4 i.e. we need x[t+4] to predict y[t].
# Hence, it can't predict patrons
# the tapering pattern is due to the underlying AR series of y itself
ccf(x = linear.residuals[[1]][, 6], y = linear.residuals[[1]][, 1])

ccf.linear <- lapply(linear.residuals, FUN = ccf.fun )

#save(linear.residuals, file = "linear.residuals.rda")
#save(ccf.linear, file = "ccf.linear.rda")

# quadratic #####  ##########

# load quadratic residuals of all variables
load("quad.residuals.rda")

ccf.quad <- lapply(data.residuals, FUN = ccf.fun )

#save(ccf.quad, file = "ccf.quad.rda")

# plot data #####

load("ccf.lg.rda")
# plot.data <- ccf.lg 

load("ccf.linear.rda")
# plot.data <- ccf.linear 

load("ccf.quad.rda")
# plot.data <- ccf.quad



# create data for the plot

plot.data <- ccf.quad

{
(vars <- names(plot.data[[1]]))

fb.ccf <- unlist(lapply(plot.data, FUN = function(x){x$Facebook.Likes} ))

tw.ccf <- unlist(lapply(plot.data, FUN = function(x){x$Twitter.Followers} ))

ytsubs.ccf <- unlist(lapply(plot.data, FUN = function(x){x$Youtube.Subscribers} ))

ytvideos.ccf <- unlist(lapply(plot.data, FUN = function(x){x$Youtube.Videos} ))

ytviews.ccf <- unlist(lapply(plot.data, FUN = function(x){x$Youtube.Views} ))

ccf.plot <- list(
  
  Facebook.Likes = fb.ccf,
  Twitter.Followers = tw.ccf,
  Youtube.Subscribers = ytsubs.ccf ,
  Youtube.Videos = ytvideos.ccf ,
  Youtube.Views = ytviews.ccf
)
}

# latex plots #####


vars <- names(ccf.plot)

pdf(file = "ccf_quad.pdf", height = 10)

par(mfrow = c(5,1))

for(i in vars){
  
  barplot(table(ccf.plot[i]), main = i,
          ylab = "No. of Creators" ,
          xlab = paste("Significant Lags")
          
            )
          
}

dev.off()





