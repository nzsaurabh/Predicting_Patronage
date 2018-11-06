# Dnest4 ratios for summary

# log growth vs linear trend
# quadratic vs linear trend was done in another file

load("forecasts.linear.dnest4.rda")

load("forecasts.lg.rda")

n <- length(forecasts.lg)

# list to store results
ratio.lg.linear <- vector("list", n)


for(i in 1:n){
  if(i == 1){
    ratio.lg.linear[[i]] <- (1 - forecasts.lg[[i]]) / (1 - forecasts.linear.dnest4[[i]])
    
  }else{
    ratio.lg.linear[[i]] <- forecasts.lg[[i]] / forecasts.linear.dnest4[[i]]
  }
}

names(ratio.lg.linear) <- names(forecasts.lg)

# Ratio Summary ############

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

summary.df <- quantile.ratio.fun(ratio.lg.linear)

library("xtable")
xtable(summary.df)

