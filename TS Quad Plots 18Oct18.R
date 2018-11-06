

# significance ##########

load("quad.summary.rda")

detrend.summary[[1]]

detrend.summary$Amanda_1$summary$coefficients

detrend.summary$Amanda_1$summary$coefficients[,4]

beta0.signif <- unlist(lapply(detrend.summary, FUN = function(x){
  return(x$summary$coefficients[1,4])
}))

beta1.signif <- unlist(lapply(detrend.summary, FUN = function(x){
  return(x$summary$coefficients[2,4])
}))

beta2.signif <- unlist(lapply(detrend.summary, FUN = function(x){
  return(x$summary$coefficients[3,4])
}))

sum(beta1.signif < 0.05)

sum(beta2.signif < 0.05)


# xtable significance #########

library(xtable)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")

myprobs <- c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1)
beta0 <- format.pval(as.vector(quantile(beta0.signif, myprobs)))
beta1 <- format.pval(as.vector(quantile(beta1.signif, myprobs)))
beta2 <- format.pval(as.vector(quantile(beta2.signif, myprobs)))

summary.df <- cbind.data.frame(beta0, beta1, beta2)
names1 <- names(quantile(beta1.signif, myprobs))
rownames(summary.df) <- names1

xtable(summary.df)

# coefficients #########

detrend.summary$Amanda_1$summary$coefficients
detrend.summary$Amanda_1$summary$coefficients[, 1]

beta0.coef <- unlist(lapply(detrend.summary, FUN = function(x){
  return(x$summary$coefficients[1, 1])
}))

beta1.coef <- unlist(lapply(detrend.summary, FUN = function(x){
  return(x$summary$coefficients[2, 1])
}))

beta2.coef <- unlist(lapply(detrend.summary, FUN = function(x){
  return(x$summary$coefficients[3, 1])
}))

sum(abs(beta2.coef[beta2.signif < 0.05]) < 1)

myprobs <- c(0, 0.10, 0.25, 0.50, 0.90, 1)
beta0 <- as.vector(quantile(beta0.coef, myprobs))
beta1 <- as.vector(quantile(beta1.coef, myprobs))
beta2 <- as.vector(quantile(beta2.coef, myprobs))

summary.df <- cbind.data.frame(beta0, beta1, beta2)
names1 <- names(quantile(beta1.coef, myprobs))
rownames(summary.df) <- names1

xtable(summary.df, digits = 1)


# ARMA plots #########


## ACF ##########

load("quad.tsanalysis.rda")

# rename for convenience
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


## PACF ###############


# data for plots
{
  pacf.lags <- lapply(detrend.tsanalysis, FUN =  function(x){x$pacf.lags})
  
  pacf.lags2 <- unlist(pacf.lags)
  
  table(pacf.lags2)
  
  # number of creators for which significant
  
  length.lags <- unlist(lapply(pacf.lags, FUN = length))
  
  
  # length 0 implies none ar significant
  table(length.lags)
  
  # percent significant
  round(100 * table(length.lags)/sum(table(length.lags)), 2)
}


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
  
  table(arma.order)
}


# latex plots ACF, PACF ARMA ##########

{
  
  pdf(file = "arma_quad.pdf", height = 9)
  
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




