library(tidyverse)
library(moments)
library(tseries)
# Model trajectory with conventional "Black-Scholes"-type model
# -> Compute log returns and evaluate i.i.d assumption as well as normality

check_log_returns <- function(df)
{
  cats <- unique(df$race)
  K <- length(cats)
  skews <- vector("double", K) # skewness coefficients for each group
  kurts <- vector("double", K) # kurtosis coefficients for each group
  pvals <- vector("double", K) # p-values for each group
  acf_max <- vector("double", K) # maximum of autocorrelations (should be low)
  
  for(i in seq_along(cats))
  {
    cat <- cats[i]
    subgroup <- subset(df, race==cat)
    
    # Calculate log returns
    returns <- yearly_returns(subgroup, colname="yearly_mean", log=TRUE)$return
    returns <- returns[-1]
    
    # Measures of normality:
    skews[i] <- skewness(returns) # should be "close" to zero
    kurts[i] <- kurtosis(returns) # should be "close" to 3
    # Test for univariate normality:
    # Alternative: Use mvn(data.frame(returns, ...), mvnTest = "mardia") for a multivariate version
    jb_test <- jarque.bera.test(returns) 
    pvals[i] <- jb_test$p.value
    
    # Plot acfs for log-returns and  squared log-returns 
    pdf(paste(paste("group_", cat, sep=""),".pdf", sep=""))
    par(mfrow=c(1,2))
    a_lin <- acf(returns, type="correlation")
    a_squared <- acf(returns^2, type="correlation")
    dev.off()
    acf_max[i] <- max(max(abs(a_lin$acf[-1])), max(abs(a_squared$acf[-1])))
  }
  
  d_normality <- data.frame(race=cats, sk=skews, ku=kurts, pvals=pvals, acf_max=acf_max)
  return(d_normality)
}

estim_b_scholes <- function(df)
{
  # Compute by race: estimates of normal distribution for log returns
  # mu: estimated mean of log returns
  # sigma2: estimated variance of log returns
  df <- df %>% get_ts() %>% group_by(race) %>% 
    yearly_returns(colname="yearly_mean", log=TRUE) %>% 
    summarize(mu=mean(return[-1]), sigma2=var(return[-1]))
  
  return(df)
}