library(tidyverse)
# Compute the autocorrelation function w.r.t. variable var for race=group
# If difference=TRUE: First difference the series then compute the acf
get_acf <- function(df, group, var, difference=FALSE)
{
  df <- df %>% filter(race==group) %>% arrange(year)
  if(difference)
  {
    df <- df %>% mutate(diff=.data[[var]]-lag(.data[[var]])) %>% filter(!(year==min(year)))
  }
  return(acf(df[,ifelse(difference, "diff",var)], type="correlation"))
}

# Crude measure of wealth increase: estimate regression line for each group
# Returns a dataframe containing race and the estimated regression parameters for each race
avg_slope <- function(df)
{
  #df_all %>% group_by(race) %>% summarize(slope=coef(lm(income_mean ~ year, data=.)).[[)
  df_lm <- df %>%
    group_by(race) %>%
    do(tidy(lm(income_mean ~ year, data = .))) %>%  #  I(year - 1990)
    ungroup() %>%
    select(race, term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate)
  colnames(df_lm) <- c("race","intercept", "slope")
  return(df_lm)
}
