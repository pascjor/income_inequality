### Contains functions for the computation of commonly used simple statistics within each group
### a) get_acf -> compute autocorrelations (only used in an exploratory files on GARCH-models - 
### not added to repo cause incomplete); b) avg_slope -> compute regression line for each group (used in 
### plots)
library(broom)

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
  df_lm <- df %>%
    group_by(race) %>%
    do(tidy(lm(income_mean ~ year, data = .))) %>%  #  I(year - 1990)
    ungroup() %>%
    select(race, term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate)
  colnames(df_lm) <- c("race","intercept", "slope")
  return(df_lm)
}
