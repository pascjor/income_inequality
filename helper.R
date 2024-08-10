library(tidyverse)
# defines income_bracket as an ordered factor
# extracts additional race_coars column matching only white, black and hispanic for later join with home.csv data
preprocess_inc <- function(df)
{
  df[,"income_bracket"] = factor(df$income_bracket, ordered=TRUE, levels=LVLS)
  pattern <- "(White)(?= Alone$)|(Black)(?= Alone$)|(Hispanic)(?= \\(Any Race\\))"
  df$race_coarse <- str_extract(df$race, pattern)
  # ensure correctness:
  print(table(df[,c("race","race_coarse")]))
  return(df)
}

preprocess_home <- function(df)
{
  #df <-  df %>% rename(race_coarse=race)
  return(df)
}

# get summary of missing value count by variable
missings_summary <- function(df)
{
  print(apply(df, FUN=function(x) sum(is.na(x)), MARGIN=2))
}

# handle missing values in income_mean either by a) reducing to complete cases, b) back- or forward filling
# or by leaving them as is ("identity")
# use implicits=TRUE to first acquire all implicity missings, i.e. year-race combinations not contained in the df
handle_missings_inc <- function(df, method="identity", implicits=FALSE)
{
  if (implicits) {df <- df %>% complete(year, race)}
  missings_summary(df)
  
  if (method == "complete_cases"){df <- na.omit(df)}
  else if (method == "ffill") {df <- df %>% group_by(race) %>% arrange(year) %>% fill(number, income_mean, income_mean_moe) %>% ungroup()}
  else if (method=="bfill") {df <- df %>% group_by(race) %>% arrange(year) %>% fill(number, income_mean, income_mean_moe, .direction="up") %>% ungroup()}
  else {;}
  return(df)
}

# as we have no missing values: no need to do anything here
handle_missings_home <- function(df, method="identity", implicits=FALSE)
{
  if (implicits) {df <- df %>% complete(year, race)}
  missings_summary(df)
  return(df)
}

get_ts <- function(df)
{
  df <- df %>% group_by(year,race) %>% 
    summarize(yearly_mean=first(income_mean), yearly_median=first(income_median), yearly_number=first(number)) %>% 
    ungroup() 
  return(df)
  #return(spread(df, key=race, value=yearly_mean))
}




