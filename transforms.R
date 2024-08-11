### Compute additional variables from the raw data - useful for further analysis steps

library(tidyverse)
# compute the pct of population with income above the given income_level 
# (for each year/race combination)
income_above <- function(df, income_level)
{
  if (!(income_level %in% LVLS)){stop("Invalid income level")}
  newcol <- paste("higher",income_level)
  df[newcol] <- ifelse(df$income_bracket > income_level,1,0)
  df = df %>% group_by(year, race) %>% 
    summarize(higher_thresh = sum(income_distribution*.data[[newcol]]))
  return(df %>% ungroup())
}

# alternative modeling approach: Use income ratios relative to a fixed reference group
normalized_ratios <- function(df, reference_stat="mean", reference_group="White Alone")
{
  ref_col  <- paste("income", reference_stat, sep="_")
  df_ref <- df %>% filter(race==reference_group) %>%
    group_by(year) %>%
    summarize(ref_value = first(.data[[ref_col]]), .groups = 'drop')
  df_combined <- full_join(df, df_ref, by=c("year"))
  df_combined["ratio"] = df_combined[,ref_col]/df_combined[,"ref_value"]
  return(df_combined)
}

# computes returns w.r.t. column "colname" within each race group
yearly_returns <- function(df, colname, log=FALSE)
{
  df <- df %>% group_by(race) %>%
    mutate(return = .data[[colname]] / lag(.data[[colname]])) %>%
    replace_na(list(return = 0))
  if (log) {df$return <- log(df$return)} # TODO: handle first value (Inf)
  return(df)
}

# takes the vector of brackets and computes the midpoints as a numeric variable
# For the extreme interval it takes the lower end and adds "add" to it.
extract_midpoint <- function(brackets, add=0) {
  brackets <- str_replace_all(brackets, ",", "") %>% 
    str_replace_all("Under", "$0 ") # replace "Under $15000" with "$0 to $15000"
  lwr_upper <- str_extract_all(brackets, "\\d+")
  midpoints <- lapply(lwr_upper, function(x) {
    if (length(x) == 2) {
      mean(as.numeric(x))  
    } else {
      as.numeric(x) + add  
    }
  })
  return(unlist(midpoints))
}
