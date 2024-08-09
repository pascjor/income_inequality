library(tidyverse)
library(scales)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source(".//helper.R")

# define some global variables
LVLS <- c("Under $15,000", "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999",
         "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 to $199,999", "$200,000 and over")

REF_LVL <- "$100,000 to $149,999"

# read data (save?)
df_inc_dist <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
df_home <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')

# preprocess and deal with missings
df_inc_dist <- df_inc_dist %>% preprocess_inc() %>% handle_missings_inc(method="bfill")
df_home <- df_home %>% preprocess_home() %>% handle_missings_home(method="bfill", implicits=TRUE)
by <- join_by(year, race_coarse == race)
df_all <- df_inc_dist %>% full_join(df_home, by=by) #by=c("year","race_coarse"))


ggplot(df_all, aes(x = year, y = income_mean, color = race)) +
  geom_line(size=1.5) + 
  labs(title = "Mean income by Race and Year", x = "Year", y = "Mean income") +
  ylim(0, NA) + scale_y_continuous(labels = comma, limits = c(0, NA))


