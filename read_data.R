### import libraries and source project files used for computation
library(tidyverse)
library(scales)
library(RColorBrewer)
library(DescTools)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source(".//helper.R")
source(".//stats.R")
source(".//transforms.R")
source(".//model.R")
source(".//graphics.R")


### read data sources and join
path_base <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/"
df_inc_dist <- readr::read_csv(paste(path_base, 'income_distribution.csv', sep=""))
df_home <- readr::read_csv(paste(path_base, 'home_owner.csv', sep=""))


### define some global variables
LVLS <- c("Under $15,000", "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999",
         "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 to $199,999", "$200,000 and over")

REF_LVL <- "$150,000 to $199,999"

#colors assigned to races for plotting
color_map <- c(
  "All Races" = "#7f8c8d",           # Neutral grey
  "White Alone" = "#2980b9",          # Bright blue
  "Black Alone" = "#2c3e50",          # Dark blue-grey
  "White Alone, Not Hispanic" = "#5dade2",  # Sky blue
  "Hispanic (Any Race)" = "#d35400",  # Dark orange
  "Asian Alone" = "#1e8449",          # Dark green
  "Black Alone or in Combination" = "#34495e",  # Slate blue
  "Asian Alone or in Combination" = "#2ecc71"   # Bright green
)

# define shorter labels to use for plots
labels = c("All Races" = "All",
           "White Alone" = "White",
           "Black Alone" = "Black",
           "White Alone, Not Hispanic" = "White (Non-Hispanic)",
           "Hispanic (Any Race)" = "Hispanic",
           "Asian Alone" = "Asian",
           "Black Alone or in Combination" = "Black (Combination)",
           "Asian Alone or in Combination" = "Asian (Combination)")

### preprocess and deal with missings
df_inc_dist <- df_inc_dist %>% preprocess_inc() %>% handle_missings_inc(method="bfill")
df_home <- df_home %>% preprocess_home() %>% handle_missings_home(method="bfill", implicits=TRUE)

# join with home data 
# TODO: use home data for further analysis
by <- join_by(year, race_coarse == race)
df_all <- df_inc_dist %>% full_join(df_home, by=by) %>% 
  filter(race %in% c("All Races", "White Alone", "Black Alone", "Hispanic (Any Race)", "Asian Alone"))

# Get an impression on yearly growth rates by race -> almost no difference 1-2%
options(pillar.sigfig = 4)
get_ts(df_all) %>% group_by(race) %>% 
  summarize(gmean=Gmean(yearly_mean/lag(yearly_mean), na.rm=TRUE)) %>% arrange(desc(gmean))


########################################################


########################################################
# (1) Create a plot of income over time by race and add simple trend lines

df_lm <- avg_slope(df_all)
# all_races <- subset(df_all, race=="All Races")
# overall_lm <- lm(income_mean ~ year , data=all_races) # get the overall trend coefficient

# Create and save plot of mean income over time by race
jpeg("income_by_year_race.jpg", width = 1920, height = 1080, res = 300)

plot_inc <- ggplot(df_all, aes(x = year, y = income_mean, color = race)) +
  geom_line(linewidth=1.2) + 
  labs(title = "Einkommensentwicklung nach Ethnizität", x = "Jahr", 
       y = "Mittleres Einkommen ($)", color="Ethnizität") +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(1970, 2020, by = 10)) +
  geom_abline(data = df_lm, 
              aes(intercept = intercept, slope = slope, color = race), 
              alpha=0.6, linewidth=1.2, linetype="dashed") + 
  # add dates of some major crisis (lehmann, 9/11, oil)
  # TODO: highlight with arrows
  geom_vline(xintercept = 2009, linetype = "dotted", color = "#8B0000", linewidth = 1) + 
  geom_vline(xintercept = 2001, linetype = "dotted", color = "#8B0000", linewidth = 1) +
  geom_vline(xintercept = 1973, linetype = "dotted", color = "#8B0000", linewidth = 1) +
  scale_color_manual(values = color_map,
                     labels = labels) 

print(plot_inc)
dev.off() # plot end
########################################################


########################################################
# (2) Measure income inequality within each race and plot over time
# Use midpoints of the bracket intervals
df_all$mid <- extract_midpoint(df_all$income_bracket, add = 100000)
df_gr_ordered <- df_all %>% group_by(year, race) %>% arrange(mid)
df_gini <- df_gr_ordered %>% summarize(gini_coef=Gini(mid, income_distribution), .groups="drop") %>% 
  filter(race %in% c("Black Alone", "White Alone", "Asian Alone", "Hispanic (Any Race)"))

# Create and save plot of gini-coefficient over time by race
jpeg("gini.jpg", width = 1920, height = 1080, res = 300)

plot_gini <- ggplot(df_gini, aes(x = year, y = gini_coef, color = race, linetype = race)) +
  geom_line(linewidth=1.2) + 
  labs(title = "Trend des Gini-Koeffizienten nach Ethnizität", x = "Jahr", 
       y = "Gini-Koeffizient", color="Ethnizität") +
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(1970, 2020, by = 10)) +
  geom_vline(xintercept = 2009, linetype = "dotted", color = "#8B0000", linewidth = 1) + 
  geom_vline(xintercept = 2001, linetype = "dotted", color = "#8B0000", linewidth = 1) +
  geom_vline(xintercept = 1973, linetype = "dotted", color = "#8B0000", linewidth = 1) +
  scale_color_manual(values = color_map,
                   labels = labels) +
  guides(
    color = guide_legend(title = "Ethnizität"),
    linetype = "none"  
  )

print(plot_gini)
dev.off() # plot end
##########################################################


##########################################################
# (3) Estimate a simple "black-scholes-type"-model for income over time
check_log_returns(df_all) # compute yearly log returns and check for i.i.d normality assumptions
model_estimates <- estim_b_scholes(df_all) %>% arrange(desc(sigma2))

# rename races before:
model_estimates <- model_estimates %>% mutate(race = str_extract(race, "^[^\\s]+"))
# save table for presentation
save_as_png(model_estimates %>% rename(Asian=))
##########################################################


##########################################################

# (4) Percentage exceeding predefined income_level
jpeg("pct_richest.jpg", width = 1920, height = 1080, res = 300)

df_inc_above <- df_all %>% income_above(income_level=REF_LVL)

plot_inc_above <- ggplot(df_inc_above, aes(x = year, y = higher_thresh, color = race)) +
  geom_line(linewidth=1.5) + 
  labs(title = "Entwicklung des Anteils mit Einkommen mehr als 200,000 $", x = "Jahr", 
       y = "Anteil (in %)", color="Ethnizität") +
  #labs(title = "Mean income by Race and Year", x = "Year", y = "Mean income") +
  scale_y_continuous(labels = comma, limits = c(0, 25)) +
  scale_x_continuous(breaks = seq(1970, 2020, by = 10)) +
  geom_vline(xintercept = 2009, linetype = "dotted", color = "#8B0000", linewidth = 1) + 
  geom_vline(xintercept = 2001, linetype = "dotted", color = "#8B0000", linewidth = 1) +
  geom_vline(xintercept = 1973, linetype = "dotted", color = "#8B0000", linewidth = 1) +
  scale_color_manual(values = color_map,
                     labels = labels) 
print(plot_inc_above)
dev.off()






