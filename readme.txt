The main script is located in the read_data.R file. Basically four types of analysis are carried out:
1) Plot of mean income over time by race 
2) Computation and plot of (approximate) gini coefficients over time by race
3) Plot of percentage with income above 200K for each race by time
4) Modeling of (mean) income over year by Black Scholes type model (bs-model)

Other files contain functions called in read_data grouped by functionality:

- helper.R: preprocessing functions of raw data - including missing data handling
- transforms.R: for the computation of additional variables derived from the raw data
- stats.R: Commonly used simple statistical approaches for time series analysis (here: regression line fitting and acf-function
- model.R: checks of assumptions underlying the bs-model; computation of parameter estimates (i.e. drift and volatitlity) 
- graphics.R: function to extract the table of parameter estimates into an image file (for use in ppt presentation)