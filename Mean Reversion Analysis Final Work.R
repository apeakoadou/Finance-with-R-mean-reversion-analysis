# Install packages
install.packages(c("tidyverse", "ggplot2", "dplyr", "readr"))

# Load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)



# Import data set
df_germany <- read.csv(file = "germany.csv", header = TRUE, row.names = 1)


# Select columns with numerical values only
num_cols <- sapply(df_germany, is.numeric)
df_germany_numeric <- df_germany[, num_cols]

# Convert to matrix
df_germany <- as.matrix(df_germany_numeric)

# Subset the matrix to remove columns with missing values
df <- df_germany[, which(colSums(is.na(df_germany)) == 0)]

# Remove columns containing the word "ERROR"
df_clean <- df[, !grepl("ERROR", colnames(df))]


# Find columns that have outliers
outlier_cols <- apply(df_clean, 2, function(x) any(x > 100 | x < -100))

# Remove columns that have outliers
df_clean <- df_clean[, !outlier_cols]




# Creating the data sets used in the experiment 
base_year_2008 <- df_clean[1:10, ]

base_year_2009 <- df_clean[2:11, ]

base_year_2010 <- df_clean[3:12, ]

base_year_2011 <- df_clean[4:13, ]

base_year_2012 <- df_clean[5:14, ]




mean_reversion_analysis <- function(base_year_x, num_portfolios = 5) {
  # Calculate the total number of companies and the portfolio size
  num_companies <- ncol(base_year_x)
  portfolio_size <- floor(num_companies / num_portfolios)
  last_portfolio_size <- num_companies - (portfolio_size * (num_portfolios - 1))
  
  # Rank the firms based on their ROE in the base year
  base_year <- rownames(base_year_x)[1]
  roe_base_year <- base_year_x[base_year, ]
  ranked_companies <- order(roe_base_year, decreasing = TRUE)
  
  # Assign each firm to a portfolio based on their ranking
  portfolio_assignments <- rep(1:num_portfolios, each = portfolio_size)
  portfolio_assignments[(num_companies - last_portfolio_size + 1):num_companies] <- num_portfolios
  
  # Create a matrix to store the portfolio assignments for each year
  portfolio_assignments_matrix <- matrix(NA, nrow = nrow(base_year_x), ncol = num_companies)
  portfolio_assignments_matrix[1, ranked_companies] <- portfolio_assignments
  
  # Assign companies to portfolios in subsequent years based on their assignments in the base year
  for (i in 2:nrow(base_year_x)) {
    roe_year <- base_year_x[i, ]
    ranked_companies <- order(roe_year, decreasing = TRUE)
    portfolio_assignments_matrix[i, ranked_companies] <- portfolio_assignments_matrix[i-1, ranked_companies]
  }
  
  # Initialize a data frame to store the results
  portfolio_names <- paste0("Portfolio ", 1:num_portfolios)
  result_df <- data.frame(year = rep(rownames(base_year_x), each = num_portfolios),
                          portfolio_names = rep(portfolio_names, times = nrow(base_year_x)),
                          avg_roe = NA)
  
  #result_df <- data.frame(year = paste0(rep(paste0("Dataset_", i), each = num_portfolios), 
  #rep(rownames(base_year_2008), times = num_portfolios)),
  #portfolio_names = rep(portfolio_names, times = nrow(base_year_2008)),
  #avg_roe = NA)
  
  # Calculate the average ROE of firms in each portfolio for each year
  for (i in 1:num_portfolios) {
    for (j in 1:nrow(base_year_x)) {
      portfolio_indices <- which(portfolio_assignments_matrix[j, ] == i)
      portfolio_roe <- base_year_x[j, portfolio_indices]
      avg_roe <- mean(portfolio_roe, na.rm = TRUE)
      result_df[(i-1)*nrow(base_year_x)+j, "avg_roe"] <- avg_roe
    }
  }
  
  return(result_df)
}



#First Experiment
exp1 <- mean_reversion_analysis(base_year_2008, num_portfolios = 5)
exp1$year_id <- rep(1:10, each=5)
exp1$experiment <- "exp1"

#Second Experiment 
exp2 <- mean_reversion_analysis(base_year_2009, num_portfolios = 5)
exp2$year_id <- rep(1:10, each=5)
exp2$experiment <- "exp2"

#Third Experiment
exp3 <- mean_reversion_analysis(base_year_2010, num_portfolios = 5)
exp3$year_id <- rep(1:10, each=5)
exp3$experiment <- "exp3"

#Fourth Experiment
exp4 <- mean_reversion_analysis(base_year_2011, num_portfolios = 5)
exp4$year_id <- rep(1:10, each=5)
exp4$experiment <- "exp4"

#Fifth Experiment 
exp5 <- mean_reversion_analysis(base_year_2012, num_portfolios = 5)
exp5$year_id <- rep(1:10, each=5)
exp5$experiment <- "exp5"

# Combine experiments into one data frame, ordered by experiment and year
combined_df <- rbind(exp1, exp2, exp3, exp4, exp5)
combined_df <- combined_df[order(combined_df$experiment, combined_df$year),]


Test <- combined_df %>% group_by(portfolio_names, year_id) %>% summarise( newmean = mean(avg_roe, na.rm = TRUE))

library(ggplot2)

# Create the plot
ggplot(Test, aes(x = year_id, y = newmean, color = portfolio_names)) +
  geom_line() +
  scale_color_discrete(name = "Portfolio") +
  labs(x = "Years", y = "Average ROE Rate") +
  ggtitle("Portfolio Performance over Time")

