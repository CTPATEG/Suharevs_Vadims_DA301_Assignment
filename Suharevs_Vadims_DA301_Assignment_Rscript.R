## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

########################## Link to the GitHUB #################################


# https://github.com/CTPATEG/Suharevs_Vadims_DA301_Assignment


###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 0. Working Directory

# 0.1 Determine the working directory.
getwd() 

# 0.2 Select the current directory to work with
# (below line is '#' for the purposes of the submission).
# setwd(dir = '/Users/VS/Desktop/LSE/3 Course/Assignment 3/Data')

# 0.3 Double-check the working directory.
getwd()

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages('tidyverse')
install.packages('tidyr')

# Import package.
suppressWarnings(library(tidyverse))
# library(tidyverse)

# Import the data set.
turtle_sales <- read.csv('turtle_sales.csv', header=TRUE)

# Print the data frame.
head(turtle_sales)
View(turtle_sales)

# Sense-check the data set
# Return the structure of the data frame.
str(turtle_sales)
glimpse(turtle_sales)

# Check the type of the data frame.
typeof(turtle_sales)

# Check the class of the data frame.
class(turtle_sales)

# Check the dimensions of the data frame
dim(turtle_sales)

# View tibble of the data frame.
as_tibble(turtle_sales)

# View a summary (descriptive statistics) of the data frame.
summary(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns (Ranking, Year, Genre, Publisher). 
turtle_sales_new <- select(turtle_sales, 
                           -Ranking, -Year, -Genre, -Publisher)


# View the new data frame.
head(turtle_sales_new)


# View the descriptive statistics.
summary(turtle_sales_new)


### Product to character mutate from the number format
# Convert 'Product' to character/text (categorical variable).
turtle_sales_new <- mutate(turtle_sales_new,
                 Product = as.character(Product))

# To search for missing values in a data set.
turtle_sales_new[is.na(turtle_sales_new)]
sum(is.na(turtle_sales_new))

# View the descriptive statistics, new.
summary(turtle_sales_new)

# Export the data as a CSV file for future use.
write_csv(turtle_sales_new, file='turtle_sales_new.csv')


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

# Global sales by Platform with jitter
qplot(Global_Sales, Platform, color=Product, 
      data=turtle_sales_new, geom=c('point', 'jitter'))

# Global sales by Platform with jitter
qplot(Global_Sales, Platform, 
      data=turtle_sales_new, geom=c('point', 'jitter'))

# Global sales by Product
qplot(Product, Global_Sales, color=Platform, data=turtle_sales_new)

# Global sales compared against NA sales
qplot(NA_Sales, Global_Sales, color=Platform, data=turtle_sales_new)

# Global sales compared against EU sales
qplot(EU_Sales, Global_Sales, color=Platform, data=turtle_sales_new)

# EU sales compared against NA sales
qplot(EU_Sales, NA_Sales, color=Platform, data=turtle_sales_new)

## 2b) Histograms
# Create histograms.

# Global sales frequency distribution
qplot(Global_Sales, bins=20, data=turtle_sales_new)

# NA sales frequency distribution
qplot(NA_Sales, bins=20, data=turtle_sales_new)

# EU sales frequency distribution
qplot(EU_Sales, bins=20, data=turtle_sales_new)


## 2c) Boxplots
# Create boxplots.

# Boxplot to check Product against Global Sales
qplot(Product, Global_Sales, data=turtle_sales_new, geom='boxplot')

# Boxplot to check Platform against Global Sales
qplot(Platform, Global_Sales, data=turtle_sales_new, geom='boxplot')

# Boxplot to check Platform against NA Sales
qplot(Platform, NA_Sales, data=turtle_sales_new, geom='boxplot')

# Boxplot to check Platform against EU Sales
qplot(Platform, EU_Sales, data=turtle_sales_new, geom='boxplot')

# Boxplot to check Global Sales
qplot(Global_Sales, Global_Sales, data=turtle_sales_new, geom='boxplot')

## 2d)
# Create barplots.

# Barplot for Platform
qplot(Platform, data=turtle_sales_new, geom='bar')

# Horisontal Barplot for Platform by Product
qplot(Global_Sales, Platform, fill=Product, data=turtle_sales_new, geom='col')

# Barplot for Product
qplot(Product, data=turtle_sales_new, geom='bar')

# Barplot for Product by Platform
qplot(Product, Global_Sales, fill=Platform, data=turtle_sales_new, geom='col')


###############################################################################

## 2e) Use the base dataset to check for Genre / Publisher for Global Sales

# Scatter for removed columns

# Global sales by Genre
qplot(Global_Sales, Genre, color=Product, 
      data=turtle_sales, geom=c('point', 'jitter'))

# Global sales by Publisher
qplot(Global_Sales, Publisher, color=Product, 
      data=turtle_sales, geom=c('point', 'jitter'))

# Box for removed columns

# Global sales by Genre
qplot(Genre, Global_Sales, data=turtle_sales, geom='boxplot')

# Global sales by Publisher
qplot(Global_Sales, Publisher, data=turtle_sales, geom='boxplot')

# Bar for removed columns
# Barplot for Genre
qplot(Genre, data=turtle_sales)

# Barplot for Publisher
qplot(Publisher, data=turtle_sales)

# Barplot for Global Sales by Publisher and Genre
qplot(Global_Sales, Publisher, fill=Genre, data=turtle_sales, geom='col')

# Barplot for Global Sales by Genre and Publisher
qplot(Global_Sales, Genre, fill=Publisher, data=turtle_sales, geom='col')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

# The initial insights show that there are clear outliers present 
# and there are quite a few of them.

# There is a wide spread of sales across different platforms

# The data is grouped together for kurtosis and histogram shows skew
# on the lower side of sales for Global sales

# Additional insights could be gained from the variables that were
# requested by the Turtle Sales team to be removed, i.e. Genre or Publisher


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
# Import package.
suppressWarnings(library(tidyverse))
suppressWarnings(library(dplyr))

# Import the data set.
turtle_sales_new <- read.csv('turtle_sales_new.csv', header=TRUE)

# Print the data frame.
head(turtle_sales_new)
View(turtle_sales_new)

# Check output: Determine the min, max, and mean values.
# Determine the minimum, maximum, mean for NA_Sales.
min(turtle_sales_new$NA_Sales)
max(turtle_sales_new$NA_Sales)
mean(turtle_sales_new$NA_Sales)

# Determine the variance and standard deviation for NA_Sales.
var(turtle_sales_new$NA_Sales)
sd(turtle_sales_new$NA_Sales)

# Determine the minimum, maximum, mean value for EU_Sales.
min(turtle_sales_new$EU_Sales)
max(turtle_sales_new$EU_Sales)
mean(turtle_sales_new$EU_Sales)

# Determine the variance and standard deviation for EU_Sales.
var(turtle_sales_new$EU_Sales)
sd(turtle_sales_new$EU_Sales)

# Determine the minimum, maximum, mean value for Global_Sales.
min(turtle_sales_new$Global_Sales)
max(turtle_sales_new$Global_Sales)
mean(turtle_sales_new$Global_Sales)

# Determine the variance and standard deviation for Global_Sales.
var(turtle_sales_new$Global_Sales)
sd(turtle_sales_new$Global_Sales)

# View the descriptive statistics.
summary(turtle_sales_new)

### Product to Text from number
# Convert 'Product' to character/text (categorical variable).
turtle_sales_new <- mutate(turtle_sales_new,
                           Product = as.character(Product))

# View the descriptive statistics again.
summary(turtle_sales_new)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

##########

# Aggregate Function
aggregate(Global_Sales~Product, turtle_sales_new, sum)

# Aggregate by Global Sales
turtle_sales_agg <- aggregate(Global_Sales~Product, turtle_sales_new, sum)

##########

# Group_By Totals
group_by(turtle_sales_new) %>% summarise(NA_Total=sum(NA_Sales), 
                                         EU_Total=sum(EU_Sales), 
                                         Global_Total=sum(Global_Sales))

# Group_By Detail by Product for Global Sales, NA and EU
turtle_sales_group <- turtle_sales_new %>%  group_by(Product) %>% 
  summarise(NA_Total=sum(NA_Sales), 
            EU_Total=sum(EU_Sales), 
            Global_Total=sum(Global_Sales), 
            .groups='drop')


# View the data frame.
head(turtle_sales_agg)
head(turtle_sales_group)

# Explore the data frame.
summary(turtle_sales_agg)
summary(turtle_sales_group)

write_csv(turtle_sales_group, file='turtle_sales_group.csv')

## 2b) Determine which plot is the best to compare game sales.

# Create Scatter plots.

# Scatter plot for Product vs Global sales
# smoothing line makes no sense on this scatter plot as the Product
# is a categorical variable
ggplot(data = turtle_sales_group,
       mapping = aes(x = Product, y = Global_Total)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) + 
  scale_y_continuous(breaks = seq(0, 70, 5)) +
  labs(title = "Relationship between Product and Global Sales",
       subtitle = "Turtle Games Case Study",
       #  Add labels to labs function.
       caption = "Source: Turtle Games Database",
       x = "Product",
       y = "Global Sales") +
  # Add theme and remove x-labels
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Scatter plot for aggregated Global sales vs NA sales
ggplot(data = turtle_sales_group,
       mapping = aes(x = NA_Total, y = Global_Total)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) + 
  scale_y_continuous(breaks = seq(0, 70, 5)) +
  labs(title = "Relationship between NA Sales and Global Sales",
       subtitle = "Turtle Games Case Study",
       #  Add labels to labs function.
       caption = "Source: Turtle Games Database",
       x = "NA Sales",
       y = "Global Sales") +
  theme_bw()

# Scatter plot for aggregated Global sales vs EU sales
ggplot(data = turtle_sales_group,
       mapping = aes(x = EU_Total, y = Global_Total)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) + 
  scale_y_continuous(breaks = seq(0, 70, 5)) +
  labs(title = "Relationship between EU Sales and Global Sales",
       subtitle = "Turtle Games Case Study",
       #  Add labels to labs function.
       caption = "Source: Turtle Games Database",
       x = "EU Sales",
       y = "Global Sales") +
  theme_bw()

# Scatter plot for aggregated EU sales vs NA sales
ggplot(data = turtle_sales_group,
       mapping = aes(x = EU_Total, y = NA_Total)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) + 
  scale_y_continuous(breaks = seq(0, 70, 5)) +
  labs(title = "Relationship between EU Sales and NA Sales",
       subtitle = "Turtle Games Case Study",
       #  Add labels to labs function.
       caption = "Source: Turtle Games Database",
       x = "EU Sales",
       y = "NA Sales") +
  theme_bw()


# Create histograms.

# Basic Histogram for Global Total
hist(turtle_sales_group$Global_Total)

# Advanced Histogram for Global Total
ggplot(turtle_sales_group, aes(x = Global_Total)) +
  geom_histogram(fill = 'red', color = 'black') +
  labs(x = "Global Sales",
       y = "Frequency",
       title = "Global Sales Frequency Distribution",
       subtitle = "Turtle Games Case Study",
       caption = "Source: Turtle Games Database")

# Basic Histogram for NA Total
hist(turtle_sales_group$NA_Total)

# Advanced Histogram for NA Total
ggplot(turtle_sales_group, aes(x = NA_Total)) +
  geom_histogram(fill = 'red', color = 'black') +
  labs(x = "NA Sales",
       y = "Frequency",
       title = "NA Sales Frequency Distribution",
       subtitle = "Turtle Games Case Study",
       caption = "Source: Turtle Games Database")

# Basic Histogram for EU Total
hist(turtle_sales_group$EU_Total)

# Advanced Histogram for EU Total
ggplot(turtle_sales_group, aes(x = EU_Total)) +
  geom_histogram(fill = 'red', color = 'black') +
  labs(x = "EU Sales",
       y = "Frequency",
       title = "EU Sales Frequency Distribution",
       subtitle = "Turtle Games Case Study",
       caption = "Source: Turtle Games Database")

# Create boxplots.

# Basic Boxplot for Global Total
boxplot(turtle_sales_group$Global_Total)

# Advanced Boxplot for Global Total
ggplot(turtle_sales_group, aes(x = Global_Total, y = Global_Total)) +
  geom_violin(fill = 'purple') +
  geom_boxplot(fill = 'orange',
               outlier.color = 'orange', outlier.size = 1,
               outlier.shape = 'square') +
  labs(x = "Global Sales",
       y = "Global Sales",
       title = "Global Sales Summary",
       subtitle = "Turtle Games Case Study",
       caption = "Source: Turtle Games Database")

# Advanced Boxplot for Global Total
ggplot(turtle_sales_group, aes(x = Global_Total, y = Global_Total)) +
  geom_boxplot(fill='purple', width = 1, notch=TRUE,
               outlier.color='blue', outlier.size = 1) +
  labs(x = "Global Sales",
       y = "Frequency",
       title = "Global Sales Summary",
       subtitle = "Turtle Games Case Study",
       caption = "Source: Turtle Games Database") 

# Basic Boxplot for NA Total
boxplot(turtle_sales_group$NA_Total)

# Advanced Boxplot for NA Total
ggplot(turtle_sales_group, aes(x = NA_Total, y = NA_Total)) +
  geom_violin(fill = 'purple') +
  geom_boxplot(fill = 'orange',
               outlier.color = 'orange', outlier.size = 1,
               outlier.shape = 'square') +
  labs(x = "NA Sales",
       y = "NA Sales",
       title = "NA Sales Summary",
       subtitle = "Turtle Games Case Study",
       caption = "Source: Turtle Games Database")

# Basic Boxplot for EU Total
boxplot(turtle_sales_group$EU_Total)

# Advanced Boxplot for EU Total
ggplot(turtle_sales_group, aes(x = EU_Total, y = EU_Total)) +
  geom_violin(fill = 'purple') +
  geom_boxplot(fill = 'orange',
               outlier.color = 'orange', outlier.size = 1,
               outlier.shape = 'square') +
  labs(x = "EU Sales",
       y = "EU Sales",
       title = "EU Sales Summary",
       subtitle = "Turtle Games Case Study",
       caption = "Source: Turtle Games Database")


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

# QQ Global Total
# Draw qqplot using Global Total data
qqnorm(turtle_sales_group$Global_Total,
       col='blue',
       xlab="z Value",
       ylab='Global Sales')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales_group$Global_Total,
       col='red',
       lwd=2)

# QQ NA Total
# Draw qqplot using NA Total data
qqnorm(turtle_sales_group$NA_Total,
       col='blue',
       xlab="z Value",
       ylab='NA Sales')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales_group$NA_Total,
       col='red',
       lwd=2)

# QQ EU Total
# Draw qqplot using EU Total data
qqnorm(turtle_sales_group$EU_Total,
       col='blue',
       xlab="z Value",
       ylab='EU Sales')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales_group$EU_Total,
       col='red',
       lwd=2)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments')
library('moments')

# Perform Shapiro-Wilk test.
shapiro.test(turtle_sales_group$Global_Total)
shapiro.test(turtle_sales_group$NA_Total)
shapiro.test(turtle_sales_group$EU_Total)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis Global.
skewness(turtle_sales_group$Global_Total)
kurtosis(turtle_sales_group$Global_Total)

# Skewness and Kurtosis NA.
skewness(turtle_sales_group$NA_Total)
kurtosis(turtle_sales_group$NA_Total)

# Skewness and Kurtosis EU.
skewness(turtle_sales_group$EU_Total)
kurtosis(turtle_sales_group$EU_Total)

## 3d) Determine correlation
# Determine correlation Global vs NA, Global vs EU, NA vs EU.
cor(turtle_sales_group$Global_Total, turtle_sales_group$NA_Total)
cor(turtle_sales_group$Global_Total, turtle_sales_group$EU_Total)
cor(turtle_sales_group$NA_Total, turtle_sales_group$EU_Total)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Filter out up to 0.10 for the games which have flopped if any
# Filter out the three blockbuster games which skew the data and hinder normality
# and should be separated out of the general cohort as the blockbuster games
# can be deemed to be in the league of their own and should be compared to
# the similar best sellers. 3 Blockbusters removed to create a new dataframe.

turtle_sales_filtered <- filter(turtle_sales_group, 
                                Global_Total>0.09,
                                Global_Total<30)

write_csv(turtle_sales_filtered, file='turtle_sales_filtered.csv')

# QQ Global Total
# Draw qqplot using Global Total data
qqnorm(turtle_sales_filtered$Global_Total,
       col='blue',
       xlab="z Value",
       ylab='Global Sales')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales_filtered$Global_Total,
       col='red',
       lwd=2)

# QQ NA Total
# Draw qqplot using NA Total data
qqnorm(turtle_sales_filtered$NA_Total,
       col='blue',
       xlab="z Value",
       ylab='NA Sales')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales_filtered$NA_Total,
       col='red',
       lwd=2)

# QQ EU Total
# Draw qqplot using EU Total data
qqnorm(turtle_sales_filtered$EU_Total,
       col='blue',
       xlab="z Value",
       ylab='EU Sales')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales_filtered$EU_Total,
       col='red',
       lwd=2)


# Perform Shapiro-Wilk test.
shapiro.test(turtle_sales_filtered$Global_Total)
shapiro.test(turtle_sales_filtered$NA_Total)
shapiro.test(turtle_sales_filtered$EU_Total)

# Skewness and Kurtosis Global.
skewness(turtle_sales_filtered$Global_Total)
kurtosis(turtle_sales_filtered$Global_Total)

# Skewness and Kurtosis NA.
skewness(turtle_sales_filtered$NA_Total)
kurtosis(turtle_sales_filtered$NA_Total)

# Skewness and Kurtosis EU.
skewness(turtle_sales_filtered$EU_Total)
kurtosis(turtle_sales_filtered$EU_Total)

cor(turtle_sales_filtered$Global_Total, turtle_sales_filtered$NA_Total)
cor(turtle_sales_filtered$Global_Total, turtle_sales_filtered$EU_Total)
cor(turtle_sales_filtered$NA_Total, turtle_sales_filtered$EU_Total)

# Check the dimensions of the data frame
dim(turtle_sales_filtered)

###############################################################################

turtle_sales_filtered_2 <- filter(turtle_sales_group, 
                                Global_Total>0.09,
                                Global_Total<20)

write_csv(turtle_sales_filtered_2, file='turtle_sales_filtered_2.csv')

# QQ Global Total
# Draw qqplot using Global Total data
qqnorm(turtle_sales_filtered_2$Global_Total,
       col='blue',
       xlab="z Value",
       ylab='Global Sales')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales_filtered_2$Global_Total,
       col='red',
       lwd=2)

# QQ NA Total
# Draw qqplot using NA Total data
qqnorm(turtle_sales_filtered_2$NA_Total,
       col='blue',
       xlab="z Value",
       ylab='NA Sales')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales_filtered_2$NA_Total,
       col='red',
       lwd=2)

# QQ EU Total
# Draw qqplot using EU Total data
qqnorm(turtle_sales_filtered_2$EU_Total,
       col='blue',
       xlab="z Value",
       ylab='EU Sales')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(turtle_sales_filtered_2$EU_Total,
       col='red',
       lwd=2)


# Perform Shapiro-Wilk test.
shapiro.test(turtle_sales_filtered_2$Global_Total)
shapiro.test(turtle_sales_filtered_2$NA_Total)
shapiro.test(turtle_sales_filtered_2$EU_Total)

# Skewness and Kurtosis Global.
skewness(turtle_sales_filtered_2$Global_Total)
kurtosis(turtle_sales_filtered_2$Global_Total)

# Skewness and Kurtosis NA.
skewness(turtle_sales_filtered_2$NA_Total)
kurtosis(turtle_sales_filtered_2$NA_Total)

# Skewness and Kurtosis EU.
skewness(turtle_sales_filtered_2$EU_Total)
kurtosis(turtle_sales_filtered_2$EU_Total)

cor(turtle_sales_filtered_2$Global_Total, turtle_sales_filtered_2$NA_Total)
cor(turtle_sales_filtered_2$Global_Total, turtle_sales_filtered_2$EU_Total)
cor(turtle_sales_filtered_2$NA_Total, turtle_sales_filtered_2$EU_Total)

# Check the dimensions of the data frame
dim(turtle_sales_filtered_2)

# Tested the different filtering between 20 and 30 for Global Sales
# Based on the Global Sales Boxplot outliers visualisation
# But decided to keep with the filter out 3 most obvious Blockbuster sales

######################## do the final graphs here

# Scatter Plots

# NA vs Global
ggplot(data = turtle_sales_filtered,
       mapping = aes(x = NA_Total, y = Global_Total)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) + 
  scale_y_continuous(breaks = seq(0, 70, 5)) +
  labs(title = "Relationship between NA Sales and Global Sales",
       subtitle = "Turtle Games Case Study",
       #  Add labels to labs function.
       caption = "Source: Turtle Games Database",
       x = "NA Sales",
       y = "Global Sales") +
  theme_bw()

# EU vs Global
ggplot(data = turtle_sales_filtered,
       mapping = aes(x = EU_Total, y = Global_Total)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) + 
  scale_y_continuous(breaks = seq(0, 70, 5)) +
  labs(title = "Relationship between EU Sales and Global Sales",
       subtitle = "Turtle Games Case Study",
       #  Add labels to labs function.
       caption = "Source: Turtle Games Database",
       x = "EU Sales",
       y = "Global Sales") +
  theme_bw()

# EU vs NA
ggplot(data = turtle_sales_filtered,
       mapping = aes(x = EU_Total, y = NA_Total)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) + 
  scale_y_continuous(breaks = seq(0, 70, 5)) +
  labs(title = "Relationship between EU Sales and NA Sales",
       subtitle = "Turtle Games Case Study",
       #  Add labels to labs function.
       caption = "Source: Turtle Games Database",
       x = "EU Sales",
       y = "NA Sales") +
  theme_bw()

# Histograms

# Global
ggplot(turtle_sales_filtered, aes(x = Global_Total)) +
  geom_histogram(fill = 'red', color = 'black') +
  labs(x = "Global Sales",
       y = "Frequency",
       title = "Global Sales Frequency Distribution",
       subtitle = "Turtle Games Case Study",
       caption = "Source: Turtle Games Database")

# NA
ggplot(turtle_sales_filtered, aes(x = NA_Total)) +
  geom_histogram(fill = 'red', color = 'black') +
  labs(x = "NA Sales",
       y = "Frequency",
       title = "NA Sales Frequency Distribution",
       subtitle = "Turtle Games Case Study",
       caption = "Source: Turtle Games Database")

# EU
ggplot(turtle_sales_filtered, aes(x = EU_Total)) +
  geom_histogram(fill = 'red', color = 'black') +
  labs(x = "EU Sales",
       y = "Frequency",
       title = "EU Sales Frequency Distribution",
       subtitle = "Turtle Games Case Study",
       caption = "Source: Turtle Games Database")

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# Filter out the three blockbuster games which skew the data and hinder normality
# and should be separated out of the general cohort as the blockbuster games
# can be deemed to be in the league of their own and should be compared to
# the similar best sellers. 3 Blockbusters removed to create a new dataframe.

# There are quite a lot of the data points at total level above the 1+ z value, 
# as well as below -1 z value - removing all these data points would invalidate
# the whole analysis as too many observations would be removed.

# Several titles above the QQ line on the positive side fall within the 
# blockbuster territory, and skew the entire dataset.

# For a more objective approach towards the analysis it would make sense to
# to slice the data further and discuss the grouping of the different titles
# with the client (Turtle Games).

# An example would be isolating the Top 10 to Top 20 Titles and running 
# a separate analysis for the top performers, as well as running separate 
# analysis for the average performers and also check out the low performers.

# Even though, seemingly, there are substantially more outliers on the positive
# side than the 3 Blockbusters – they still form a portion of the global sales.
# Tested the different filtering between 20 and 30 for global sales based 
# on the global sales boxplot outliers visualisation, but decided to 
# leave them within the dataset.

# Depending on the title and the marketing campaign, or the distribution after
# launch of the product, it would make sense to not only look at the global
# picture, but rather target different regions separately and run the analysis
# separately, which would help working with the outliers specific to 
# a given region. 

# An example, Product 254 – NA Sales 19.03, vs EU Sales of 1.85. If the sales
# were somewhat proportionate, the analysis would have been objective, 
# otherwise, due to demographics, local norms or regulations, preferences, 
# marketing etc the results could be vastly different depending on the region
# and hence could skew the data one way or the other.



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 0. Install potentially useful Packages
# Install the psych package.
install.packages('psych')

# Import the psych package.
suppressWarnings(library(psych))
suppressWarnings(library(tidyverse))

# Set current working directory

# 1. Load and explore the data
# View data frame created in Week 5.
turtle_sales_filtered <- read.csv('turtle_sales_filtered.csv', header=TRUE)

# Print the data frame.
head(turtle_sales_filtered)

# Convert 'Product' to character/text (categorical variable).
#turtle_sales_filtered <- mutate(turtle_sales_filtered,
#                           Product = as.character(Product))

# Determine a summary of the data frame.
summary(turtle_sales_filtered)

# Determine the variance and standard deviation for Global_Total.
var(turtle_sales_filtered$Global_Total)
sd(turtle_sales_filtered$Global_Total)

# Determine the variance and standard deviation for NA_Total
var(turtle_sales_filtered$NA_Total)
sd(turtle_sales_filtered$NA_Total)

# Determine the variance and standard deviation for EU_Total
var(turtle_sales_filtered$EU_Total)
sd(turtle_sales_filtered$EU_Total)

# Correlation for Global vs NA, Global vs EU, EU vs NA
cor(turtle_sales_filtered$Global_Total, turtle_sales_filtered$NA_Total)
cor(turtle_sales_filtered$Global_Total, turtle_sales_filtered$EU_Total)
cor(turtle_sales_filtered$NA_Total, turtle_sales_filtered$EU_Total)

###############################################################################

# 2.0 Simple Linear Regression subjects

# This plot of Product vs Global_Total makes no sense, but testing out the view
plot(turtle_sales_filtered$Product, turtle_sales_filtered$Global_Total)

# NA_Total tested against Global_Total is one of the main test subjects
plot(turtle_sales_filtered$NA_Total, turtle_sales_filtered$Global_Total)

# EU_Total tested against Global_Total is one of the main test subjects
plot(turtle_sales_filtered$EU_Total, turtle_sales_filtered$Global_Total)

# EU_Total tested against NA_Total is just to test the relationship
plot(turtle_sales_filtered$EU_Total, turtle_sales_filtered$NA_Total)

# 2.1 Create a simple linear regression model for NA_Sales vs Global

## 2.1 a) Determine the correlation between columns
# Create a linear regression model on the original data.

# NA_Total tested against Global_Total is one of the main test subjects
plot(turtle_sales_filtered$NA_Total, turtle_sales_filtered$Global_Total)

model1 <- lm(Global_Total ~ NA_Total, data = turtle_sales_filtered)

model1

summary(model1)

plot(model1$residuals)

plot(turtle_sales_filtered$NA_Total, turtle_sales_filtered$Global_Total)
abline(coefficients(model1))

turtle_sales_filtered <- mutate(turtle_sales_filtered,
                       log_Global_Total = log(Global_Total))

model2 <- lm(log_Global_Total ~ NA_Total, data = turtle_sales_filtered)

summary(model2)


## 2.1 b) Create a plot (simple linear regression)
# Basic visualisation.
plot(turtle_sales_filtered$NA_Total, turtle_sales_filtered$log_Global_Total)
abline(coefficients(model2))

##################################

## 2.2 a) Determine the correlation between columns
# Create a linear regression model on the original data.

# EU_Total tested against Global_Total is one of the main test subjects
plot(turtle_sales_filtered$EU_Total, turtle_sales_filtered$Global_Total)

model1 <- lm(Global_Total ~ EU_Total, data = turtle_sales_filtered)

model1

summary(model1)

plot(model1$residuals)

plot(turtle_sales_filtered$EU_Total, turtle_sales_filtered$Global_Total)
abline(coefficients(model1))

turtle_sales_filtered <- mutate(turtle_sales_filtered,
                                log_Global_Total = log(Global_Total))

model2 <- lm(log_Global_Total ~ EU_Total, data = turtle_sales_filtered)

summary(model2)


## 2.2 b) Create a plot (simple linear regression)
# Basic visualisation.
plot(turtle_sales_filtered$EU_Total, turtle_sales_filtered$log_Global_Total)
abline(coefficients(model2))

##################################

## 2.3 a) Determine the correlation between columns
# Create a linear regression model on the original data.

# NA_Total tested against EU_Total is one of the main test subjects
plot(turtle_sales_filtered$EU_Total, turtle_sales_filtered$NA_Total)

model1 <- lm(NA_Total ~ EU_Total, data = turtle_sales_filtered)

model1

summary(model1)

plot(model1$residuals)

plot(turtle_sales_filtered$EU_Total, turtle_sales_filtered$NA_Total)
abline(coefficients(model1))

turtle_sales_filtered <- mutate(turtle_sales_filtered,
                                log_NA_Total = log(NA_Total))

model2 <- lm(log_NA_Total ~ EU_Total, data = turtle_sales_filtered)

summary(model2)


## 2.3 b) Create a plot (simple linear regression)
# Basic visualisation.
plot(turtle_sales_filtered$EU_Total, turtle_sales_filtered$log_NA_Total)
abline(coefficients(model2))

###############################################################################

# 3.1 Create a multiple linear regression model
# Select only numeric columns from the original data frame.

# Import the data set.
turtle_sales <- read.csv('turtle_sales.csv', header=TRUE)

# Print the data frame.
head(turtle_sales)

# Create a new object and specify the lm function and the variables.
model_mlr_original = lm(Global_Sales ~ NA_Sales + EU_Sales, data=turtle_sales)

# Multiple linear regression model.
summary(model_mlr_original)

###############################################################################

# 3.2 Create a multiple linear regression model
# Select only numeric columns from the treated data frame.

cor(turtle_sales_filtered)
corPlot(turtle_sales_filtered, cex=2)

# Create a new object and specify the lm function and the variables.
model_mlr_1 = lm(Global_Total ~ NA_Total + EU_Total, data=turtle_sales_filtered)

# Multiple linear regression model.
summary(model_mlr_1)



###############################################################################

# 4.1 Predictions based on given values for ORIGINAL dataset
# Compare with observed values for a number of records.

# a. NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
Global_Sales_Forecast <- data.frame(NA_Sales=c(34.02), EU_Sales=c(23.80))
predict(model_mlr_original, newdata = Global_Sales_Forecast, 
        interval='confidence')

# b. NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
Global_Sales_Forecast <- data.frame(NA_Sales=c(3.93), EU_Sales=c(1.56))
predict(model_mlr_original, newdata = Global_Sales_Forecast, 
        interval='confidence')

# c. NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
Global_Sales_Forecast <- data.frame(NA_Sales=c(2.73), EU_Sales=c(0.65))
predict(model_mlr_original, newdata = Global_Sales_Forecast, 
        interval='confidence')

# d. NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
Global_Sales_Forecast <- data.frame(NA_Sales=c(2.26), EU_Sales=c(0.97))
predict(model_mlr_original, newdata = Global_Sales_Forecast, 
        interval='confidence')

# e. NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
Global_Sales_Forecast <- data.frame(NA_Sales=c(22.08), EU_Sales=c(0.52))
predict(model_mlr_original, newdata = Global_Sales_Forecast, 
        interval='confidence')

###############################################################################

# 4.2 Predictions based on given values for AGGREGATED dataset
# Compare with observed values for a number of records.

# a. NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
Global_Total_Forecast <- data.frame(NA_Total=c(34.02), EU_Total=c(23.80))
predict(model_mlr_1, newdata = Global_Total_Forecast, 
        interval='confidence')

# b. NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
Global_Total_Forecast <- data.frame(NA_Total=c(3.93), EU_Total=c(1.56))
predict(model_mlr_1, newdata = Global_Total_Forecast, 
        interval='confidence')

# c. NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
Global_Total_Forecast <- data.frame(NA_Total=c(2.73), EU_Total=c(0.65))
predict(model_mlr_1, newdata = Global_Total_Forecast, 
        interval='confidence')

# d. NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
Global_Total_Forecast <- data.frame(NA_Total=c(2.26), EU_Total=c(0.97))
predict(model_mlr_1, newdata = Global_Total_Forecast, 
        interval='confidence')

# e. NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
Global_Total_Forecast <- data.frame(NA_Total=c(22.08), EU_Total=c(0.52))
predict(model_mlr_1, newdata = Global_Total_Forecast, 
        interval='confidence')


###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# The residuals are random and let us plot the linear regression without issue.

#	Based on the plotted values the NA Sales (0.87) as well as the 
# EU Sales (0.78) have got a strong positive correlation with the Global Sales.
# It makes sense as these two variables comprise the total sales figure. 
# There is a moderate correlation between EU and NA sales (0.47) so one might
# not necessarily be affected by the other.

# Simple linear regression for NA Total vs Global Total generates the 
# R-Squared of 0.76 whilst the logarithmic model R-Squared actually makes 
# the model worse with R-Squared at 0.66. For the EU it barely 
# changes (0.61 => 0.63) vs Global and NA vs EU (0.22 => 0.18).

#	The adjusted R-Squared for MLR is at 93.8% which shows it being a high 
# quality model confirming that both NA and EU sales can explain the 
# Global sales with the 93.8% accuracy and are significant variables.

# Fitted Results as requested:
#   NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80
# fit      lwr      upr
# 67.7591 65.49936 70.01883

# NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56
# fit      lwr      upr
# 7.349275 7.080392 7.618158

# NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65
# fit      lwr      upr
# 4.912281 4.589107 5.235455

# NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97
# fit      lwr      upr
# 4.774066 4.464075 5.084056

# NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52
# fit     lwr      upr
# 26.34421 24.8969 27.79152

# All the requested fitted lines fall within the Upper and Lower limits
# which means the model is stable.


###############################################################################
###############################################################################




