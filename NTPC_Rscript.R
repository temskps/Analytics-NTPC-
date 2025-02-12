# ========================================================================================================
# Purpose:      Analytics II Project (NTPC Limited - Case Study)
# Author:       Kenneth Chow, Li Yang, Karen, Justin, Niklaus
# DOC:          31-03-2024
# Data Source:  client_train.csv, invoice_train.csv
#=========================================================================================================

# =========================================================================================================================================================
# Loading Packages
# =========================================================================================================================================================

library(ggcorrplot)
library(rsample)
library(data.table)
library(ggplot2)
library(corrplot)
library(caTools)
library(randomForest)
library(caret)
library(dplyr)
library(plyr)
library(tidyverse)
library(openxlsx)
library(rpart)
library(rpart.plot)
library(arules)
library(arulesViz)
library(plotly)
library(skimr)
library(scales)
library(patchwork)
library(base)
library(simpleboot)
library(glue)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(forcats)
library(neuralnet)

# =========================================================================================================================================================
# IMPORTING DATA
# =========================================================================================================================================================

setwd("/Users/kenneth/Downloads/archive")
client_train.df=fread("client_train.csv",stringsAsFactors = TRUE)
invoice_train.df=fread("invoice_train.csv",stringsAsFactors = TRUE)

# =========================================================================================================================================================
# DATA EXPLORATION ON client_train and invoice_train
# =========================================================================================================================================================

summary(client_train.df)
summary(invoice_train.df)
invoice_train.df$invoice_date <- as.Date(invoice_train.df$invoice_date, origin = "1970-01-01")

invoice_train.df %>% 
  skim()

#count by region
count_region <- client_train.df %>%
  count(region, sort = TRUE)
#101, 104, 311 are the top 3 Regions

#number of customers by year
num_customer_year <- client_train.df %>%
  mutate(
    client_catg = as.factor(client_catg),
    creation_date = dmy(creation_date),
    year = year(creation_date),
    month = month(creation_date)
  ) %>%
  group_by(year, client_catg) %>%
  summarise(counts = n(), .groups = "drop") %>%
  arrange(desc(year)) %>%
  ggplot() +
  geom_line(aes(
    x = year,
    y = counts,
    color = client_catg,
    group = client_catg
  ),
  linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1977, 2019, 3)) +
  scale_color_manual(values = c("#212F3D", "#154360", "#EB984E")) +
  labs(x = NULL,
       y = NULL,
       title = "Number of customers by year 1997 - 2018") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white"),
    axis.text.y = element_text(size = 10, face = "bold", color = "#006837"),
    axis.text.x = element_text(
      angle = 45,
      vjust = 0.5,
      size = 10,
      face = "bold",
      color = "gray50"
    ),
    plot.caption = element_text(hjust = 1),
    legend.position = "bottom"
  )

num_customer_year_log <- 
  num_customer_year +
  scale_y_log10(labels = comma)

merge_plot <- num_customer_year / num_customer_year_log +
  labs(
    x = NULL,
    y = NULL,
    title = "log10",
    color = "Client cat."
  )

merge_plot

#Counter Type Distribution Pie chart
elec_gas_segment <- table(invoice_train.df$counter_type)
labels <- c("ELEC", "GAZ")
percentages <- round(elec_gas_segment/sum(elec_gas_segment)*100)
labels <- paste(labels, percentages)
labels <- paste(labels,"%",sep="")
pie(elec_gas_segment,
    labels = labels, 
    col = viridis_pal()(length(elec_gas_segment)))

#identify honest and fraudulent customers
honest_cust <- client_train.df %>% 
  select(client_id, target) %>% 
  filter(target == 0) %>% 
  pull(client_id)

fraudulent_cust <- client_train.df %>% 
  select(client_id, target) %>% 
  filter(target == 1) %>% 
  pull(client_id)


#honest vs fraud vs average graph of ELEC and GAZ consumption
con_comp <- function(counter, cust_1, cust_2){
  
  clients_in_invoice <- invoice_train.df %>% 
    filter(counter_type == {{counter}},
           invoice_date >= "2005-01-01") %>%
    distinct(client_id) %>% 
    pull(client_id)
  
  honest <- intersect(clients_in_invoice, honest_cust)
  fraudulent <- intersect(clients_in_invoice, fraudulent_cust)
  
  count_invoice_train <- invoice_train.df %>% 
    filter(counter_type == {{counter}},
           invoice_date >= "2005-01-01") 
  
  
  monthly_avg_con <- count_invoice_train %>% 
    select(client_id, consommation_level_1, invoice_date) %>% 
    mutate(invoice_month = month(invoice_date)) %>% 
    group_by(invoice_month) %>% 
    summarise(avg_consommation_level_1 = mean(consommation_level_1)) 
  
  
  honest_monthly_avg_con <- count_invoice_train %>% 
    filter(client_id == honest[{{cust_1}}]) %>% 
    select(client_id, consommation_level_1, invoice_date) %>% 
    mutate(invoice_month = month(invoice_date)) %>% 
    group_by(invoice_month) %>% 
    summarise(avg_consommation_level_1 = mean(consommation_level_1)) 
  
  
  fraud_monthly_avg_con <- count_invoice_train %>% 
    filter(client_id == fraudulent[{{cust_2}}]) %>% 
    select(client_id, consommation_level_1, invoice_date) %>% 
    mutate(invoice_month = month(invoice_date)) %>% 
    group_by(invoice_month) %>% 
    summarise(avg_consommation_level_1 = mean(consommation_level_1)) 
  
  plot <- ggplot() +
    geom_line(
      data = monthly_avg_con,
      aes(x = invoice_month, y = avg_consommation_level_1, color = "Monthly AVG"),
      size = 1.5
    ) +
    geom_line(
      data = honest_monthly_avg_con,
      aes(x = invoice_month, y = avg_consommation_level_1, color = "Honest Customer"),
      size = 1.2
    ) +
    geom_line(
      data = fraud_monthly_avg_con,
      aes(x = invoice_month, y = avg_consommation_level_1, color = "Fraudulent Customer"),
      size = 1.2,
      linetype = 2
    ) +
    scale_x_continuous(breaks = seq(1, 12, 1),
                       labels = month.abb) +
    scale_color_manual(
      name = NULL,
      breaks = c("Monthly AVG",
                 "Honest Customer",
                 "Fraudulent Customer"),
      values = c(
        "Monthly AVG" = "#154360",
        "Honest Customer" = "#3e9c15",
        "Fraudulent Customer" = "#cc0000"
      )
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(
      title =  "Consumption discrepancy (fraudulent and honest)",
      subtitle = glue("{counter} Consumption comparison \nbetween two randomly selected customers."),
      color = "",
      x = "Month",
      y = "Consumption"
    )
  
  return(plot)
  
}
(con_comp("ELEC", 550, 250) + 
    theme(legend.position = "none")) / 
  (con_comp("GAZ", 550, 250) +
     labs( title = NULL))

#target variable bar chart
target_counts <- table(client_train.df$target)
barplot(target_counts, 
        main = "Counts of Target",
        ylab = "Counts",
        xlab = "Target",
        col = c("#4CAF50","#F4511E"))


#percentage of customers per district
ggplot(client_train.df, aes(x = factor(disrict), fill = factor(target))) +
  geom_bar(position = "fill") +
  labs(title = "Target Variable Distribution by District", x = "District", y = "Proportion of Customers") +
  scale_fill_discrete(name = "Target", labels = c("No Theft", "No Theft")) +
  theme_minimal()


# Plotting client distribution by region with separate bars for "No Theft" and "Theft"
ggplot(client_train.df, aes(x = factor(region), fill = factor(target))) +
  geom_bar(position = "dodge") +
  facet_wrap(~ region, scales = "free_x") +
  labs(title = "Client Distribution by Region", x = "Region", y = "Count of Clients") +
  scale_fill_discrete(name = "Target", labels = c("No Theft", "Theft")) +
  theme_minimal()

# Calculate the percentage of frauds by region
fraud_pct <- client_train.df %>%
  group_by(region) %>%
  summarise(fraud_percentage = mean(target) * 100)

ggplot(fraud_pct, aes(x = region, y = fraud_percentage)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Region", y = "Percentage of Frauds", title = "Percentage of Frauds by Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation matrix
joined.df<-merge(client_train.df,invoice_train.df, by="client_id")
joined.df$invoice_date<-as.Date(joined.df$invoice_date)
joined.df$creation_date<-as.Date(joined.df$creation_date)
joined.df$year_invoice_date<-year(joined.df$invoice_date)
joined.df$month_invoice_date<-month(joined.df$invoice_date)
joined.df$day_invoice_date<-day(joined.df$invoice_date)
joined.df$year_creation_date<-year(joined.df$creation_date)
joined.df$month_creation_date<-month(joined.df$creation_date)
joined.df$day_creation_date<-day(joined.df$creation_date)
joined.df <- subset(joined.df, select = -c(client_id, counter_type, creation_date, invoice_date))

df_numeric <- data.frame(lapply(joined.df, function(x) as.numeric(as.character(x))))
cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")
# Visualize the correlation matrix

ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower",
           lab = TRUE, 
           lab_size = 3, 
           lab_col = "black",
           tl.cex = 8,
           tl.col = "black",
           tl.srt = 45,
           hc.order = TRUE,
           colors = c("#6D9EC1", "white", "#E46726"),
           title="Correlation matrix", 
           ggtheme=theme_minimal()
)

# =========================================================================================================================================================
# Data cleaning
# =========================================================================================================================================================

invoice_train.df <- invoice_train.df %>%
  mutate(counter_statue = case_when(
    counter_statue == 0 | counter_statue == "0" | counter_statue == "A" ~ 0,
    counter_statue == 1 | counter_statue == "1" ~ 1,
    counter_statue == 2 ~ 2,
    counter_statue == 3 ~ 3,
    counter_statue == 4 | counter_statue == "4" ~ 4,
    counter_statue == 5 | counter_statue == "5" | counter_statue == 769 | counter_statue == 618 | counter_statue == 269375 | counter_statue == 46 | counter_statue == 420 ~ 5,
    TRUE ~ as.integer(NA) # Default case if none of the above conditions are met
  ))

sum(is.na(client_train.df))
sum(is.na(invoice_train.df))
sapply(client_train.df, function(x) sum(is.na(x)))
sapply(invoice_train.df, function(x) sum(is.na(x)))

# Get the unique values to check
unique(invoice_train.df$client_id)
unique(invoice_train.df$invoice_date)
unique(invoice_train.df$tarif_type)
unique(invoice_train.df$counter_number)
unique(invoice_train.df$counter_statue)
unique(invoice_train.df$counter_code)
unique(invoice_train.df$reading_remarque)
unique(invoice_train.df$counter_coefficient)
unique(invoice_train.df$consommation_level_1)
unique(invoice_train.df$consommation_level_2)
unique(invoice_train.df$consommation_level_3)
unique(invoice_train.df$consommation_level_4)
unique(invoice_train.df$old_index)
unique(invoice_train.df$new_index)
unique(invoice_train.df$months_number)
unique(invoice_train.df$counter_type)

#checked for negative numbers 
summary(invoice_train.df)
summary(client_train.df)

#DUPLICATE CHECK
# Convert dataframe to data.table
client_train.dt<- as.data.table(client_train.df)
invoice_train.dt<- as.data.table(invoice_train.df)
rm(client_train.df)
rm(invoice_train.df)

#Check for duplicates
duplicated_rows <- client_train.dt[duplicated(client_train.dt), ]
View(duplicated_rows)
duplicated_rows <- invoice_train.dt[duplicated(invoice_train.dt), ]
View(duplicated_rows)
rm(duplicated_rows)

# Remove duplicate rows from client_train.dt
invoice_train.dt<- unique(invoice_train.dt)
#Final Check
duplicated_rows <- invoice_train.dt[duplicated(invoice_train.dt), ]
View(duplicated_rows)
rm(duplicated_rows)

#Check for outliers
# Assuming client_train.dt is your data.table containing the specified columns

# Select columns for boxplot
columns <- invoice_train.dt[, .(consommation_level_1, consommation_level_2, consommation_level_3, consommation_level_4)]

# Convert data.table to data.frame for plotting with base R
columns_df <- as.data.frame(columns)

# Set options to suppress scientific notation
options(scipen = 999)

# Create boxplot
boxplot(columns_df, 
        main = "Boxplot of Consommation Levels",
        xlab = "Consommation Level",
        ylab = "Value",
        col = c("red", "green", "blue", "orange"),  # Colors for each box
        names = c("Level 1", "Level 2", "Level 3", "Level 4"),  # Labels for each box
        border = "black"  # Border color for boxes
)
rm(columns,columns_df)

# Reset options
options(scipen = 0)

# Assuming client_train.dt is your data.table containing the specified column
# Filter outliers for consommation_level_1
outliers_level_1 <- invoice_train.dt[consommation_level_1 >= 600000]
# Filter outliers for consommation_level_2
outliers_level_2 <- invoice_train.dt[consommation_level_2 >= 600000]
# Filter outliers for consommation_level_4
outliers_level_4 <- invoice_train.dt[consommation_level_4 >= 200000]

# Print the outliers for level 1
print(outliers_level_1$client_id)
# Print the outliers
print(outliers_level_2$client_id)
# Print the outliers for level 4
print(outliers_level_4$client_id)

rm(outliers_level_1,outliers_level_2,outliers_level_4)

#CONSUMPTION LEVEL 1 OUTLIER DATA REPLACEMENT
# Subset the data for client_id "train_Client_48203"
client_48203_df <- invoice_train.dt[client_id == "train_Client_48203"]

# Convert to data frame
client_48203_df <- as.data.frame(client_48203_df)

#Create new column invoice_year
client_48203_df <- client_48203_df %>%
  mutate(invoice_year = year(invoice_date))

# Group by invoice_year and calculate summary statistics
summary_stats_48203 <- client_48203_df %>%
  group_by(invoice_year) %>%
  summarise(mean_consommation_level_1 = mean(consommation_level_1, na.rm = TRUE),
            mean_consommation_level_2 = mean(consommation_level_2, na.rm = TRUE),
            mean_consommation_level_3 = mean(consommation_level_3, na.rm = TRUE),
            mean_consommation_level_4 = mean(consommation_level_4, na.rm = TRUE))

#Outlier for Consumption Level 1 Chart
ggplot(summary_stats_48203, aes(x = invoice_year, y = mean_consommation_level_1)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(aes(label = round(mean_consommation_level_1)), vjust = -0.5, hjust = 0.5, nudge_y = 100) + # Add text labels
  labs(title = "Yearly Average Consumption Level (Client 48203)",
       x = "Year", y = "Average Consumption Level 1") +
  theme_bw() +
  scale_x_continuous(breaks = summary_stats_48203$invoice_year, labels = summary_stats_48203$invoice_year) +
  annotate("text", x = Inf, y = -Inf, label = "", hjust = 1, vjust = 0, size = 4)

#replacement of outlier for consumption level 1
# Filter summary_stats_48203 for invoice_year 2011 and 2013
filtered_years <- summary_stats_48203[summary_stats_48203$invoice_year %in% c(2011, 2013), ]

# Calculate the mean of invoice_year for 2011 and 2013
mean_consommation_48203 <- mean(filtered_years$mean_consommation_level_1)

# Print the mean of invoice_year for 2011 and 2013
print(mean_consommation_48203)

invoice_train.dt[client_id == "train_Client_48203" & consommation_level_1 > 600000,
                 consommation_level_1 := floor(mean_consommation_48203)]

summary(invoice_train.dt$consommation_level_1)

# Select columns for boxplot
columns <- invoice_train.dt[, .(consommation_level_1, consommation_level_2, consommation_level_3, consommation_level_4)]

# Convert data.table to data.frame for plotting with base R
columns_df <- as.data.frame(columns)

# Set options to suppress scientific notation
options(scipen = 999)
boxplot(columns_df, 
        main = "Boxplot of Consommation Levels",
        xlab = "Consommation Level",
        ylab = "Value",
        col = c("red", "green", "blue", "orange"),  # Colors for each box
        names = c("Level 1", "Level 2", "Level 3", "Level 4"),  # Labels for each box
        border = "black"  # Border color for boxes
)

rm(columns_df, client_48203_df,columns, client_48203_df_columns_df, filtered_years, summary_stats_48203, mean_consommation_48203)

#CONSUMPTION LEVEL 2 OUTLIER DATA REPLACEMENT
# CLIENT 113523
# Subset the data for client_id "train_Client_48203"
client_113523_df <- invoice_train.dt[client_id == "train_Client_113523"]

# Convert to data frame
client_113523_df <- as.data.frame(client_113523_df)

#Create new column invoice_year
client_113523_df <- client_113523_df %>%
  mutate(invoice_year = year(invoice_date))

# Group by invoice_year and calculate summary statistics
summary_stats_113523 <- client_113523_df %>%
  group_by(invoice_year) %>%
  summarise(mean_consommation_level_1 = mean(consommation_level_1, na.rm = TRUE),
            mean_consommation_level_2 = mean(consommation_level_2, na.rm = TRUE),
            mean_consommation_level_3 = mean(consommation_level_3, na.rm = TRUE),
            mean_consommation_level_4 = mean(consommation_level_4, na.rm = TRUE))

#replacement of outlier for consumption level 1
# Filter summary_stats_48203 for invoice_year 2011 and 2013
filtered_years <- summary_stats_113523[summary_stats_113523$invoice_year %in% c(2011, 2013), ]

# Calculate the mean of invoice_year for 2011 and 2013
mean_consommation_113523 <- mean(filtered_years$mean_consommation_level_2)

# Print the mean of invoice_year for 2011 and 2013
print(mean_consommation_113523)

invoice_train.dt[client_id == "train_Client_113523" & consommation_level_2 > 600000,
                 consommation_level_2 := floor(mean_consommation_113523)]

rm(client_113523_df, filtered_years, summary_stats_48203, mean_consommation_48203, summary_stats_113523, mean_consommation_113523)

# CLIENT 56441
# Subset the data for client_id
client_56441_df <- invoice_train.dt[client_id == "train_Client_56441"]

# Convert to data frame
client_56441_df <- as.data.frame(client_56441_df)

#Create new column invoice_year
client_56441_df <- client_56441_df %>%
  mutate(invoice_year = year(invoice_date))

# Group by invoice_year and calculate summary statistics
summary_stats_56441 <- client_56441_df %>%
  group_by(invoice_year) %>%
  summarise(mean_consommation_level_1 = mean(consommation_level_1, na.rm = TRUE),
            mean_consommation_level_2 = mean(consommation_level_2, na.rm = TRUE),
            mean_consommation_level_3 = mean(consommation_level_3, na.rm = TRUE),
            mean_consommation_level_4 = mean(consommation_level_4, na.rm = TRUE))

#replacement of outlier for consumption level 1
# Filter for invoice_year 2011 and 2013
filtered_years <- summary_stats_56441[summary_stats_56441$invoice_year %in% c(2011, 2013), ]

# Calculate the mean of invoice_year for 2011 and 2013
mean_consommation_56441 <- mean(filtered_years$mean_consommation_level_2)

# Print the mean of invoice_year for 2011 and 2013
print(mean_consommation_56441)

invoice_train.dt[client_id == "train_Client_56441" & consommation_level_2 > 600000,
                 consommation_level_2 := floor(mean_consommation_56441)]

rm(filtered_years, summary_stats_56441, mean_consommation_56441, client_56441_df)

# CLIENT 62387
# Subset the data for client_id
client_62387_df <- invoice_train.dt[client_id == "train_Client_62387"]

# Convert to data frame
client_62387_df <- as.data.frame(client_62387_df)

#Create new column invoice_year
client_62387_df <- client_62387_df %>%
  mutate(invoice_year = year(invoice_date))

# Group by invoice_year and calculate summary statistics
summary_stats_62387 <- client_62387_df %>%
  group_by(invoice_year) %>%
  summarise(mean_consommation_level_1 = mean(consommation_level_1, na.rm = TRUE),
            mean_consommation_level_2 = mean(consommation_level_2, na.rm = TRUE),
            mean_consommation_level_3 = mean(consommation_level_3, na.rm = TRUE),
            mean_consommation_level_4 = mean(consommation_level_4, na.rm = TRUE))

#replacement of outlier for consumption level 1
# Filter for invoice_year 2011 and 2013
filtered_years <- summary_stats_62387[summary_stats_62387$invoice_year %in% c(2011, 2013), ]

# Calculate the mean of invoice_year for 2011 and 2013
mean_consommation_62387 <- mean(filtered_years$mean_consommation_level_2)

# Print the mean of invoice_year for 2011 and 2013
print(mean_consommation_62387)

invoice_train.dt[client_id == "train_Client_62387" & consommation_level_2 > 600000,
                 consommation_level_2 := floor(mean_consommation_62387)]

rm(filtered_years, summary_stats_62387, mean_consommation_62387, client_62387_df)

# CLIENT 5737
# Subset the data for client_id
client_5737_df <- invoice_train.dt[client_id == "train_Client_5737"]

# Convert to data frame
client_5737_df <- as.data.frame(client_5737_df)

#Create new column invoice_year
client_5737_df <- client_5737_df %>%
  mutate(invoice_year = year(invoice_date))

# Group by invoice_year and calculate summary statistics
summary_stats_5737 <- client_5737_df %>%
  group_by(invoice_year) %>%
  summarise(mean_consommation_level_1 = mean(consommation_level_1, na.rm = TRUE),
            mean_consommation_level_2 = mean(consommation_level_2, na.rm = TRUE),
            mean_consommation_level_3 = mean(consommation_level_3, na.rm = TRUE),
            mean_consommation_level_4 = mean(consommation_level_4, na.rm = TRUE))

#replacement of outlier for consumption level 1
# Filter for invoice_year 2011 and 2013
filtered_years <- summary_stats_5737[summary_stats_5737$invoice_year %in% c(2011, 2013), ]

# Calculate the mean of invoice_year for 2011 and 2013
mean_consommation_5737 <- mean(filtered_years$mean_consommation_level_2)

# Print the mean of invoice_year for 2011 and 2013
print(mean_consommation_5737)

invoice_train.dt[client_id == "train_Client_5737" & consommation_level_2 > 600000,
                 consommation_level_2 := floor(mean_consommation_5737)]

rm(filtered_years, summary_stats_5737, mean_consommation_5737, client_5737_df)

# LEVEL 4
# CLIENT 5737
# Subset the data for client_id
client_72985_df <- invoice_train.dt[client_id == "train_Client_72985"]

# Convert to data frame
client_72985_df <- as.data.frame(client_72985_df)

#Create new column invoice_year
client_72985_df <- client_72985_df %>%
  mutate(invoice_year = year(invoice_date))

# Group by invoice_year and calculate summary statistics
summary_stats_72985 <- client_72985_df %>%
  group_by(invoice_year) %>%
  summarise(mean_consommation_level_1 = mean(consommation_level_1, na.rm = TRUE),
            mean_consommation_level_2 = mean(consommation_level_2, na.rm = TRUE),
            mean_consommation_level_3 = mean(consommation_level_3, na.rm = TRUE),
            mean_consommation_level_4 = mean(consommation_level_4, na.rm = TRUE))

#replacement of outlier for consumption level 1
# Filter for invoice_year 2011 and 2013
filtered_years <- summary_stats_72985[summary_stats_72985$invoice_year %in% c(2016, 2018), ]

# Calculate the mean of invoice_year for 2011 and 2013
mean_consommation_72985 <- mean(filtered_years$mean_consommation_level_4)

# Print the mean of invoice_year for 2011 and 2013
print(mean_consommation_72985)

invoice_train.dt[client_id == "train_Client_72985" & consommation_level_4 > 200000,
                 consommation_level_4 := floor(mean_consommation_72985)]

rm(filtered_years, summary_stats_72985, mean_consommation_72985, client_72985_df)

# CLIENT 105976
# Subset the data for client_id
client_105976_df <- invoice_train.dt[client_id == "train_Client_105976"]

# Convert to data frame
client_105976_df <- as.data.frame(client_105976_df)

#Create new column invoice_year
client_105976_df <- client_105976_df %>%
  mutate(invoice_year = year(invoice_date))

# Group by invoice_year and calculate summary statistics
summary_stats_105976 <- client_105976_df %>%
  group_by(invoice_year) %>%
  summarise(mean_consommation_level_1 = mean(consommation_level_1, na.rm = TRUE),
            mean_consommation_level_2 = mean(consommation_level_2, na.rm = TRUE),
            mean_consommation_level_3 = mean(consommation_level_3, na.rm = TRUE),
            mean_consommation_level_4 = mean(consommation_level_4, na.rm = TRUE))

#replacement of outlier for consumption level 1
# Filter for invoice_year 2017 and 2015
filtered_years <- summary_stats_105976[summary_stats_105976$invoice_year %in% c(2017, 2015), ]

# Calculate the mean of invoice_year for 2011 and 2013
mean_consommation_105976 <- mean(filtered_years$mean_consommation_level_4)

# Print the mean of invoice_year for 2011 and 2013
print(mean_consommation_105976)

invoice_train.dt[client_id == "train_Client_105976" & consommation_level_4 > 200000,
                 consommation_level_4 := floor(mean_consommation_105976)]

rm(filtered_years, summary_stats_105976, client_105976_df, mean_consommation_105976)

# FINAL CHECK 
columns <- invoice_train.dt[, .(consommation_level_1, consommation_level_2, consommation_level_3, consommation_level_4)]

# Convert data.table to data.frame for plotting with base R
columns_df <- as.data.frame(columns)

# Set options to suppress scientific notation
options(scipen = 999)
boxplot(columns_df, 
        main = "Boxplot of Consommation Levels",
        xlab = "Consommation Level",
        ylab = "Value",
        col = c("red", "green", "blue", "orange"),  # Colors for each box
        names = c("Level 1", "Level 2", "Level 3", "Level 4"),  # Labels for each box
        border = "black"  # Border color for boxes
)
rm(columns, columns_df)

# =========================================================================================================================================================
# feature engineering 
# =========================================================================================================================================================

#adjustments for feature engineering
invoice_train.df <- data.frame(invoice_train.dt)
client_train.df <- data.frame(client_train.dt)
rm(client_train.dt, invoice_train.dt)

#Creating new statistical features & joining datasets
invoice_train.df_pp <- invoice_train.df %>%
  group_by(client_id) %>%
  mutate(across(
    .cols = c(
      consommation_level_1,
      consommation_level_2,
      consommation_level_3,
      old_index,
      new_index
    ),
    ~ cumsum(.x),
    .names = "cumsum_{col}"
  )) 

find_mode <- function(x) {
  uniquex <- unique(x)
  uniquex[which.max(tabulate(match(x, uniquex)))]
}

summary_invoice_train.df <- invoice_train.df_pp %>% select(
  -c(
    counter_code,
    counter_number
  )
) %>%
  group_by(client_id, counter_type) %>%
  summarise(
    avg_consom_l_1 = mean(consommation_level_1),
    var_consom_l_1 = var(consommation_level_1),
    sd_consom_l_1 = sd(consommation_level_1),
    median_consom_l_1 = median(consommation_level_1),
    mode_consom_l_1 = find_mode(consommation_level_1),
    avg_diff_consom_l_1 = mean(diff(consommation_level_1)),
    range_consom_l_1 = max(consommation_level_1) - min(consommation_level_1),
    sd_cumsum_consommation_level_1 = sd(cumsum_consommation_level_1),
    avg_cumsum_consommation_level_1 = mean(cumsum_consommation_level_1),
    median_cumsum_consommation_level_1 = median(cumsum_consommation_level_1),
    #
    avg_consom_l_2 = mean(consommation_level_2),
    var_consom_l_2 = var(consommation_level_2),
    sd_consom_l_2 = sd(consommation_level_2),
    median_consom_l_2 = median(consommation_level_2),
    mode_consom_l_2 = find_mode(consommation_level_2),
    avg_diff_consom_l_2 = mean(diff(consommation_level_2)),
    range_consom_l_2 = max(consommation_level_2) - min(consommation_level_2),
    sd_cumsum_consommation_level_2 = sd(cumsum_consommation_level_2),
    avg_cumsum_consommation_level_2 = mean(cumsum_consommation_level_2),
    median_cumsum_consommation_level_2 = median(cumsum_consommation_level_2),
    #
    avg_consom_l_3 = mean(consommation_level_3),
    var_consom_l_3 = var(consommation_level_3),
    sd_consom_l_3 = sd(consommation_level_3),
    median_consom_l_3 = median(consommation_level_3),
    mode_consom_l_3 = find_mode(consommation_level_3),
    avg_diff_consom_l_3 = mean(diff(consommation_level_3)),
    range_consom_l_3 = max(consommation_level_3) - min(consommation_level_3),
    sd_cumsum_consommation_level_3 = sd(cumsum_consommation_level_3),
    avg_cumsum_consommation_level_3 = mean(cumsum_consommation_level_3),
    median_cumsum_consommation_level_3 = median(cumsum_consommation_level_3),
    #
    avg_consom_l_4 = mean(consommation_level_4),
    var_consom_l_4 = var(consommation_level_4),
    sd_consom_l_4 = sd(consommation_level_4),
    median_consom_l_4 = median(consommation_level_4),
    mode_consom_l_4 = find_mode(consommation_level_4),
    avg_diff_consom_l_4 = mean(diff(consommation_level_4)),
    range_consom_l_4 = max(consommation_level_4) - min(consommation_level_4),
    #
    avg_diff_old_index = mean(diff(old_index)),
    var_old_index = var(old_index),
    avg_diff_new_index = mean(diff(new_index)),
    var_new_index = var(new_index),
    #
    diff_avg_new_old_index = avg_diff_new_index - avg_diff_old_index,
    diff_var_new_old_index = var_new_index - var_old_index,
    #
    range_old_index = max(old_index) - min(old_index),
    range_new_index = max(new_index) - min(new_index),
    #
    min_old_index = min(old_index),
    min_new_index = min(new_index),
    #
    diff_min_new_old_index = min_new_index - min_old_index,
    #
    max_old_index = max(old_index),
    max_new_index = max(new_index),
    #
    diff_max_new_old_index = max_new_index - max_old_index,
    #
    sd_old_index = sd(old_index),
    sd_new_index = sd(new_index),
    #
    diff_sd_new_old_index = sd_new_index - sd_old_index,
    #
    sd_cumsum_old_index = sd(cumsum_old_index),
    avg_cumsum_old_index = mean(cumsum_old_index),
    median_cumsum_old_index = median(cumsum_old_index),
    #
    sd_cumsum_new_index = sd(cumsum_new_index),
    avg_cumsum_new_index = mean(cumsum_new_index),
    median_cumsum_new_index = median(cumsum_new_index),
    #
    count_counter_coefficient = length(counter_coefficient),
    count_counter_coefficient = mean(counter_coefficient),
    #
    count_invoice_date = length(invoice_date),
    #
    mode_reading_remarque = find_mode(reading_remarque),
    #
    mode_months_number = find_mode(months_number),
    #
    mode_counter_statue = find_mode(counter_statue)
  )



summary_invoice_train.df_wider <- summary_invoice_train.df %>%
  ungroup() %>%
  pivot_wider(names_from = counter_type, values_from = -client_id) %>%
  select(-c(counter_type_ELEC, counter_type_GAZ)) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

train_full <- client_train.df %>%
  left_join(summary_invoice_train.df_wider, by = "client_id") %>%
  mutate(
    creation_date = dmy(creation_date),
    month = month(creation_date),
    target = fct_rev(as.factor(target)),
    account_duration = interval(creation_date, ymd("2019-01-01")) %/% months(1)
  ) %>%
  select(client_id, where(is.numeric),
         -c(target, creation_date),
         target)

##model prep##
full_data_model <- invoice_train.df %>%
  select(client_id, tarif_type) %>%
  distinct(client_id, tarif_type) %>%
  right_join(train_full, by = "client_id") %>%
  select(where(is.numeric), target) %>%
  drop_na() %>%
  mutate(across(
    .cols = c(
      tarif_type,
      month,
      region,
      disrict,
      client_catg,
      mode_counter_statue_ELEC,
      mode_counter_statue_GAZ,
      mode_reading_remarque_ELEC,
      mode_reading_remarque_GAZ
    ),
    as.factor
  ))

rm(find_mode, train_full, invoice_train.df_pp, summary_invoice_train.df, summary_invoice_train.df_wider)

#additional data transformation
index_columns <- grep("index", names(full_data_model), value = TRUE)
full_data_model[index_columns] <- lapply(full_data_model[index_columns], as.numeric)
consom_columns <- grep("consom", names(full_data_model), value = TRUE)
full_data_model[consom_columns] <- lapply(full_data_model[consom_columns], as.numeric)
month_columns <- grep("month", names(full_data_model), value = TRUE)
full_data_model[month_columns] <- lapply(full_data_model[month_columns], as.numeric)
date_columns <- grep("date", names(full_data_model), value = TRUE)
full_data_model[date_columns] <- lapply(full_data_model[date_columns], as.numeric)
full_data_model$month<-factor(full_data_model$month)

rm( index_columns, consom_columns, month_columns, date_columns)


#Checking Data types
full_data_model %>%
  glimpse()

# =========================================================================================================================================================
# Train Test Split & Downsampling 
# =========================================================================================================================================================

#train test split 70-30 unbalanced
set.seed(1)
partition <- createDataPartition(full_data_model$target, p = 0.7, list = FALSE)

# Create training and test sets
train_set <- full_data_model[partition, ]
test_set <- full_data_model[-partition, ]

# Down-sampling the majority class (balanced)
downsample_full_data <- downSample(x = full_data_model[, -ncol(full_data_model)], y = full_data_model[, ncol(full_data_model)], yname = 'target')
# Create balanced training and test sets
partition2 <- createDataPartition(downsample_full_data$target, p = 0.7, list = FALSE)
balanced_train_set <- downsample_full_data[partition, ]
balanced_test_set <- downsample_full_data[-partition, ]

# Viewing the distribution of the target variable after down-sampling
table(balanced_train_set$target)
# Summary of the down-sampled training set
summary(balanced_train_set)

rm(partition, partition2, downsample_full_data, client_train.df, invoice_train.df)

# =========================================================================================================================================================
# CART Model
# =========================================================================================================================================================

#unbalanced data CART
cart.m1 <- rpart(target~., data = train_set, method="class", control = rpart.control(minsplit = 2, cp = 0))
CVerror.cap <- cart.m1$cptable[which.min(cart.m1$cptable[,"xerror"]), "xerror"] + cart.m1$cptable[which.min(cart.m1$cptable[,"xerror"]), "xstd"]
i <- 1; j <- 4
while (cart.m1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
num_splits <- sum(cart.m1$frame$var != "<leaf>")
#Attain optimal cp
cp.opt <- ifelse(i > 1, sqrt(cart.m1$cptable[i,1] * cart.m1$cptable[i-1,1]), 1)
cp.opt
printcp(cart.m1, digits=3)
plotcp(cart.m1)
split_row_index <- which.min(abs(cart.m1$cptable[, "CP"] - cp.opt))
split_details <- cart.m1$cptable[split_row_index, ]
print(split_details) #split 2249
#prune
cart.m2<-prune(cart.m1, cp=cp.opt)
printcp(cart.m2)
plotcp(cart.m2)
#predict testset
cart.pred<-predict(cart.m2, newdata = test_set, type="class")
results<-data.frame(test_set$target, cart.pred)
cart.table <- table(Actual = test_set$target, cart.pred, deparse.level = 2)
cart.table
accuracy<-(cart.table[1,1]+cart.table[2,2])/(cart.table[1,1]+cart.table[2,2]+cart.table[2,1]+cart.table[1,2])*100
accuracy
truepositiverate<- (cart.table[1,1])/(cart.table[1,1]+cart.table[1,2])
truepositiverate*100
var_importance <- as.data.frame(cart.m2$variable.importance)

# Variable importance plot
var_importance <- cart.m2$variable.importance
sorted_importance <- sort(var_importance, decreasing = TRUE)
par(mar = c(5, 12, 4, 2) + 0.1)  # default is c(5, 4, 4, 2) + 0.1
barplot(sorted_importance, 
        main = "Variable Importance", 
        horiz = TRUE, 
        las = 1, 
        cex.names = 0.5)
par(mar = c(5, 4, 4, 2) + 0.1)

rm(number_of_splits, sorted_importance, var_importance, cart.m1, cart.m2, results, accuracy, cart.pred, cart.table, cp.opt, CVerror.cap, i, j, num_splits, split_row_index, split_details)

#evaluating important variables
scaledVarImptbal <- round(100 * cart.m2$variable.importance / sum(cart.m2$variable.importance))
scaledVarImptbal <- as.data.frame(scaledVarImptbal)
scaledVarImptbal$Variable <- rownames(scaledVarImptbal)
important_vars_df <- scaledVarImptbal[scaledVarImptbal$scaledVarImptbal >= 2, ]
important_var_names1 <- important_vars_df$Variable
print(important_var_names1)

rm(scaledVarImptbal, important_vars_df, important_var_names1)

#balanced data CART
cart.m3 <- rpart(target~., data = balanced_train_set, method="class", control = rpart.control(minsplit = 2, cp = 0))
num_splits <- sum(cart.m3$frame$var != "<leaf>")
CVerror.cap <- cart.m3$cptable[which.min(cart.m3$cptable[,"xerror"]), "xerror"] + cart.m3$cptable[which.min(cart.m3$cptable[,"xerror"]), "xstd"]
i <- 1; j <- 4
while (cart.m3$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
#Attain optimal cp
cp.opt <- ifelse(i > 1, sqrt(cart.m3$cptable[i,1] * cart.m3$cptable[i-1,1]), 1)
cp.opt
printcp(cart.m3)
plotcp(cart.m3)
split_row_index <- which.min(abs(cart.m3$cptable[, "CP"] - cp.opt))
split_details <- cart.m3$cptable[split_row_index, ]
print(split_details) #split 1114
#prune
cart.m4<-prune(cart.m3, cp=cp.opt)
printcp(cart.m4)
plotcp(cart.m4)

#visualisation
# Prune the tree to a 3 levels
cart.m4_visual <- prune(cart.m4, cp = cart.m4$cptable[which(cart.m4$cptable[,"nsplit"] == 5), "CP"])
# Now plot the pruned tree
rpart.plot(cart.m4_visual)

#predict testset
cart.pred<-predict(cart.m4, newdata = balanced_test_set, type="class")
results<-data.frame(balanced_test_set$target, cart.pred)
cart.table <- table(Actual = balanced_test_set$target, cart.pred, deparse.level = 2)
cart.table
accuracy<-(cart.table[1,1]+cart.table[2,2])/(cart.table[1,1]+cart.table[2,2]+cart.table[2,1]+cart.table[1,2])*100
accuracy
truepositiverate<- (cart.table[1,1])/(cart.table[1,1]+cart.table[1,2])
truepositiverate*100
var_importance <- as.data.frame(cart.m4$variable.importance)

# Variable importance plot
var_importance <- cart.m4$variable.importance
sorted_importance <- sort(var_importance, decreasing = TRUE)
par(mar = c(5, 12, 4, 2) + 0.1)  # default is c(5, 4, 4, 2) + 0.1
barplot(sorted_importance, 
        main = "Variable Importance", 
        horiz = TRUE, 
        las = 1, 
        cex.names = 0.5)
par(mar = c(5, 4, 4, 2) + 0.1)

#evaluating important variables for balanced cart model
scaledVarImptbal <- round(100 * cart.m4$variable.importance / sum(cart.m4$variable.importance))
scaledVarImptbal <- as.data.frame(scaledVarImptbal)
scaledVarImptbal$Variable <- rownames(scaledVarImptbal)
important_vars_df <- scaledVarImptbal[scaledVarImptbal$scaledVarImptbal >= 2, ]
important_var_names2 <- important_vars_df$Variable
print(important_var_names2)
balanced_train_set_important <- balanced_train_set[, c(important_var_names2, "target")]
balanced_test_set_important <- balanced_test_set[, c(important_var_names2, "target")]

#using only important variables for training balanced dataset
cart.m5 <- rpart(target~., data = balanced_train_set_important, method="class", control = rpart.control(minsplit = 2, cp = 0))
CVerror.cap <- cart.m5$cptable[which.min(cart.m5$cptable[,"xerror"]), "xerror"] + cart.m5$cptable[which.min(cart.m5$cptable[,"xerror"]), "xstd"]
i <- 1; j <- 4
while (cart.m5$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
#Attain optimal cp
cp.opt <- ifelse(i > 1, sqrt(cart.m5$cptable[i,1] * cart.m5$cptable[i-1,1]), 1)
cp.opt
#prune
cart.m6<-prune(cart.m5, cp=cp.opt)
#predict testset
cart.pred<-predict(cart.m6, newdata = balanced_test_set_important, type="class")
results<-data.frame(balanced_test_set_important$target, cart.pred)
cart.table <- table(Actual = balanced_test_set_important$target, cart.pred, deparse.level = 2)
cart.table
accuracy<-(cart.table[1,1]+cart.table[2,2])/(cart.table[1,1]+cart.table[2,2]+cart.table[2,1]+cart.table[1,2])*100
accuracy

rm(balanced_train_set_important, balanced_test_set_important, cart.m4_visual, cart.m5, cart.m6, important_vars_df, scaledVarImptbal, important_var_names2, num_splits, split_details, split_row_index, sorted_importance, var_importance, cart.m3, cart.m4, results, accuracy, cart.pred, cart.table, cp.opt, CVerror.cap, i, j, truepositiverate)

# =========================================================================================================================================================
# Random Forest
# =========================================================================================================================================================

#Unbalanced dataset
set.seed(1)
target_variable <- train_set$target
features_only <- train_set[, setdiff(names(train_set), "target")]
m.RF.1 <- randomForest(x = features_only, y = target_variable, ntree=100, mtry=sqrt(ncol(features_only)), importance=TRUE)
m.RF1.pred <- predict(m.RF.1, newdata = test_set)
RF1.table = table(Actual.data = test_set$target, RF.m1.pred = m.RF1.pred, deparse.level = 2)
RF1.table
RF1.accuracy <- (RF1.table[1,1]+RF1.table[2,2])/(RF1.table[1,1]+RF1.table[2,2]+RF1.table[2,1]+RF1.table[1,2])
RF1.accuracy
mean(m.RF1.pred == test_set$target)
View(data.frame(importance(m.RF.1, scale=T)))
varImp(m.RF.1, type = 1)
m.RF.1
plot(m.RF.1)

#understanding variable importance of unbalanced dataset
importance_measures <- importance(m.RF.1, type = 1)  # type = 1 for MeanDecreaseAccuracy
variable_names <- rownames(importance_measures)
important_vars <- variable_names[importance_measures >= 15] #34 variables
print(important_vars)

#balanced dataset
balanced_train_set<-na.omit(balanced_train_set)
target_variable <- balanced_train_set$target
features_only <- balanced_train_set[, setdiff(names(balanced_train_set), "target")]
m.RF.2 <- randomForest(x = features_only, y = target_variable, ntree=100, mtry=sqrt(ncol(features_only)), importance=TRUE)
m.RF2.pred <- predict(m.RF.2, newdata = balanced_test_set)
RF2.table = table(Actual.data = balanced_test_set$target, RF.m2.pred = m.RF2.pred, deparse.level = 2)
RF2.table
RF2.accuracy <- (RF2.table[1,1]+RF2.table[2,2])/(RF2.table[1,1]+RF2.table[2,2]+RF2.table[2,1]+RF2.table[1,2])
RF2.accuracy
mean(m.RF2.pred == balanced_test_set$target)
View(data.frame(importance(m.RF.2, scale=T)))
varImpPlot(m.RF.2, type = 1)
varImp(m.RF.2, type = 1)
m.RF.2
plot(m.RF.2) #can see how the error has been significantly improved

#filtering out less significant variables
varImpPlot(m.RF.2, type = 1)  # Plot out importance of x-variables by MeanDecreaseAccuracy
varImpPlot(m.RF.2, type = 2) # Plot out importance of x-variables by MeanDecreaseGini
importance_measures <- importance(m.RF.2, type = 1)  # type = 1 for MeanDecreaseAccuracy
variable_names <- rownames(importance_measures)
important_vars <- variable_names[importance_measures >= 15] #20 variables
names(balanced_train_set_important)
balanced_train_set_important <- balanced_train_set[, c(important_vars, "target")]
balanced_test_set_important <- balanced_test_set[, c(important_vars, "target")]
names(balanced_train_set_important)
#testing balance test set w important variables
balanced_train_set_important<-na.omit(balanced_train_set_important)
target_variable <- balanced_train_set_important$target
features_only <- balanced_train_set_important[, setdiff(names(balanced_train_set_important), "target")]
m.RF.3 <- randomForest(x = features_only, y = target_variable, ntree=100, mtry=sqrt(ncol(features_only)), importance=TRUE)
m.RF3.pred <- predict(m.RF.3, newdata = balanced_test_set_important)
RF3.table = table(Actual.data = balanced_test_set_important$target, RF.m3.pred = m.RF3.pred, deparse.level = 2)
RF3.table
RF3.accuracy <- (RF3.table[1,1]+RF3.table[2,2])/(RF3.table[1,1]+RF3.table[2,2]+RF3.table[2,1]+RF3.table[1,2])
RF3.accuracy

rm(m.RF.2, m.RF.3, important_data, important_measures, important_vars, m.RF2.pred, m.RF3.pred, RF1.table,RF2.accuracy,RF2.table,RF3.table, RF3.accuracy,variable_names)
rm(important_measures, variable_names, important_vars, balanced_train_set_important, balanced_test_set_important, RF1.accuracy,RF1.table.RF.1, features_only, m.RF1.pred, RFaccuracy, RFfalseneg, RFfalsepos, RFprecision, RFrecall, target_variable)

# =========================================================================================================================================================
# Neural Network 
# =========================================================================================================================================================

# --- Data Preparation ---
# Convert factor variables to dummy variables
data_transformed <- data.frame(model.matrix(~ . - 1, data = trainset.bal))

# Normalize the data
preproc <- preProcess(data_transformed, method = c("center", "scale"))
data_normalized <- predict(preproc, data_transformed)

# --- Dataset Splitting ---
# Splitting the dataset (assuming 'target' is the last column)
target_variable <- data_normalized[, ncol(data_normalized)]
features <- data_normalized[, -ncol(data_normalized)]


# Training the neural network with 5 hidden units
set.seed(1)  # For reproducibility

# Checking the documentation and data classes (for educational purposes, not needed for the model)
data_normalized_classes <- sapply(data_normalized, class)
print(data_normalized_classes)

# --- Data Preparation Correction ---
data_transformed <- data.frame(model.matrix(~ . - 1 - target, data = trainset.bal))
target_variable <- trainset.bal$target

# Normalize the data (only features)
preproc <- preProcess(data_transformed, method = c("center", "scale"))
features_normalized <- predict(preproc, data_transformed)

# Combine the normalized features with the original target variable
data_normalized <- cbind(features_normalized, target0 = target_variable)

variable_names <- names(data_normalized)
variable_names

selected_variables <- c(
  "avg_consom_l_1_ELEC", "avg_consom_l_1_GAZ",
  "var_consom_l_1_ELEC", "var_consom_l_1_GAZ",
  "sd_consom_l_1_ELEC", "sd_consom_l_1_GAZ",
  "median_consom_l_1_ELEC", "median_consom_l_1_GAZ",
  "mode_consom_l_1_ELEC", "mode_consom_l_1_GAZ",
  "avg_diff_consom_l_1_ELEC", "avg_diff_consom_l_1_GAZ",
  "range_consom_l_1_ELEC", "range_consom_l_1_GAZ",
  "sd_cumsum_consommation_level_1_ELEC", "sd_cumsum_consommation_level_1_GAZ",
  "avg_cumsum_consommation_level_1_ELEC", "avg_cumsum_consommation_level_1_GAZ",
  "median_cumsum_consommation_level_1_ELEC", "median_cumsum_consommation_level_1_GAZ","target0")

data_normalized_selected <- data_normalized[, selected_variables]
str(data_normalized_selected)
nn_model <- neuralnet(target0 ~ ., data = data_normalized_selected, hidden = 2, linear.output = FALSE, err.fct = "ce")

plot(nn_model)
nn_model$net.result
nn_model$result.matrix
pred.m2 <- ifelse(unlist(nn_model$net.result) > 0.5, 1, 0)
positive_class_probabilities <- nn_model$net.result[[1]][, 2]
cat('Trainset Confusion Matrix with neuralnet (1 hidden layer, 2 hidden nodes, Scaled X):')
table(data_normalized_selected$target0, pred.m2)

length(data_normalized_selected$target0)
length(pred.m2)

positive_class_probabilities <- nn_model$net.result[[1]][, 2]

# Converting probabilities to class predictions based on a 0.5 threshold
pred.m2 <- ifelse(positive_class_probabilities > 0.5, 1, 0  )
confusion_matrix <- table(data_normalized_selected$target0, pred.m2)
print(confusion_matrix)

##unbalanced##
unbal_data_transformed <- data.frame(model.matrix(~ . - 1 - target, data = trainset))
unbal_target_variable <- trainset$target

# Normalize the data (only features)
preproc <- preProcess(unbal_data_transformed, method = c("center", "scale"))
unbal_features_normalized <- predict(preproc, unbal_data_transformed)

# Combine the normalized features with the original target variable
unbal_data_normalized <- cbind(unbal_features_normalized, target0 = unbal_target_variable)

unbal_data_normalized_selected <- unbal_data_normalized[, selected_variables]
str(data_normalized_selected)
unbal_nn_model <- neuralnet(target0 ~ ., data = unbal_data_normalized_selected, hidden = 2, linear.output = FALSE, err.fct = "ce")

##evalutaion for unbalanced data##
unbal_data_test_transformed<- data.frame(model.matrix(~ . - 1, data = testset))
test_variable <- unbal_data_normalized[, ncol(unbal_data_test_transformed)]
test_features <- unbal_data_normalized[, -ncol(unbal_data_test_transformed)]
test_data_classes <- sapply(unbal_data_normalized, class)
print(test_data_classes)
test_data <- cbind(test_features, target0 = test_variable)
test_data <- unbal_data_normalized[, selected_variables]
str(test_data)
test_data_matrix <- as.matrix(test_data[,-which(names(test_data) == "target0")])
unbal_nn_predictions <- compute(unbal_nn_model, test_data_matrix)
predicted_labels <- ifelse(unbal_nn_predictions$net.result[, 1] > 0.5, 1, 0)
actual_labels <- test_data$target0
conf_matrix <- table(Actual = actual_labels, Predicted = predicted_labels)
print(conf_matrix)

##performance evaluation for balanced data)
data_test_transformed <- data.frame(model.matrix(~ . - 1, data = testset))
test_variable <- data_normalized[, ncol(data_test_transformed)]
test_features <- data_normalized[, -ncol(data_test_transformed)]
test_data_classes <- sapply(data_normalized, class)
print(test_data_classes)
test_data <- cbind(test_features, target0 = test_variable)
test_data <- data_normalized[, selected_variables]
str(test_data)
nn_predictions <- compute(nn_model, test_data[,-which(names(test_data) == "target0")])

predicted_classes <- ifelse(nn_predictions$net.result > 0.5, 1, 0)
actual_classes <- test_data$target0  
positive_class_probabilities <- nn_predictions$net.result[, 2]
predicted_classes <- ifelse(positive_class_probabilities > 0.5, 1, 0)
conf_matrix <- table(Actual = actual_classes, Predicted = predicted_classes)
print(conf_matrix)
