library(tidyverse)
library(ggplot2)
library(summarytools)

#read dataset into youtube_data
youtube_data <- read.csv("youtubers_df_clean.csv")

#Exploratory Data Analysis
head(youtube_data)
str(youtube_data)
summary(youtube_data)
dim(youtube_data)
colSums(is.na(youtube_data))
dfSummary(youtube_data)

# Correct misspelt column name
youtube_data <- youtube_data %>%
  rename(Subscribers=Suscribers)

#Checking for outliers
ggplot(youtube_data, aes(y=Visits)) +
  geom_boxplot()
# Calculate IQR for 'visit' column
Q1_visit <- quantile(youtube_data$Visits, 0.25)
Q3_visit <- quantile(youtube_data$Visits, 0.75)

IQR_visit <- Q3_visit - Q1_visit

lower_bound_visit <- Q1_visit - 1.5 * IQR_visit
upper_bound_visit <- Q3_visit + 1.5 * IQR_visit

outliers_visit <- which(youtube_data$Visits < lower_bound_visit | youtube_data$Visits > upper_bound_visit)
outliers_visit

# Calculate IQR for 'Likes' column
Q1_like <- quantile(youtube_data$Likes, 0.25)
Q3_like <- quantile(youtube_data$Likes, 0.75)

IQR_like <- Q3_like - Q1_like

lower_bound_like <- Q1_like - 1.5 * IQR_like
upper_bound_like <- Q3_like + 1.5 * IQR_like

outliers_likes <- which(youtube_data$Likes < lower_bound_like | youtube_data$Likes > upper_bound_like)
outliers_likes

# Calculate IQR for 'Comments' column
Q1_comment <- quantile(youtube_data$Comments, 0.25)
Q3_comment <- quantile(youtube_data$Comments, 0.75)

IQR_comment <- Q3_comment - Q1_comment

lower_bound_comment <- Q1_comment - 1.5 * IQR_comment
upper_bound_comment <- Q3_comment + 1.5 * IQR_comment

outliers_comments <- which(youtube_data$Comments < lower_bound_comment | youtube_data$Comments > upper_bound_comment)
outliers_comments

# TREND ANALYSIS
#Most Popular Categories

#filter out empty rows in the 'category' columns
filtered_category <- youtube_data %>%
  filter(Categories != "")

category_counts <- filtered_category %>%
  count(Categories) %>%
  arrange(desc(n))

# Export category_count as a csv file
write.csv(category_counts,
          'category_counts.csv', row.names = FALSE)

# CATEGORY BY COUNTRY
category_by_country <- youtube_data %>%
  group_by(Country, Categories) %>%
  summarise(num_streamers =n()) %>%
  filter(!is.na(Categories) & 
           Categories != '' & Country != 'Unknown') %>%
  arrange(Country, desc(num_streamers))
View(category_by_country)

write.csv(category_by_country, "category_by_country.csv",
          row.names= FALSE)

# RELATIONSHIP BETWEEN CONTENT CATEGORIES AND ENGAGEMENT METRICS

#count the number of occurrences per category
occurences_by_category <- youtube_data %>%
  group_by(Categories) %>%
  summarise(
    num_streamers = n(),
    avg_likes = mean(Likes, na.rm= TRUE),
    avg_comments = mean(Comments, na.rm = TRUE),
    avg_subscribers = mean(Subscribers, na.rm = TRUE)
  )

# Remove empty rows
clean_occurences_by_category <- occurences_by_category %>%
  filter(!is.na(Categories) & 
           Categories != '')

write.csv(clean_occurences_by_category, 'clean_occurences_by_category.csv',
          row.names = FALSE)

#Calculate the average subscribers
avg_subscribers = mean(youtube_data$Subscribers, na.rm=TRUE)
avg_subscribers
# filter streamers with above average subscribers
above_average_streamers <- youtube_data %>%
  filter(
    Subscribers > avg_subscribers
    ) %>%
  select(Username, Subscribers) 
above_average_streamers

write.csv(above_average_streamers, 'above_average_streamers.csv', row.names=FALSE)
