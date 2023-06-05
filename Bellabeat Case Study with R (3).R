# Case Study 2: How Can a Wellness Technology Company Play It Smart?

# Scenario
# You are a junior data analyst working on the marketing analyst team at Bellabeat. Urška Sršen, co-founder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opportunities for the company. You have been asked to focus on one of Bellabeat’s products and analyze smart device data to gain insight into how consumers are using their smart devices. The insights you discover will then help guide marketing strategy for the company. You will present your analysis to the Bellabeat executive team along with your high-level recommendations for Bellabeat’s marketing strategy. 

# Question for the analysis
# What are some trends in smart device usage?
# How could these trends apply to Bellabeat customers?
# How could these trends help influence Bellabeat marketing strategy?
  
# Business task
# Menganalisis data penggunaan perangkat pintar untuk membuka peluang pertumbuhan baru dan insight yang dihasilkan dapat mengembangkan strategi pemasaran bagi perusahaan.

# Installing and loading the packages
install.packages('tidyverse')
install.packages('lubridate')
install.packages('tidyr')

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

# Importing dataset
activity <- read.csv("dailyActivity_merged.csv")
calories <- read.csv("hourlyCalories_merged.csv")
intensities <- read.csv("hourlyIntensities_merged.csv")
sleep <- read.csv("sleepDay_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")

# Getting to know the data
glimpse(activity)
glimpse(calories)
glimpse(intensities)
glimpse(sleep)
glimpse(weight)
class(activity$ActivityDate) # --> mengecek struktur tipe data

# Manipulating and fixing the data
# activity
activity$ActivityDate = as.POSIXct(activity$ActivityDate, format = "%m/%d/%Y", 
                                 tz = Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
# calories
calories$ActivityHour = as.POSIXct(calories$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", 
                                 tz = Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
# intensities
intensities$ActivityHour = as.POSIXct(intensities$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p",
                                    tz = Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
# sleep
sleep$SleepDay = as.POSIXct(sleep$SleepDay, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")

# Exploring and summarizing the data
# Distinct "id" value
n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

# Observations each dataframe
nrow(activity)
nrow(calories)
nrow(intensities)
nrow(sleep)
nrow(weight)

# Summary statistic
# Activity
activity %>% 
  select(TotalSteps, TotalDistance, SedentaryMinutes, Calories) %>% 
  summary()
activity %>% 
  select(VeryActiveMinutes, LightlyActiveMinutes, FairlyActiveMinutes) %>% 
  summary()

# Calories
calories %>% 
  select(Calories) %>% 
  summary()

# Sleep
sleep %>% 
  select(TotalTimeInBed, TotalMinutesAsleep, TotalSleepRecords) %>% 
  summary()

# Weight
weight %>% 
  select(BMI, WeightKg) %>% 
  summary()

# Merging between activity and sleep data (for visualization)
merged_data1 <- merge(sleep,activity, by = c("Id", "date"))
n_distinct(merged_data1)

# Visualization
# Activity
ggplot(data=activity, aes(x=TotalSteps, y=Calories)) +
  geom_point() + geom_smooth() + labs(title = "Total Steps vs. Calories") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Calories") + xlab("Total Steps")

# Sleep
ggplot(data=sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) +
  geom_point() + labs(title = "Total Minutes Asleep vs. Total Time in Bed") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_jitter() +
  ylab("Total Time In Bed") + xlab("Total Minutes Asleep")

# Intensities
intensities2 <- intensities %>% 
  group_by(time) %>% 
  drop_na() %>% 
  summarise(mean_int= mean(TotalIntensity))

ggplot(data=intensities2, aes(x=time, y=mean_int)) +
  geom_histogram(stat = "identity", fill = "lightblue") + 
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Avg Total Intensity") + xlab("Time") +
  labs(title = "Average Total Intensity vs. Time")

# Merged data (activity & sleep)
ggplot(data=merged_data1, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) +
  geom_point(color="darkblue") + geom_smooth() + labs(title = "Minutes Asleep vs. Sedentary Minutes") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Sedentary Minutes") + xlab("Total Minutes Asleep")