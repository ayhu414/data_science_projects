library(tidyverse)
library(ggplot2)
library(stringr)

raw_data <- read_csv("assignment1/assignment_1_data.csv")

d <- hist(raw_data$income)
plot(d)

h <- hist(raw_data$dist)
plot(h)

# exploring the dataset
glimpse(raw_data)

# 1) finding inconsistencies in data
# checking visually and forming summary statistics for each col

plt_pscore <- hist(raw_data$past_score)
plot(plt_pscore)
raw_data %>% 
  summarize(
    mean_past_score = mean(past_score, na.rm = TRUE),
    median_past_score = median(past_score, na.rm = TRUE),
    sd_past_score = sd(past_score),
    max_past_score = max(past_score),
    min_past_score = min(past_score)
  )
sum(is.na(raw_data$past_score))

#returns: Mean, Median, Sd,   Max,  Min,  sum(is.na)
#         8.00, 8.01,   1.41, 13.2, 3.24, 0

plt_dist <- hist(raw_data$dist)
plot(pst_dist)
raw_data %>% 
  summarize(
    mean_dist = mean(dist, na.rm = TRUE),
    median_dist = median(dist, na.rm = TRUE),
    sd_dist = sd(dist),
    max_dist = max(dist),
    min_dist = min(dist)
  )
sum(is.na(raw_data$dist))

#returns: Mean, Median, Sd,   Max,  Min,  sum(is.na)
#         1836, 7.95  , 3239, 13609,0.753,0

plt_inc <- hist(raw_data$income)
plot(plt_inc)
raw_data %>% 
  summarize(
    mean_dist = mean(income, na.rm = TRUE),
    median_income = median(income, na.rm = TRUE),
    sd_income = sd(income),
    max_income = max(income),
    min_income = min(income)
  )
sum(is.na(raw_data$income))
sum(raw_data$income == -99)
sum(raw_data$income <= 0)

#returns: Mean, Median, Sd,   Max,  Min,  sum(is.na), sum(income <= 0)
#         1836, 7.95  , 3239, 13609,0.753,0         , 1115

plt_lab <- hist(raw_data$lab)
plot(plt_lab)
raw_data %>% 
  summarize(
    mean_dist = mean(lab, na.rm = TRUE),
    median_lab = median(lab, na.rm = TRUE),
    sd_lab = sd(lab),
    max_lab = max(lab),
    min_lab = min(lab)
  )
sum(is.na(raw_data$lab))
sum(raw_data$lab < 0)
sum(raw_data$lab > 1)

#returns: Mean, Median, Sd   ,   Max,  Min,  sum(is.na), sum(lab <0), sum(lab >1)
#         0.304,0     , 0.562,   3  ,  -1 ,  0         , 206        , 104

plt_score <- hist(raw_data$score)
plot(plt_score)
raw_data %>% 
  summarize(
    mean_dist = mean(score, na.rm = TRUE),
    median_score = median(score, na.rm = TRUE),
    sd_score = sd(score),
    max_score = max(score),
    min_score = min(score)
  )
sum(is.na(raw_data$score))
sum(raw_data$score < 200)
sum(raw_data$score > 17)

#returns: Mean, Median, Sd   ,Max,  Min,  sum(is.na), sum(score <0), sum(lab <200)
#         36.3  8.25   152.   1313. 1.88 ,0         , 0           , 9650
#note, try to make a histogram of the super small values and see whats going on

unique(raw_data$gender)
table(raw_data$gender)

#returns:     F female      M   male 
#          3344   1576   3516   1564

unique(raw_data$race)
table(raw_data$race)

#       A        B    black        H hispanic        W 
#     220     1236      358     5426     1458     1302

plt_income_bucket <- hist(raw_data$income_bucket)
plot(plt_income_bucket)
raw_data %>% 
  summarize(
    mean_income_bucket = mean(income_bucket, na.rm = TRUE),
    median_income_bucket = median(income_bucket, na.rm = TRUE),
    sd_income_bucket = sd(income_bucket),
    max_income_bucket = max(income_bucket),
    min_income_bucket = min(income_bucket)
  )
sum(is.na(raw_data$income_bucket))

#  mean_income_bucket median_income_bucket sd_income_bucket, na
#  1          2.99    3                    1.18              0

unique(raw_data$plays_sports)
table(raw_data$plays_sports)

#         0          1   baseball basketball gymnastics     soccer     tennis volleyball 
#      5950       2050        324        332        350        352        354        288 

table(raw_data$student_id)

#too long to print, summarized as: each student id appears twice
#note: how does people who dont go to lab school have ids? is it just a normal id?
# if so, i will eliminate from each of the pairings
# notibly, if one has odd/inconsistent information, i will eliminate that one
# assuming that one of the double entries have wrong information
# ie, -99s in income, consistent double entries

plt_lat <- hist(raw_data$lat)
plot(plt_lat)
raw_data %>% 
  summarize(
    mean_lat = mean(lat, na.rm = TRUE),
    median_lat = median(lat, na.rm = TRUE),
    sd_lat = sd(lat),
    max_lat = max(lat),
    min_lat = min(lat)
  )
sum(is.na(raw_data$lat))

#  mean_lat median_lat sd_lat max_lat min_lat (is.na = 0)
#  4.47     0          12.9   41.9    0

plt_long <- hist(raw_data$long)
plot(plt_long)
raw_data %>% 
  summarize(
    mean_long = mean(long, na.rm = TRUE),
    median_long = median(long, na.rm = TRUE),
    sd_long = sd(long),
    max_long = max(long),
    min_long = min(long)
  )
sum(is.na(raw_data$long))

#mean_long median_long sd_long max_long min_long (is.na = 0)
#-9.37     0           27.1    0        -87.7


#Development
my_summary <- function(x){
#input: observations of one column
#return: NA
#
#prints a set of summary statistics,
#plots distribution of data, and finds NAs
  summarize(
    mean_x = mean(x, na.rm = TRUE),
    median_x = median(x, na.rm = TRUE),
    sd_x = sd(x),
    max_x = max(x),
    min_x = min(x)
  )
  sum(is.na(raw_data$x))
}

apply(X = raw_data, FUN = my_summary, MARGIN = 2)
# to not touch the original raw data
working_df <- raw_data
