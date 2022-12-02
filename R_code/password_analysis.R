# Setup -----------------------------------------------------------------------------------------------------------

cat("\014")  # ctrl+L
#if (!is.null(dev.list()["RStudioGD"])) { dev.off(dev.list()["RStudioGD"]) }  # remove all plots
try(dev.off(dev.list()["RStudioGD"]), silent=T)
rm(list = ls())  # remove all variables
set.seed(100)

# Imports
library(dplyr)
library(ggplot2)
library(sqldf)
library(stringr)

# Set WD to directory of current script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data Loading ----------------------------------------------------------------------------------------------------

df = read.csv("top_200_password_2020_by_country.csv")
names(df) = c("country_code", "country_name", "password_rank_country", "password", "user_count", "time_to_crack", "password_rank_global", "time_to_crack_seconds")
df$time_to_crack = NULL

# Functions -------------------------------------------------------------------------------------------------------

has_lower_letters = function(str.in) grepl("[a-z]", str.in)
has_upper_letters = function(str.in) grepl("[A-Z]", str.in)
has_numbers = function(str.in) grepl("[0-9]", str.in)
has_special_chars = function(str.in) grepl("[^0-9A-Za-z]", str.in)

# need a function that turns a password into a number which represents the number of seconds it will take to crack using brute force methods under certain circumstances
# these 2-3 letter acronyms are just the abbreviations for the columns in the "Feature Engineering" section directly below
time_to_crack_brute = function(len, hll, hul, hn, hsc) {
  cracks_per_second = 10000000
  password_length = len %>% as.numeric
  num_possibilites = 0
  if (hll) num_possibilites = num_possibilites + 26
  if (hul) num_possibilites = num_possibilites + 26
  if (hn) num_possibilites = num_possibilites + 10
  if (hsc) num_possibilites = num_possibilites + 31
  (num_possibilites ** password_length) / cracks_per_second
}

# these numbers in the square brackets are the column numbers for the data frame they represent
ttcb_driver = function(x, output) time_to_crack_brute(x[8], x[9], x[10], x[11], x[12])

# input a password, and this function will give you the time to crack (same as time_to_crack_brute(), but simpler to use)
ttcb_std = function(password.in) {
  len = password.in %>% str_length
  hll = password.in %>% has_lower_letters
  hul = password.in %>% has_upper_letters
  hn = password.in %>% has_numbers
  hsc = password.in %>% has_special_chars
  time_to_crack_brute(len, hll, hul, hn, hsc)
}

# Feature Engineering ---------------------------------------------------------------------------------------------

df$password_length = df$password %>% str_length %>% as.numeric
df$has_lower_letters = df$password %>% has_lower_letters
df$has_upper_letters = df$password %>% has_upper_letters
df$has_numbers = df$password %>% has_numbers
df$has_special_chars = df$password %>% has_special_chars
df$time_to_crack_brute = apply(df, 1, ttcb_driver)

# Data Formatting and Exploration ---------------------------------------------------------------------------------

# same thing
df %>% head
"SELECT * FROM df LIMIT 6" %>% sqldf

# find number of rows in data
"SELECT COUNT(1) FROM df" %>% sqldf

# find number of unique passwords
"SELECT COUNT(DISTINCT password) FROM df" %>% sqldf

# find amount of rows in data
"SELECT COUNT(1) FROM df" %>% sqldf

# find unique countries
"SELECT DISTINCT country_name FROM df LIMIT 6" %>% sqldf

# find amount of unique countries
"SELECT COUNT(DISTINCT country_name) FROM df" %>% sqldf

# find all passwords that take longer than 0 seconds to crack and order them in ascending order
"SELECT * FROM df WHERE time_to_crack_seconds > 0 ORDER BY time_to_crack_seconds DESC LIMIT 10" %>% sqldf

# find count of distinct cracking times and rank in descending order
"SELECT time_to_crack_seconds, COUNT(time_to_crack_seconds) AS count FROM df GROUP BY time_to_crack_seconds ORDER BY time_to_crack_seconds DESC LIMIT 10" %>% sqldf

# same as above but use time_to_crack_brute instead
"SELECT time_to_crack_brute, COUNT(time_to_crack_brute) AS count FROM df GROUP BY time_to_crack_brute ORDER BY time_to_crack_brute DESC LIMIT 10" %>% sqldf

# find the top 10 hardest passwords to brute force
"SELECT password, time_to_crack_seconds, time_to_crack_brute FROM df ORDER BY time_to_crack_brute DESC LIMIT 10" %>% sqldf

# amount of seconds it would take to crack password (histogram)
# the log10 means that this is the amount of digits in the seconds value, remove it to see the actual value in seconds
ggplot(df) + geom_histogram(aes(x=log10(time_to_crack_brute)), bins=25)

# number of passwords grouped by char count
ggplot(df) + geom_bar(aes(x=password_length))

# find the top 10 shortest passwords
"SELECT DISTINCT(password) FROM df ORDER BY password_length LIMIT 10" %>% sqldf

# bar graph for character types
ggplot() + geom_col(aes(x=c("hll", "hul", "hn", "hsc"), y=c(sum(df$has_lower_letters), sum(df$has_upper_letters), sum(df$has_numbers), sum(df$has_special_chars))))

# find the amount of unique countries in the dataset
"SELECT COUNT(DISTINCT(country_name)) FROM df" %>% sqldf

# find most commonly occuring password in dataset
# put another way, find how many countries have this password as top 200
df %>% count(password) %>% arrange(desc(n)) %>% head

# Export Data -----------------------------------------------------------------------------------------------------

# write.csv(df, "top_200_passwords_feature_engineered.csv", row.names=F)
