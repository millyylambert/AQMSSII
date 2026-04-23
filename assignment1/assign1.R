# Assignment 1
# AQMSSII, Spring 2026
# Romilly Lambert 

# This file will contain my solutions for Assingment 1
print("Hello, git!")
install.packages("gapminder")
library(gapminder)
write.csv(gapminder, "data/gapminder.csv", row.names = FALSE)
getwd()
setwd("/Users/romillylambert/Desktop/AQMSSII")
list.files()
list.files("data")
setwd("/Users/romillylambert/Desktop/AQMSSII")
library(gapminder)
write.csv(gapminder, "data/gapminder.csv", row.names = FALSE)
