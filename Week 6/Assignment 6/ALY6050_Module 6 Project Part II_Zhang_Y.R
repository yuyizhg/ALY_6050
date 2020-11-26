library(mFilter)
library(quantmod)

# Data preparation
data <- read.csv("/Users/yuyizhang/FCR/NEU/CPS/Analytics_2018/ALY 6050_Introduction to Enterprise Analytics/Week 6/Assignment 6/Week 6 Project-part 2-Data.csv", header = FALSE)
hon <- as.matrix(data[, 5])

# Decompose the stock price
hon.hp <- hpfilter(log(hon), 1600)

# Plots
plot(hon.hp, ask = F)

