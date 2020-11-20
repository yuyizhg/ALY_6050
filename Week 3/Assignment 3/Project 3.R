library(forecast)


# 1. Perform exponential smoothing forecasts on the Honeywell stock prices to forecast the price for 4/16/2018.
data <- read.csv("/Users/yuyizhang/FCR/NEU/CPS/Analytics_2018/ALY 6050_Introduction to Enterprise Analytics/Week 3/Assignment 3/Honeywell.csv")
hon.ts <- ts(data[, 3])
plot.ts(hon.ts, main = 'Honeywell daily stock prices from 2017-2018', ylab = 'Price')

ses.hon15 <- ses(hon.ts, alpha = 0.15)
summary(ses.hon15)
autoplot(ses.hon15)

ses.hon35 <- ses(hon.ts, alpha = 0.35)
summary(ses.hon35)
autoplot(ses.hon35)

ses.hon55 <- ses(hon.ts, alpha = 0.55)
summary(ses.hon55)
autoplot(ses.hon55)

ses.hon75 <- ses(hon.ts, alpha = 0.75)
summary(ses.hon75)
autoplot(ses.hon75)



# 2. Use your exponential smoothing forecast with 𝜶=𝟎.75, and perform adjusted exponential smoothing forecasts on the Honeywell stock prices to forecast the price for 4/16/2018.
des.hon15 <- holt(hon.ts, alpha = 0.75, beta = 0.15)
summary(des.hon15)
autoplot(des.hon15)

des.hon25 <- holt(hon.ts, alpha = 0.75, beta = 0.25)
summary(des.hon25)
autoplot(des.hon25)

des.hon45 <- holt(hon.ts, alpha = 0.75, beta = 0.45)
summary(des.hon45)
autoplot(des.hon45)

des.hon85 <- holt(hon.ts, alpha = 0.75, beta = 0.85)
summary(des.hon85)
autoplot(des.hon85)



# 3. Perform a simple regression analysis of Honeywell stock prices versus periods (i.e.,1, 2, 3,…) to forecast the Honeywell stock value for 4/16/2018.
regression <- lm(data$Close ~ data$Period)
regression.summ <- summary(regression)
## stockprice <- 150.4 + 0.008926*period
hon.127 <- 150.4 + 0.008926 * 127
hon.MSE <- mean(regression.summ$residuals^2)


# a. Coefficients of correlation and determination, and the interpretations of their values
cc <- sqrt(0.003981)


# b. A histogram of the regression residuals, and the interpretation of its shape
regression.res <- resid(regression)
hist(regression.res, main = 'Histogram of Residuals', col = '#ffffe6')


# c. A Chi-squared normality test of the residuals, and the interpretation of its outputs
## Null hypothesis H0: the random variable follows the Normal distribution
## Alternative hypothesis H1: the random variable does not follow the Normal distribution
observed <- regression.res + 10
prob.exp <- pnorm(observed, mean(observed), sd(observed))
chisq.test(observed, p = prob.exp, rescale.p = TRUE)
## p-value = 1, so there isn't sufficient evidence to reject H0. 


# d. A Normal probability plot of the residuals
qqnorm(regression.res)
qqline(regression.res, col = "red")


# e. A scatter plot of residuals versus time to study their independency, and the interpretation of the shape of the scatter plot
plot(data$Period, regression.res, main = 'Residual Scatter Plots versus Time')


# f. A scatter plot of residuals versus the predicted stock values to study their homoscedasticity, and the interpretation of the shape of the scatter plot
## stockprice <- 150.4 + 0.008926*period
hon.pred <- 150.4 + 0.008926 * data$Period
plot(hon.pred, regression.res, main = 'Residual Scatter Plots versus Predicted Stock Values')


