# Perform a simulation analysis consisting of 5,000 simulations to determine:
# a. Average number of victims that can be expected at each hospital.
mu <- 150
mu.bim <- mu * 0.2
mu.tm <- mu * 0.15
mu.mg <- mu * 0.3
mu.bm <- mu * 0.25
mu.bw <- mu * 0.1


# b. For each hospital, the average total time (in hours) needed to transport all victims.
time.bim <- mu.bim * 15 / 60
time.tm <- mu.tm * 10 / 60
time.mg <- mu.mg * 7 / 60
time.bm <- mu.bm * 15 / 60
time.bw <- mu.bw * 20 / 60


# c. For part (a) above, create a chart the displays the Law of Large Numbers in action for the Beth Israel Medical. (Law of large numbers: As the number of trials becomes larger, the observed averages approach to the theoretical average.)
v5000 <- rnorm(5000, 150, 50)
vb5000 <- v5000 * 0.2
m5000 = vector()

for (i in 1:5000) {
  m5000 <- c(m5000, mean(vb5000[1:i]))
}

plot(1:5000, m5000, type = "l", lwd = 2, col = 'blue', main = 'Law of Large Numbers in Total Transport Time for Beth Israel Medical', xlab = 'Similations', ylab = 'Number of Victims')


# d. For the Beth Israel Medical hospital, perform an exploratory data analysis of the total transport time by:
#  i. Calculating a 95% confidence interval for the total transport time
r.bim <- rnorm(5000, 15, 3)
tb5000 <- vb5000 * r.bim / 60
xbar <- mean(tb5000)
s <- sd(tb5000)
error <- qnorm(0.975) * s / sqrt(5000)
left <- xbar - error
right <- xbar + error


#  ii. Determining a probability distribution that best fits the total transport time (in hours).
hist(tb5000, main = 'Histogram of Total Transport Time for Beth Israel Medical', col = '#ffffe6', xlab = 'Total Transport Time in hours')
xbar
s


#  iii. Supporting your assertion in part (ii) by creating a frequency distribution and performing a Chi-squared Goodness of fit test.
## Null hypothesis H0: the random variable follows the Normal distribution
## Alternative hypothesis H1: the random variable does not follow the Normal distribution
observed <- tb5000 + 5
prob.exp <- pnorm(observed, xbar, s)
chisq.test(observed, p = prob.exp, rescale.p = TRUE)

## p-value = 1, so there isn't sufficient evidence to reject H0. 


# e. Let 𝒕 denote the average transport time (in minutes) per victim for the entire process of transporting all victims. Perform an exploratory data analysis of 𝒕.
r.bim <- rnorm(5000, 15, 3)
r.tm <- rnorm(5000, 10, 4)
r.mg <- rnorm(5000, 7,2)
r.bm <- rnorm(5000, 15, 5)
r.bw <- rnorm(5000, 20, 3)

t <- 0.2 * r.bim + 0.15 * r.tm + 0.3 * r.mg + 0.25 * r.bm + 0.1 * r.bw


#  i. Calculating a 95% confidence interval for t
t.xbar <- mean(t)
t.s <- sd(t)
t.error <- qnorm(0.975) * t.s / sqrt(5000)
t.left <- t.xbar - error
t.right <- t.xbar + error


#  ii. Determining a probability distribution that best fits t.
hist(t, main = 'Histogram of Average Transport Time for All Victims', col = '#ffffe6', xlab = 'Average Transport Time in minutes')
t.xbar
t.s


#  iii. Supporting your assertion in part (ii) by creating a frequency distribution and performing a Chi-squared Goodness of fit test.
## Null hypothesis H0: the random variable follows the Normal distribution
## Alternative hypothesis H1: the random variable does not follow the Normal distribution
observed <- t
prob.exp <- pnorm(observed, t.xbar, t.s)
chisq.test(observed, p = prob.exp, rescale.p = TRUE)

## p-value = 1, so there isn't sufficient evidence to reject H0. 

