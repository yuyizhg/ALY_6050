library(ggplot2)
library(grid)
library(vcd)


# Problem 1
R <- runif(1000)
X <- -log(R)


# 1. Create a relative frequency histogram of X.
hist(X, main = 'Histogram of X', breaks = 25, xlim = c(0, 8), col = '#ffffe6')


# 2. Select a probability distribution that, in your judgement, is the best fit for X.
theta <- mean(X)
avrg <- theta
std.dv <- theta
## F(x) = -theta Ln(r) = -Ln(r)


# 3. Support your assertion above by creating a probability plot for X.
range <- seq(0, avrg + 6* std.dv, 0.01)
Y <- dexp(range, theta)
plot(range, Y, type = 'l', ylim = c(0, max(Y) + 0.01), main = 'Probability Plot of X')

Z <- dexp(X, theta)
plot(data.frame(x = X, y = Z), main = 'Empirical Plot of X')


# 4. Support your assertion above by performing a Chi-squared test of best fit with a 0.05 level of significance.

## Null hypothesis H0: the random variable follows the exponential distribution
## Alternative hypothesis H1: the random variable does not follow the exponential distribution

observed <- X
prob.exp <- dexp(R, rate = theta)
chisq.test(X, p = prob.exp, rescale.p = TRUE)

## so there isn't sufficient evidence to reject H0. 




# Problem 2
R1 <- runif(10000)
R2 <- runif(10000)
R3 <- runif(10000)
X <- -log(R1*R2*R3)

# 1. Create a relative frequency histogram of X.
hist(X, main = 'Histogram of X', breaks = 25, ylim = c(0, 1500), col = '#ffffe6')


# 2. Select a probability distribution that, in your judgement, is the best fit for X.
## n=3, rate = 1, Gamma(3,1)


# 3. Support your assertion above by creating a probability plot for X.
alpha <- 3
beta <- 1
avrg <- alpha * beta
std.dv <- sqrt(alpha*beta^2)

range <- seq(0, avrg + 5*std.dv, 0.01)
y <- dgamma(range, alpha, rate = 1/beta)
plot(range, y, type = 'l', ylim = c(0, max(y)+0.01), main = 'Probability Plot of X')

Z <- dgamma(X, alpha, rate = 1/beta)
plot(data.frame(x = X, y = Z), main = 'Empirical Plot of X')

# 4. Support your assertion above by performing a Chi-squared test of best fit with a 0.05 level of significance.

## Null hypothesis H0: the random variable follows the Gamma distribution
## Alternative hypothesis H1: the random variable does not follow the Gamma distribution

observed <- X
prob.exp <- pgamma(X, alpha, rate = 1/beta)
chisq.test(X, p = prob.exp, rescale.p = TRUE)

## p-value = 1, so there isn't sufficient evidence to reject H0. 





# Problem 3
R1 <- runif(1000)
R2 <- runif(1000)

X1 <- -log(R1)
X2 <- -log(R2)

K <- (X1 - 1) ^ 2 / 2
Y = vector()

for (i in 1:1000) {
  print(i)
  if(X2[i] >= K[i]) {
    R <- runif(1)
    message('assigning ', X2[i], K[i], R)
    if(R > 0.5) {
      Y <- c(Y, X1[i])
    } else {
      Y <- c(Y, -X1[i])
    } 
  } else {
    message('skipping ', X2[i], X1[i], K[i])
  }
}


# 1. Create a relative frequency histogram of Y
hist(Y, main = 'Histogram of Y', breaks = 20, ylim = c(0, 180), col = '#ffffe6')


# 2. Select a probability distribution that, in your judgement, is the best fit for Y.
Mu <- mean(Y)
Sigma <- sd(Y)


# 3. Support your assertion above by creating a probability plot for Y.
range <- seq(Mu - 4*Sigma, Mu + 4*Sigma, 0.01)
y <- dnorm(range, Mu, Sigma)
plot(range, y, main = 'Probability Plot of Y', ylim = c(0, max(y) + 0.01), axis = FALSE)
axis(1, at = seq(Mu - 3*Sigma, Mu + 3*Sigma, Sigma))

# 4. Support your assertion above by performing a Chi-squared test of best fit with a 0.05 level of significance.

## Null hypothesis H0: the random variable follows the Normal distribution
## Alternative hypothesis H1: the random variable does not follow the Normal distribution
observed <- Y + 3
prob.exp <- pnorm(observed, mean(observed), sd(observed))
chisq.test(observed, p = prob.exp, rescale.p = TRUE)

## p-value = 1, so there isn't sufficient evidence to reject H0. 





# Problem 4
  
R1 <- runif(1000)
R2 <- runif(1000)

X1 <- -log(R1)
X2 <- -log(R2)

K <- (X1 - 1) ^ 2 / 2
Y = vector()
N = vector()
W = vector()
for (i in 1:1000) {
  print(i)
  if(X2[i] >= K[i]) {
    R <- runif(1)
    #message('assigning ', X2[i], K[i], R)
    if(R > 0.5) {
      Y <- c(Y, X1[i])
    } else {
      Y <- c(Y, -X1[i])
    }
  } else {
    #message('skipping ', X2[i], X1[i], K[i])
  }
  N <- c(N, length(Y))
  W <- c(W, i / length(Y))
}

W[1] <- 0

# 1. Estimate the expected value and the standard deviation of W
mu <- mean(W)
sigma <- sd(W)


# 2. Select a probability distribution that, in your judgement, is the best fit for W
hist(W, main = 'Histogram of W', col = '#ffffe6')


# 3. Support your assertion above by performing a Chi-squared test of best fit with a 0.05 level of significance.

## Null hypothesis H0: the random variable follows the Normal distribution
## Alternative hypothesis H1: the random variable does not follow the Normal distribution
#observed <- W
#prob.exp <- pnorm(observed, mean(observed), sd(observed))
#chisq.test(observed, p = prob.exp, rescale.p = TRUE)


## p-value = 2.2e-16, so there is sufficient evidence to reject H0. 

observed <- W
prob.exp <- dnorm(observed, mean(observed), sd(observed))
chisq.test(observed, p = prob.exp, rescale.p = TRUE)



# 4. As the number of iterations M becomes larger, the values W will approach a certain limiting value. Investigate this limiting value of W by completing the following table and plotting W versus M. What value do you propose for the limiting value that W approaches to?

JW = vector()
JW <- c(JW, W[10], W[20], W[30], W[40], W[50], W[60], W[70], W[80], W[90], W[100], W[200], W[300], W[400], W[500], W[600], W[700], W[800], W[900], W[1000])

