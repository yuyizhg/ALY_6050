library(triangle)

# 1. Perform a simulation consisting of 1000 occurrences, and calculate the minimum total cost for each occurrence.
ad1000 <- rtriangle(1000, 13000, 16000, 14000) 
orderq1000 <- vector()

for (i in 1:1000) {
  ttcost <- function(orderq) {9.6 * orderq + 210 * ad1000[i] / orderq + 60 * ad1000[i]}
  op <- optimize(ttcost, interval = c(1, 1000))
  op_num <- gsub(".*?([0-9].+).*", "\\1", op[1])
  options(digits=5)
  orderq1000 <- c(orderq1000, as.double(op_num))
}

inv1000 <- 9.6 * orderq1000 + 210 * ad1000 / orderq1000 + 60 * ad1000



# 2. Determine the probability distribution that best fits the minimum total cost.
hist(inv1000, main = 'Histogram of Minimum Total Cost', col = '#ffffe6', xlab = 'Minimum Total Cost')

## Null hypothesis H0: the Minimum Total Cost follows the Normal distribution
## Alternative hypothesis H1: the Minimum Total Cost does not follow the Normal distribution
observed <- inv1000
prob.exp <- pnorm(observed, mean(observed), sd(observed))
chisq.test(observed, p = prob.exp, rescale.p = TRUE)
## p-value = 1, so there isn't sufficient evidence to reject H0. 



# 3. Determine the probability distribution that best fits the order quantity.
hist(orderq1000, main = 'Histogram of Order Quantity', col = '#ffffe6', xlab = 'Order Quantity')

## Null hypothesis H0: the Order Quantity follows the Normal distribution
## Alternative hypothesis H1: the Order Quantity does not follow the Normal distribution
observed <- orderq1000
prob.exp <- pnorm(observed, mean(observed), sd(observed))
chisq.test(observed, p = prob.exp, rescale.p = TRUE)
## p-value = 1, so there isn't sufficient evidence to reject H0. 



# 4. Determine a probability distribution that best fits the annual number of orders.
ordern1000 <- ad1000 / orderq1000
hist(ordern1000, main = 'Histogram of Annual Number of Orders', col = '#ffffe6', xlab = 'Annual Number of Orders')

## Null hypothesis H0: the Order Quantity follows the Normal distribution
## Alternative hypothesis H1: the Order Quantity does not follow the Normal distribution
observed <- ordern1000
prob.exp <- pnorm(observed, mean(observed), sd(observed))
chisq.test(observed, p = prob.exp, rescale.p = TRUE)
## p-value = 1, so there isn't sufficient evidence to reject H0. 


