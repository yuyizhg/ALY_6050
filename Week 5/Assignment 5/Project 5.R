library(lpSolve)

# 1-5. Linear Programming
# Set coefficients of the objective function
f.obj <- c(499.99-330, 729.99-370, 699.99-410, 269.99-127)

# Set matrix corresponding to coefficients of constraints by rows
f.con <- matrix(c(330,370,410,127,
                  25,40,25,1.25,
                  0.7,0.7,-0.3,-0.3,
                  0,0,1,-2),nrow = 4, byrow = TRUE)

# Set unequality signs
f.dir <- c("<=",
           "<=",
           ">=",
           ">=")

# Set right hand side coefficients
f.rhs <- c(170000,
           82*30*5,
           0,
           0)

# Final value (z)
lp("max", f.obj, f.con, f.dir, f.rhs)

# Variables final values
lp("max", f.obj, f.con, f.dir, f.rhs)$solution

# Sensitivities
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens = TRUE)$sens.coef.form
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens = TRUE)$sens.coef.to



# 6. Increasing the budget
last_result <- 0
budget <- 170000
while (budget < 200000) {
  f.obj <- c(169.99, 359.99, 289.99, 142.99)
  f.con <- matrix(c(330,370,410,127,
                    25,40,25,1.25,
                    0.7,0.7,-0.3,-0.3,
                    0,0,1,-2),nrow = 4, byrow = TRUE)
  f.dir <- c("<=",
             "<=",
             ">=",
             ">=")
  f.rhs <- c(budget,
             12300,
             0,
             0)
  result <- lp("max", f.obj, f.con, f.dir, f.rhs)$objval
  if (last_result == result) {
    break
  }
  last_result <- result
  budget = budget + 100
}



# 7. Increasing the space
last_result <- 0
space <- 12300
while (space < 20000) {
  f.obj <- c(169.99, 359.99, 289.99, 142.99)
  f.con <- matrix(c(330,370,410,127,
                    25,40,25,1.25,
                    0.7,0.7,-0.3,-0.3,
                    0,0,1,-2),nrow = 4, byrow = TRUE)
  f.dir <- c("<=",
             "<=",
             ">=",
             ">=")
  f.rhs <- c(170000,
             space,
             0,
             0)
  result <- lp("max", f.obj, f.con, f.dir, f.rhs)$objval
  if (last_result == result) {
    break
  }
  last_result <- result
  space = space + 150
}


