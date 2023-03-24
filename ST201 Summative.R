######## INFO ########

# PROJECT
# Title: ST201 Summative Group Project
# Group Number: 12

# R Script
# Created: 24 Mar 2023
# Inputs: coviddata.csv
# Outputs:

######## SETUP ########

rm(list = ls()) # clear workspace
setwd("") # setting working directory

######## PACKAGES ########

# Check system and installs packages user doesn't have, load needed packages

need <- c("dplyr") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages


# Attaching data
data <- read.csv("coviddata.csv")
data <- na.omit(data)

# Create the design matrix
X <- as.matrix(data[, c("Sex", "Age", "Education", "IncomeContinuity", "HealthStatus", "Unemployed", "Student",
                        "Pandemic_Difficulties_1", "Pandemic_Difficulties_2", "Pandemic_Difficulties_3",
                        "Pandemic_Difficulties_4", "Pandemic_Difficulties_5", "Pandemic_Difficulties_6",
                        "Pandemic_Difficulties_7", "Pandemic_Difficulties_8", "Pandemic_Difficulties_9",
                        "Pandemic_Difficulties_10", "Pandemic_Difficulties_11", "Pandemic_Difficulties_12",
                        "Pandemic_Difficulties_13", "Pandemic_Difficulties_14", "Pandemic_Difficulties_15",
                        "Pandemic_Difficulties_16", "Covid_risk", "Social_support")])

# Create the outcome vector
Y <- data$Gad_score

# Normalize the data
X <- scale(X)
Y <- scale(Y)

# Set the learning rate and number of iterations
alpha <- 0.01
iterations <- 1000

# Initialize the coefficients
b <- matrix(0, ncol = ncol(X), nrow = 1)

# Create a function to calculate the gradient
gradient <- function(b, X, Y) {
  n <- length(Y)
  grad <- t((t(X) %*% ((X %*% t(b)) - Y)) / n)
  return(grad)
}

# Use gradient descent to optimize the coefficients
for (i in 1:iterations) {
  b <- b - (alpha * gradient(b, X, Y))
}

# Print the coefficients
print(b)

# Predict the outcome variable
predictions <- X %*% t(b)

# Print the predicted values
print(predictions)

# Calculate the mean squared error
MSE <- mean((Y - predictions)^2)

# Print the mean squared error
print(paste("Mean Squared Error:", MSE))
