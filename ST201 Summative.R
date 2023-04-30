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
gc()            # Clear memory
cat("\f")       # Clear the console
options(scipen = 9) # Remove scientific notation for numbers with 9 decimals or less

######## PACKAGES ########
# Prepare needed libraries
packages <- c("haven" # To import *.dta files
              , "stargazer" # Nice output tables
              , "MASS" # For stepwise selection
              , "ISLR"
              , "glmnet"
              , "boot"
              , "leaps"
              , "randomForest"
)
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i]
                     , repos = "http://cran.rstudio.com/"
                     , dependencies = TRUE
    )
  }
  library(packages[i], character.only = TRUE)
}
rm(packages)

install.packages("installr")

library(installr)

updateR()

# Attaching data
data <- read.csv("coviddata.csv")
data <- na.omit(data)[,-1] #omit NAs and the index column


for (i in 1:ncol(data)){
  if (!(colnames(data)[i] %in% c('Age', 'Social_support', 'Covid_risk', 'Gad_score'))){
    data[, colnames(data)[i]] <- as.factor(data[, colnames(data)[i]])
  }
}

#train-test
set.seed(1000)
split <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.75,0.25))
covid.train <- data[split,]
covid.test <- data[!split,]

# try OLS first and obtain summary
covid.best.OLS <- lm(Gad_score~., covid.train)
summary(covid.best.OLS)

#save results into a data frame
results.ols <- data.frame(feature = names(covid.best.OLS$coefficients)[-1] # -1 to exclude intercept
                          , ols = round(coef(covid.best.OLS)[-1], 5) # -1 to exclude intercept
)

covid.stepwise <- data.frame(model.matrix(Gad_score ~ ., covid.train)[,-1])
covid.stepwise$Gad_score <- covid.train$Gad_score

# try step wise selection
covid.forward <- regsubsets(Gad_score~., covid.stepwise, 
                                  nvmax=NULL, method="forward"
)
covid.backward <- regsubsets(Gad_score~., covid.stepwise, 
                                 nvmax=NULL, method="backward"
)

get_formula <- function(nfeatures, reg, outcome){
  vars <- summary(reg)$which[nfeatures,-1]
  predictors <- names(which(vars==TRUE))
  predictors <- paste(predictors, collapse = " + ")
  formula <- as.formula(paste0(outcome, " ~ ", predictors))
  return(formula)
}

cv_best_features <- function(reg){
  cv.fit <- data.frame(features = seq.int(from = 1, to = ncol(summary(reg)$which)-1, by = 1),
                       mse = NA
  )
  for (i in 1:(ncol(summary(reg)$which)-1)) { 
    # The inner loop cycles through number of features
    formula.cv <- get_formula(i, reg, 'Gad_score')
    # Estimate the model
    model.cv <- glm(as.formula(formula.cv), data = covid.stepwise)
    #  Calculate MSE from CV
    set.seed(100)
    mse <- cv.glm(data = covid.stepwise, glmfit = model.cv, K = 5)$delta[1]
    # Put MSE back into main dataframe
    cv.fit$mse[cv.fit$features == i] <- mse
  }
  return(cv.fit[which.min(cv.fit$mse),]$features)
}

cv_best_features(covid.forward)
#19
cv_best_features(covid.backward)
#18

#set up 
covid.best.forward <- lm(get_formula(25, covid.forward, 'Gad_score'), data=covid.stepwise)
covid.best.backward <- lm(get_formula(22, covid.backward, 'Gad_score'), data=covid.stepwise)

#Random Forest
rf.fit <- randomForest(Gad_score ~ ., data=covid.train, ntree=1000,
                       keep.forest=TRUE, importance=TRUE)

#generate predictions
prediction <- data.frame(Gad_score = covid.test$Gad_score)

covid.stepwise.test <- data.frame(model.matrix(Gad_score ~ ., covid.test)[,-1])
covid.stepwise.test$Gad_score <- covid.test$Gad_score 

prediction$score.OLS <- predict(covid.best.OLS, newdata = as.data.frame(covid.test))
prediction$score.forward <- predict(covid.best.forward, newdata = as.data.frame(covid.stepwise.test))
prediction$score.backward <- predict(covid.best.backward, newdata = as.data.frame(covid.stepwise.test))
prediction$score.rf <- predict(rf.fit, newdata = as.data.frame(covid.test))

#Calculate MSE
accuracy <- data.frame(model = c('OLS', 'forward', 'backward', 'RandomForest'), MSE=NA)
accuracy$MSE[accuracy$model == "OLS"] <- mean((prediction$Gad_score - prediction$score.OLS)^2)
accuracy$MSE[accuracy$model == "forward"] <- mean((prediction$Gad_score - prediction$score.forward)^2)
accuracy$MSE[accuracy$model == "backward"] <- mean((prediction$Gad_score - prediction$score.backward)^2)
accuracy$MSE[accuracy$model == "RandomForest"] <- mean((prediction$Gad_score - prediction$score.rf)^2)
accuracy

#-----------------------------------------------------------------------------------------

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
