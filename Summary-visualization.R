# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear memory
cat("\f")       # Clear the console
options(scipen = 9) # Remove scientific notation for numbers with 9 decimals or less

# Prepare needed libraries
packages <- c("haven" # To import *.dta files
              , "ggplot2" # Best plotting
              , "stargazer" # Nice output tables
              , "car" # For doing F-tests
              , "sandwich" # For robust standard errors
              , "lmtest" # For robust standard errors
              , "leaps"
              , "MASS" # For stepwise selection
              , "ggrepel"   # For labels in scatter plots
              , "dplyr" #for groupby functions
)
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i]
                     , repos = "http://cran.rstudio.com/"
                     , dependencies = TRUE
    )
  }
  library(packages[i], character.only=TRUE)
}
rm(packages)

coviddata <- read.csv('coviddata.csv')
coviddata <- subset(coviddata, select=-1)

coviddata$Sex <- as.factor(coviddata$Sex)
coviddata$Education <- as.factor(coviddata$Education)

#replace missing values with 'NA' string
coviddata[is.na(coviddata)] <- 'NA'

coviddata$IncomeContinuity <- as.factor(coviddata$IncomeContinuity)
coviddata$HealthStatus <- as.factor(coviddata$HealthStatus)
coviddata$Unemployed <- as.factor(coviddata$Unemployed)
coviddata$Student <- as.factor(coviddata$Student)

#create summary statistics table
stargazer(coviddata
          , summary = TRUE   # We want summary only
          , median =  TRUE
          , type = "html" # Type of output - text, HTML or LaTeX
          , title = "Descriptive statistics" #Title of my output
          , digits = 2
          , out = "summary.html" # File name
)

#create table for factor variables
options(na.action = "na.pass")
factors.df <- model.matrix(~ Sex+Education+IncomeContinuity+HealthStatus+Unemployed+Student-1, data = coviddata)
factors.summary <- data.frame(factors.df)
#names(factors.summary) <- colnames()
stargazer(factors.summary,
          summary=TRUE,
          type='html',
          title='Descriptive statistics - Categorical variables',
          digits=2,
          out='summary_categorical.html')

#visualization of important variables
#Histogram of Gad_score
ggplot(coviddata, aes(x=Gad_score))+
  geom_histogram(bins = 50
                 , color = "darkblue"
                 , fill = "lightblue"
  ) +
  # Adjust y-axis ticks
  scale_y_continuous(breaks = seq(from = 0        
                                  , to = 200
                                  , by = 20
  )
  ) +
  # Adjust x-axis ticks
  scale_x_continuous(breaks = seq(from = -2        
                                  , to = 2.8
                                  , by = 0.4
  )
  ) +
  labs(title = "Distribution of Generalized Anxiety level scores"
       , subtitle = ""
       , x = "Gad_score"
       , y = "count"
  ) +
  # Apply black-and-white theme
  theme_bw()

agg_risk <- data.frame(summarize(group_by(coviddata, Covid_risk), mean_Gad_score=mean(Gad_score), .groups='drop'))

#Covidrisk vs. axiety score
ggplot(agg_risk, aes(x = Covid_risk
                      , y = mean_Gad_score
)
) +
  geom_point(color = "darkgreen"
             , fill = "lightblue"
             , size = 3
             , shape = 21         
             , alpha = 1         
  ) +
  labs(title = ""
       , subtitle = ""
       , x = "Mean covid risk"
       , y = "anxiety score"
  ) +
  theme_bw()

agg_age <- data.frame(summarize(group_by(coviddata, Age), mean_Gad_score=mean(Gad_score), .groups='drop'))

#Covidrisk vs. axiety score
ggplot(agg_age, aes(x = Age
                     , y = mean_Gad_score
)
) +
  geom_point(color = "darkgreen"
             , fill = "lightblue"
             , size = 3
             , shape = 21         
             , alpha = 1         
  ) +
  labs(title = ""
       , subtitle = ""
       , x = "Mean covid risk"
       , y = "anxiety score"
  ) +
  theme_bw()

#-----------------------Inference model building)
gc()
rm(list=ls())

coviddata <- read.csv('coviddata.csv')
coviddata <- subset(coviddata, select=-1)

coviddata$Sex <- as.factor(coviddata$Sex)
coviddata$Education <- as.factor(coviddata$Education)

#replace missing values with 'NA' string
coviddata[is.na(coviddata)] <- 'NA'

coviddata$IncomeContinuity <- as.factor(coviddata$IncomeContinuity)
coviddata$HealthStatus <- as.factor(coviddata$HealthStatus)
coviddata$Unemployed <- as.factor(coviddata$Unemployed)
coviddata$Student <- as.factor(coviddata$Student)

#train-test
set.seed(1000)
split <- sample(c(TRUE, FALSE), nrow(coviddata), replace=TRUE, prob=c(0.75,0.25))
covid.train <- coviddata[split,]
covid.test <- coviddata[!split,]

#generate predictions
prediction <- data.frame(Gad_score = covid.test$Gad_score)

accuracy <- data.frame(model = c('OLS', 'forward', 'backward', 'RandomForest', 'OLS-adjusted'), MSE=NA)


# try OLS first and obtain summary
covid.best.OLS <- lm(Gad_score~., covid.train)
summary(covid.best.OLS)

#save results into a data frame
results.ols <- data.frame(feature = names(covid.best.OLS$coefficients)[-1] # -1 to exclude intercept
                          , ols = round(coef(covid.best.OLS)[-1], 5) # -1 to exclude intercept
)

prediction$score.OLS <- predict(covid.best.OLS, newdata = as.data.frame(covid.test))

residualPlots(covid.best.OLS)

#train-test
coviddata$Pandemic_Difficulties_11 <- as.factor(coviddata$Pandemic_Difficulties_11)
covid.train <- coviddata[split,]
covid.test <- coviddata[!split,]

covid.bestres.OLS <- lm(Gad_score~., covid.train)
summary(covid.bestres.OLS)

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

n_features_forward <- cv_best_features(covid.forward)
n_features_backward <- cv_best_features(covid.backward)

#set up 
covid.best.forward <- lm(get_formula(which.min(summary(covid.forward)$bic), covid.forward, 'Gad_score'), data=covid.stepwise)
covid.best.backward <- lm(get_formula(which.min(summary(covid.backward)$bic), 'Gad_score'), data=covid.stepwise)

#Random Forest
rf.fit <- randomForest(Gad_score ~ ., data=covid.train, ntree=1000,
                       keep.forest=TRUE, importance=TRUE)

#generate predictions

covid.stepwise.test <- data.frame(model.matrix(Gad_score ~ ., covid.test)[,-1])
covid.stepwise.test$Gad_score <- covid.test$Gad_score 

prediction$score.forward <- predict(covid.best.forward, newdata = as.data.frame(covid.stepwise.test))
prediction$score.backward <- predict(covid.best.backward, newdata = as.data.frame(covid.stepwise.test))
prediction$score.rf <- predict(rf.fit, newdata = as.data.frame(covid.test))
prediction$score.bestres.OLS <- predict(covid.bestres.OLS, newdata = as.data.frame(covid.test))

#Calculate MSE
accuracy$MSE[accuracy$model == "OLS"] <- mean((prediction$Gad_score - prediction$score.OLS)^2)
accuracy$MSE[accuracy$model == "forward"] <- mean((prediction$Gad_score - prediction$score.forward)^2)
accuracy$MSE[accuracy$model == "backward"] <- mean((prediction$Gad_score - prediction$score.backward)^2)
accuracy$MSE[accuracy$model == "RandomForest"] <- mean((prediction$Gad_score - prediction$score.rf)^2)
accuracy$MSE[accuracy$model == "OLS-adjusted"] <- mean((prediction$Gad_score - prediction$score.bestres.OLS)^2)
accuracy