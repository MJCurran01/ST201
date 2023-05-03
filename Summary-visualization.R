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
              , "leaps" #for stepwise selection
              , "boot" #for cross validation
              , "MASS" # For stepwise selection
              , "ggrepel"   # For labels in scatter plots
              , "dplyr" #for groupby functions
              , "modelsummary"#for correlation matrices
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

#replace missing values with 'NA' string
coviddata[is.na(coviddata)] <- 'NA'

#save columns as factor variables
for (i in 1:ncol(coviddata)){
  if ((colnames(coviddata)[i] %in% c('Sex', 'Education', 'IncomeContinuity', 'HealthStatus', 'Unemployed', 'Student',
                                ''))){
    coviddata[, colnames(coviddata)[i]] <- as.factor(coviddata[, colnames(coviddata)[i]])
  }
}

for (i in 1:16){
  colnames(coviddata)[i+7] <- paste('PD', toString(i), sep='')
}

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

corr <- cor(subset(coviddata, select=-c(Sex, Education, IncomeContinuity, HealthStatus, Unemployed, Student)))
colnames(corr) <- colnames(subset(coviddata, select=-c(Sex, Education, IncomeContinuity, HealthStatus, Unemployed, Student)))
rownames(corr) <- colnames(subset(coviddata, select=-c(Sex, Education, IncomeContinuity, HealthStatus, Unemployed, Student)))

stargazer(corr, summary=FALSE, title='correlation matrix', digits=2, out='correlations.html')

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
  geom_smooth(method=lm, color='darkred', se=FALSE) +
  geom_point(color = "darkgreen"
             , fill = "lightblue"
             , size = 3
             , shape = 21         
             , alpha = 1         
  ) +
  labs(title = ""
       , subtitle = ""
       , x = "covid risk"
       , y = "mean anxiety score"
  ) +
  theme_bw()

agg_support <- data.frame(summarize(group_by(coviddata, Social_support), mean_Gad_score=mean(Gad_score), .groups='drop'))

#Covidrisk vs. axiety score
ggplot(agg_support, aes(x = Social_support
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
       , x = "Social support"
       , y = "mean anxiety score"
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
       , x = "Age"
       , y = "mean anxiety score"
  ) +
  theme_bw()

coviddata$PD1 <- as.factor(coviddata$PD1)

ggplot(coviddata, aes(x = PD1
                , y = Gad_score
                , fill = PD1
)
) +
  geom_boxplot() +
  theme_bw() +
  labs(title = ""
       , subtitle = ""
       , x = "Difficulty"
       , y = "Gad_score"
  ) 

coviddata$PD2 <- as.factor(coviddata$PD2)

ggplot(coviddata, aes(x = PD2
                      , y = Gad_score
                      , fill = PD2
)
) +
  geom_boxplot() +
  theme_bw() +
  labs(title = ""
       , subtitle = ""
       , x = "Difficulty"
       , y = "Gad_score"
  ) 

#-----------------------Inference model building----------------------
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

covid.train <- data.frame(model.matrix(Gad_score ~ ., coviddata)[,-1])
names(covid.train)[names(covid.train) == 'Sex1'] <- 'Female'
covid.train$Gad_score=coviddata$Gad_score

#covid.train <- coviddata[split,]
#covid.test <- coviddata[!split,]

# try OLS first and obtain summary
covid.best.OLS <- lm(Gad_score~., covid.train)
summary(covid.best.OLS)

# try step wise selection
covid.forward <- regsubsets(Gad_score~., covid.train, 
                            nvmax=NULL, method="forward"
)
covid.backward <- regsubsets(Gad_score~., covid.train, 
                             nvmax=NULL, method="backward"
)

get_formula <- function(nfeatures, reg, outcome){
  vars <- summary(reg)$which[nfeatures,-1]
  predictors <- names(which(vars==TRUE))
  predictors <- paste(predictors, collapse = " + ")
  formula <- as.formula(paste0(outcome, " ~ ", predictors))
  return(formula)
}

#set up 
covid.best.forward <- lm(get_formula(which.min(summary(covid.forward)$bic), covid.forward, 'Gad_score'), data=covid.train)
covid.best.backward <- lm(get_formula(which.min(summary(covid.backward)$bic), covid.backward, 'Gad_score'), data=covid.train)

forward.bic <- data.frame(n_features=c(1:30), bic=summary(covid.forward)$bic, cp=summary(covid.forward)$cp, adjr2=summary(covid.forward)$adjr2)
ggplot(forward.bic, aes(n_features)
) +
  geom_line(aes(y=bic), color = "red"
             , size = 1
             , alpha = 1         
  ) +
  geom_point(aes(y=bic), color = "maroon",
             size=2,
             alpha=1) +
  geom_line(aes(y=adjr2*800-400), color = "blue"
            , size = 1
            , alpha = 1         
  ) +
  geom_point(aes(y=adjr2*800-400), color = "darkblue",
             size=2,
             alpha=1,
             shape=17) +
  geom_vline(xintercept = which.min(summary(covid.forward)$bic), color='red', alpha=0.8, linetype='dotted', size=1.3) +
  geom_vline(xintercept = which.max(summary(covid.forward)$adjr2), color='blue', alpha=0.8, linetype='dotted', size=1.3) +
  labs(title = ""
       , subtitle = ""
       , x = "Number of features (n)"
       , y = "Model BIC"
  ) +
  scale_colour_manual("", 
                      breaks = c("AdjR2", "BIC"),
                      values = c("AdjR2"="blue", "BIC"="red")) +
  theme_bw()

backward.bic <- data.frame(n_features=c(1:30), bic=summary(covid.backward)$bic, cp=summary(covid.backward)$cp, adjr2=summary(covid.backward)$adjr2)
ggplot(backward.bic, aes(n_features)
) +
  geom_line(aes(y=bic), color = "red"
            , size = 1
            , alpha = 1         
  ) +
  geom_point(aes(y=bic), color = "maroon",
             size=2,
             alpha=1) +
  geom_line(aes(y=adjr2*800-400), color = "blue"
            , size = 1
            , alpha = 1         
  ) +
  geom_point(aes(y=adjr2*800-400), color = "darkblue",
             size=2,
             alpha=1,
             shape=17) +
  geom_vline(xintercept = which.min(summary(covid.backward)$bic), color='red', alpha=0.8, linetype='dotted', size=1.3) +
  geom_vline(xintercept = which.max(summary(covid.backward)$adjr2), color='blue', alpha=0.8, linetype='dotted', size=1.3) +
  labs(title = ""
       , subtitle = ""
       , x = "Number of features (n)"
       , y = "Model BIC"
  ) +
  scale_colour_manual("", 
                      breaks = c("AdjR2", "BIC"),
                      values = c("AdjR2"="blue", "BIC"="red")) +
  theme_bw()

covid.best.OLS.int <- lm(Gad_score~.+Age:(.), data=covid.train)
covid.forward.int <- regsubsets(Gad_score~.+Age:(.), covid.train, 
                                nvmax=NULL, method="forward"
)
covid.backward.int <- regsubsets(Gad_score~.+Age:(.), covid.train, 
                                 nvmax=NULL, method="backward"
)
covid.best.forward.int <- lm(get_formula(which.min(summary(covid.forward.int)$bic), covid.forward.int, 'Gad_score'), data=covid.train)
covid.best.backward.int <- lm(get_formula(which.min(summary(covid.backward.int)$bic), covid.backward.int, 'Gad_score'), data=covid.train)

covid.train$Pandemic_Difficulties_11_sq=covid.train$Pandemic_Difficulties_11^2
covid.train$Pandemic_Difficulties_10_sq=covid.train$Pandemic_Difficulties_10^2

covid.best.forward.int_q <- lm(Gad_score ~ Female + Age + Pandemic_Difficulties_2 + Pandemic_Difficulties_3 + 
                                 Pandemic_Difficulties_5 + Pandemic_Difficulties_7 + 
                                 Pandemic_Difficulties_13 + Pandemic_Difficulties_14 + Pandemic_Difficulties_10 +
                                 Pandemic_Difficulties_10_sq +
                                 Pandemic_Difficulties_11+ Pandemic_Difficulties_11_sq +
                                 Covid_risk + Age:Pandemic_Difficulties_9 + Age:Pandemic_Difficulties_11 + 
                                 Age:Pandemic_Difficulties_13, data=covid.train)

stargazer(covid.best.forward, covid.best.forward.int, covid.best.backward.int, covid.best.forward.int_q,
          type='html', 
          out='model.html',
          single.row = TRUE,
          column.labels = c("Stepwise", "Forward + interactions", 'Backward + interactions', '(2) + polynomials'),
          dep.var.labels.include = FALSE,
          dep.var.caption  = "",
          se = NULL)

stargazer(covid.best.OLS, covid.best.forward, covid.best.forward.int, covid.best.backward.int, 
          type='html', 
          out='model_full.html',
          single.row = TRUE,
          column.labels = c("OLS", "Stepwise", "Forward + interactions", 'Backward + interactions', '(2) + polynomials'),
          dep.var.labels.include = FALSE,
          dep.var.caption  = "",
          se = NULL)

#generate CV mse
accuracy <- data.frame(model = c('OLS', 'forward', 'OLS+interactions', 'interactions+forward', 'interactions+forward+poly*'), '  CV MSE k=5  '=NA, '  CV MSE k=10  '=NA, '  LOOCV MSE  '=NA)

get_cv_mse <- function(reg, k){
  model_formula=formula(reg)
  model.cv <- glm(model_formula, data = covid.train)
  #  Calculate MSE from CV
  set.seed(100)
  mse <- cv.glm(data = covid.train, glmfit = model.cv, K = k)$delta[1]
  # Put MSE back into main dataframe
  return(mse)
}

CV_k = c(5, 10, 1115)
for (i in 1:3){
  ols_mse=get_cv_mse(covid.best.OLS, CV_k[i])
  forward_mse=get_cv_mse(covid.best.forward, CV_k[i])
  ols_int_mse=get_cv_mse(covid.best.OLS.int, CV_k[i])
  forward_int_mse=get_cv_mse(covid.best.forward.int, CV_k[i])
  forward_int_q_mse=get_cv_mse(covid.best.forward.int_q, CV_k[i])
  accuracy[,i+1] <- c(ols_mse, forward_mse, ols_int_mse, forward_int_mse, forward_int_q_mse)
}

stargazer(accuracy, summary = FALSE, type='html', out='accuracy.html', column.sep.width = '10pt')

accuracy_plot=data.frame(model=c(rep('OLS', 3),
                                 rep('forward', 3),
                                 rep('OLS+interactions', 3),
                                 rep('interactions+forward', 3),
                                 rep('interactions+forward+poly*', 3)),
                         CV.fit = rep(c('CV MSE k=5','CV MSE k=10','LOOCV MSE')),
                         MSE = c(as.double(accuracy[1,2:4]), 
                                 as.double(accuracy[2,2:4]), 
                                 as.double(accuracy[3,2:4]), 
                                 as.double(accuracy[4,2:4]), 
                                 as.double(accuracy[5,2:4]))
                         )
accuracy_plot$CV.fit=as.factor(accuracy_plot$CV.fit)
accuracy_plot$model=factor(accuracy_plot$model, levels=c('OLS', 'forward', 'OLS+interactions','interactions+forward', 'interactions+forward+poly*'))
                         
ggplot(accuracy_plot, aes(model, MSE, fill=CV.fit)) +
  geom_bar(stat = "identity", position = 'dodge', color='black', alpha=0.5) + 
  scale_fill_manual(values=c('red', 'blue', 'green')) +
  theme_bw() + 
  coord_cartesian(ylim = c(0.6, 0.69))

