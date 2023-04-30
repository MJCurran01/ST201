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

#Agegroup and salary
coviddata$agegroup <- as.integer(coviddata$Age>=18 & coviddata$Age<=27) + 
  2*as.integer(coviddata$Age>=28 & coviddata$Age<=36) +
  3*as.integer(coviddata$Age>=37 & coviddata$Age<=45) +
  4*as.integer(coviddata$Age>=46 & coviddata$Age<=54) +
  5*as.integer(coviddata$Age>=55 & coviddata$Age<=63) +
  6*as.integer(coviddata$Age>=64)
coviddata$agegroup[coviddata$agegroup==1] <- "18-27"
coviddata$agegroup[coviddata$agegroup==2] <- "28-36"
coviddata$agegroup[coviddata$agegroup==3] <- "37-45"
coviddata$agegroup[coviddata$agegroup==4] <- "46-54"
coviddata$agegroup[coviddata$agegroup==5] <- "55-63"
coviddata$agegroup[coviddata$agegroup==6] <- "64+"
coviddata$agegroup <- as.factor(coviddata$agegroup)

ggplot(coviddata, aes(x = agegroup
                , y = Gad_score
                , fill = agegroup
)
) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  labs(title = "Distribution of anxiety score for different agegroups"
       , subtitle = ""
       , x = "Agegroup"
       , y = "anxiety score"
  ) 

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