# Data Science Project - Due July 7

# Research Questions: 
# 1. What is the prevalence of subjective sleep disturbance and what are its predictors in post liver transplant recipients
# 2. What is the impact and relationship of sleep disturbance with health related quality of life in these patients

# Goals:
# 1. Description of relevant data
# 2. Estimation of the prevalence of sleep disturbance.
# 3. Identify predictors that are associated with the sleep disturbance.
# 4. Identify the relationship between sleep disturbance and quality of life (physical and mental).

data <- read.csv("project_data.csv")
library(dplyr)
library(tidyr)
install.packages("epiR")
library("epiR")
library(ISLR)
library(MASS)

##########################################################################################################################################################
# 2. Estimation of the prevalence of sleep disturbance.

# filter raw data for only the columns we need to look at, filter out missing values and filter for those of interest
newdata <- data %>%
  select(Gender, Age, BMI, Time.from.transplant, 
         Liver.Diagnosis, Recurrence.of.disease, Rejection.graft.dysfunction, 
         Any.fibrosis, Renal.Failure, Depression, Corticoid, Epworth.Sleepiness.Scale, 
         Pittsburgh.Sleep.Quality.Index.Score, Athens.Insomnia.Scale, Berlin.Sleepiness.Scale, 
         SF36.PCS, SF36.MCS) %>%
  na.omit(newdata) %>%
  mutate(Epworth.Sleepiness.Scale = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0)) %>%
  mutate(Pittsburgh.Sleep.Quality.Index.Score= ifelse(Pittsburgh.Sleep.Quality.Index.Score > 5, 1, 0)) %>%
  mutate(Athens.Insomnia.Scale = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

# prevalence estimations for epworth.sleepiness.scale
ep.prev <- as.matrix(cbind(sum(newdata$Epworth.Sleepiness.Scale == 1), nrow(newdata)))
epi.conf(ep.prev, ctype = "prevalence", method = "exact", design = 1, conf.level = 0.95)

# prevalence estimations for Pittsburgh.Sleep.Quality
pitt.prev <- as.matrix(cbind(sum(newdata$Pittsburgh.Sleep.Quality.Index.Score == 1), nrow(newdata)))
epi.conf(pitt.prev, ctype = "prevalence", method = "exact", design = 1, conf.level = 0.95)

# prevalence estimations for Athens Insomnia Scale
athens.prev <- as.matrix(cbind(sum(newdata$Athens.Insomnia.Scale == 1), nrow(newdata)))
epi.conf(athens.prev, ctype = "prevalence", method = "exact", design = 1, conf.level = 0.95)

# prevalence estimations for berlin.sleepiness.scale
berlin.prev <- as.matrix(cbind(sum(newdata$Berlin.Sleepiness.Scale == 1), nrow(newdata)))
epi.conf(berlin.prev, ctype = "prevalence", method = "exact", design = 1, conf.level = 0.95)

##########################################################################################################################################################
# 3. Identify predictors that are associated with the sleep disturbance.

# converting relevant columns to factors
newdata$Gender <- as.factor(newdata$Gender)
newdata$Liver.Diagnosis <- as.factor(newdata$Liver.Diagnosis)
newdata$Recurrence.of.disease <- as.factor(newdata$Recurrence.of.disease)
newdata$Rejection.graft.dysfunction <- as.factor(newdata$Rejection.graft.dysfunction)
newdata$Any.fibrosis <- as.factor(newdata$Any.fibrosis)
newdata$Renal.Failure <- as.factor(newdata$Renal.Failure)
newdata$Depression <- as.factor(newdata$Depression)
newdata$Corticoid <- as.factor(newdata$Corticoid)

newdata$Berlin.Sleepiness.Scale <- as.factor(newdata$Berlin.Sleepiness.Scale)
newdata$Pittsburgh.Sleep.Quality.Index.Score <- as.factor(newdata$Pittsburgh.Sleep.Quality.Index.Score)
newdata$Epworth.Sleepiness.Scale <- as.factor(newdata$Epworth.Sleepiness.Scale)
newdata$Athens.Insomnia.Scale <- as.factor(newdata$Athens.Insomnia.Scale)

# conducting logistic regression for each of the 4 sleep tests using the step back method for variable selection
# selecting the best model for each sleep scale by the smallest AIC value
######### in stepAIC function, we used direction="both" it calcs the AIC values in diff variations of the model after taking out and removing the predictors
model1 <- glm(Epworth.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + 
                Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
                Any.fibrosis + Renal.Failure + Depression + Corticoid, data = newdata, family = binomial)
model1.step.back <- stepAIC(model1, direction="both")
summary(model1.step.back)

model2 <- glm(Pittsburgh.Sleep.Quality.Index.Score ~ Gender + Age + BMI + Time.from.transplant + 
                Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
                Any.fibrosis + Renal.Failure + Depression + Corticoid, data = newdata, family = binomial)
model2.step.back <- stepAIC(model2, direction="both")
summary(model2.step.back)

model3 <- glm(Athens.Insomnia.Scale ~ Gender + Age + BMI + Time.from.transplant + 
                Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
                Any.fibrosis + Renal.Failure + Depression + Corticoid, data = newdata, family = binomial)
model3.step.back <- stepAIC(model3, direction="both")
summary(model3.step.back)

model4 <- glm(Berlin.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + 
                Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
                Any.fibrosis + Renal.Failure + Depression + Corticoid, data = newdata, family = binomial)
model4.step.back <- stepAIC(model4, direction="both")
summary(model4.step.back)

# gives odds ratios, above 1 means they are risk factors, below 1 means protective factor
  exp(model1.step.back$coefficients) 
  # Corticoid1 
  
  exp(model2.step.back$coefficients)
  # Gender2, Depression1, Corticoid1 
  
  exp(model3.step.back$coefficients) 
  # Liver.Diagnosis2, Liver.Diagnosis5, Depression1, Corticoid1 
  
  exp(model4.step.back$coefficients) 
  # BMI

##########################################################################################################################################################
# 4
plot(data$Epworth.Sleepiness.Scale, data$SF36.PCS)
summary(plot(data$Epworth.Sleepiness.Scale, data$SF36.PCS))
cor.test(data$Epworth.Sleepiness.Scale, data$SF36.PCS)
lm(Epworth.Sleepiness.Scale~SF36.PCS, data = newdata2)

newdata2 <- data %>%
  select(Gender, Age, BMI, Time.from.transplant, 
         Liver.Diagnosis, Recurrence.of.disease, Rejection.graft.dysfunction, 
         Any.fibrosis, Renal.Failure, Depression, Corticoid, Epworth.Sleepiness.Scale, 
         Pittsburgh.Sleep.Quality.Index.Score, Athens.Insomnia.Scale, Berlin.Sleepiness.Scale, 
         SF36.PCS, SF36.MCS) %>%
  na.omit(newdata2)

# regression for physical health
lm(Epworth.Sleepiness.Scale~SF36.PCS, data = newdata2)
lm(Pittsburgh.Sleep.Quality.Index.Score~SF36.PCS, data = newdata2)
lm(Athens.Insomnia.Scale~SF36.PCS, data = newdata2)

# regression for mental health
lm(Epworth.Sleepiness.Scale~SF36.MCS, data = newdata2)
lm(Pittsburgh.Sleep.Quality.Index.Score~SF36.MCS, data = newdata2)
lm(Athens.Insomnia.Scale~SF36.MCS, data = newdata2)
