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

# conducting logistic regression for each of the 4 sleep tests
model1 <- glm(Epworth.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + 
                Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
                Any.fibrosis + Renal.Failure + Depression + Corticoid, data = newdata, family = binomial)

model2 <- glm(Pittsburgh.Sleep.Quality.Index.Score ~ Gender + Age + BMI + Time.from.transplant + 
                Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
                Any.fibrosis + Renal.Failure + Depression + Corticoid, data = newdata, family = binomial)

model3 <- glm(Athens.Insomnia.Scale ~ Gender + Age + BMI + Time.from.transplant + 
                Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
                Any.fibrosis + Renal.Failure + Depression + Corticoid, data = newdata, family = binomial)

model4 <- glm(Berlin.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant + 
                Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
                Any.fibrosis + Renal.Failure + Depression + Corticoid, data = newdata, family = binomial)


# gives odds ratios, above 1 means they are risk factors, below 1 means protective factor
exp(model1$coefficients) 
# BMI, time from transplant, Recurrence.of.disease1, Rejection.graft.dysfunction1, Depression1, Corticoid1 

exp(model2$coefficients)
# Gender2, BMI, Liver.Diagnosis2, Liver.Diagnosis3, Liver.Diagnosis4, Liver.Diagnosis5, Recurrence.of.disease1, 
# Any.fibrosis1, Renal.Failure1, Depression1, Corticoid1 

exp(model3$coefficients) 
# Gender2, BMI, Liver.Diagnosis2, Liver.Diagnosis3, Liver.Diagnosis4, Liver.Diagnosis5, Recurrence.of.disease1,
# Any.fibrosis1, Renal.Failure1, Depression1, Corticoid1 

exp(model4$coefficients) 
# Age, BMI, Liver.Diagnosis2, Liver.Diagnosis5, Recurrence.of.disease1, Any.fibrosis1, Depression1, Corticoid1 

##########################################################################################################################################################
# 4
plot(data$Epworth.Sleepiness.Scale, data$SF36.PCS)
summary(plot(data$Epworth.Sleepiness.Scale, data$SF36.PCS))
cor.test(data$Epworth.Sleepiness.Scale, data$SF36.PCS)
lm(Epworth.Sleepiness.Scale~SF36.PCS, data = data)
lm(Epworth.Sleepiness.Scale >= 10~SF36.PCS, data = data)

