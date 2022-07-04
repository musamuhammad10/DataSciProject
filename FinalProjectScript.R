
dataset <- project_data
summary(dataset)
head(dataset)

install.packages("dplyr")
library("dplyr")
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


# 3
data$Renal.Failure <- as.factor(data$Renal.Failure)
data$Recurrence.of.disease <- as.factor(data$Recurrence.of.disease)
data$Rejection.graft.dysfunction <- as.factor(data$Rejection.graft.dysfunction)
data$Any.fibrosis <- as.factor(data$Any.fibrosis)
data$Depression <- as.factor(data$Depression)
data$Liver.Diagnosis <- as.factor(data$Liver.Diagnosis)
data$Corticoid <- as.factor(data$Corticoid)
data$Gender <- as.factor(data$Gender)


epworth <- ifelse(data$Epworth.Sleepiness.Scale > 10, 1, 0)
model1 <- glm(epworth ~ Gender + Age + BMI + Time.from.transplant + 
              Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
              Any.fibrosis + Renal.Failure + Depression + Corticoid, data = data, family = binomial)
model1
summary(model1)

# is it 4 or 5????
pittsburgh <- ifelse(data$Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0)
model2 <- glm(pittsburgh ~ Gender + Age + BMI + Time.from.transplant + 
                Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
                Any.fibrosis + Renal.Failure + Depression + Corticoid, data = data, family = binomial)
model2

athensscale <- ifelse(data$Athens.Insomnia.Scale > 5, 1, 0)
model3 <- glm(athensscale ~ Gender + Age + BMI + Time.from.transplant + 
                Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
                Any.fibrosis + Renal.Failure + Depression + Corticoid, data = data, family = binomial)
model3

berlinscale <- ifelse(data$Berlin.Sleepiness.Scale == 1, 1, 0)
model4 <- glm(berlinscale ~ Gender + Age + BMI + Time.from.transplant + 
                Liver.Diagnosis + Recurrence.of.disease + Rejection.graft.dysfunction +
                Any.fibrosis + Renal.Failure + Depression + Corticoid, data = data, family = binomial)
model4

# 4
plot(data$Epworth.Sleepiness.Scale, data$SF36.PCS)
summary(plot(data$Epworth.Sleepiness.Scale, data$SF36.PCS))
cor.test(data$Epworth.Sleepiness.Scale, data$SF36.PCS)
lm(Epworth.Sleepiness.Scale~SF36.PCS, data = data)
lm(Epworth.Sleepiness.Scale >= 10~SF36.PCS, data = data)

