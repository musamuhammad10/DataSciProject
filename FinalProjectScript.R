
dataset <- project_data
summary(dataset)
head(dataset)

install.packages("dplyr")
library("dplyr")
install.packages("epiR")
library("epiR")

rawdata <- dataset %>%
  select(1:3, 9, 16, 18:20, 24, 42,46, 58, 70, 75, 77, 85:87)
rawdata

names(which(colSums(is.na(rawdata))>0))

data <- na.omit(rawdata)
data

##########################################################################################################################################################

# prevelance estimations for epworth.sleepiness.scale

ep <- ifelse(data$Epworth.Sleepiness.Scale > 10, 1, 0)
epsum <- sum(ep)
nrow(data)
#ep.prev.est <- epsum/nrow(data)
ep.prev <- as.matrix(cbind(epsum, nrow(data)))
epi.conf(ep.prev, ctype = "prevalence", method = "exact", design = 1, conf.level = 0.95)

# prevelance estimations for Pittsburgh.Sleep.Quality

pitt <- ifelse(data$Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0)
pittsum <- sum(pitt)
nrow(data)
#pitt.prev.est <- pittsum/nrow(data)
pitt.prev <- as.matrix(cbind(pittsum, nrow(data)))
epi.conf(pitt.prev, ctype = "prevalence", method = "exact", design = 1, conf.level = 0.95)

# prevelance estimations for Athens Insomnia Scale

athens <- ifelse(data$Athens.Insomnia.Scale > 5, 1, 0)
athenssum <- sum(athens)
nrow(data)
#athens.prev.est <- athenssum/nrow(data)
athens.prev <- as.matrix(cbind(athenssum, nrow(data)))
epi.conf(athens.prev, ctype = "prevalence", method = "exact", design = 1, conf.level = 0.95)

# prevelance estimations for berlin.sleepiness.scale

berlin <- ifelse(data$Berlin.Sleepiness.Scale == 1, 1, 0)
berlinsum <- sum(berlin)
nrow(data)
#berlin.prev.est <- berlinsum/nrow(data)
berlin.prev <- as.matrix(cbind(berlinsum, nrow(data)))
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

