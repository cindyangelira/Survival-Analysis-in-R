library(tidyverse) #data pre processing, data manipulation
library(survivalmodels) #survival analysis model
library(lubridate) # date type handling
library(magrittr) # create pipeline
library(mlr3)
library(mlr3benchmark) # result analysis
library(mlr3pipelines) # data preprocessing
library(mlr3proba) # benchmark and compare model
library(mlr3tuning) 
library(mlr3extralearners)


set.seed(1)
df <- read_csv("Dialysis.csv")
head(df)

# Pre processing----
df <- df %>% select(-6)
colnames(df)[c(4,5,6)] <- c("age", "begin", "disease")

#  Encoding the  categorical feature----
library(caret)
dmy <- dummyVars(" ~ .", data = df)
df_clean <- data.frame(predict(dmy, newdata = df)) %>% select(-c(1,6))
head(df_clean)

# Create Task ----
task <- TaskSurv$new("patient_id", backend = df_clean, time = "time", event = "event", type = "right")
task

# Create learners----
learners <- lrns(
  paste0("surv.", c("coxtime","deephit", "deepsurv", "loghaz", "pchazard")),
  frac = 0.3, activation = "relu",
  dropout = 0.1, 
  early_stopping = TRUE, 
  epochs = 10,
  batch_size = 32L
  )
learners <- c(learners, lrns(c("surv.kaplan", "surv.coxph")))
learners

# Create pipeline----
pipeline <- function(learner){
  po("scale") %>>% po("learner", learner)
}
learners <- lapply(learners, pipeline)

# Benchamrk----
resample <- rsmp("cv", folds = 5)
learners <- c(learners, lrns(c("surv.kaplan", "surv.coxph")))
models <- benchmark_grid(task, learners, resample)
bnchmrk <- benchmark(models)

# Aggregate result
measures <- msrs(c("surv.cindex", "surv.graf"))
bnchmrk$aggregate(measures)
bm <- fortify(bnchmrk)
aggr <- bnchmrk$aggregate(measures)

coxtime = aggr$resample_result[[1]]
coxtime$prediction(predict_sets = "test")
# Plot all survival curves----
plot(coxtime$prediction(predict_sets = "test")$distr, ind = 1, fun = "survival")

# Plot first two survival curves----
plot(coxtime$prediction(predict_sets = "test")$distr[1], "survival", main = "First 2 Survival Curves")
lines(coxtime$prediction(predict_sets = "test")$distr[2], "survival", col = 2)



coxph = aggr$resample_result[[7]]
coxph$prediction(predict_sets = "test")
plot(coxph$prediction(predict_sets = "test")$distr, ind = 1, fun = "survival")
