#### Load data & packages ----

df <- read.csv (file = "processed data.csv", stringsAsFactors = TRUE)
library (dplyr)
library (ggplot2)
library (pROC)

#### Statistic analysis ----
## Analysis the influence of age on psychology state - log linear regression

# CBTS

log_model_lin_CBTS <- glm (high_low_CBTS ~ 1 ,data = df, family = binomial)

log_model_lin_CBTS_age <- glm (high_low_CBTS ~ 1 + Age ,data = df, family = binomial)

summary (log_model_lin_CBTS)

summary (log_model_lin_CBTS_age)

# EPDS

log_model_lin_EPDS <- glm (high_low_EPDS ~ 1 ,data = df, family = binomial)

log_model_lin_EPDS_age <- glm (high_low_EPDS ~ 1 + Age ,data = df, family = binomial)

summary (log_model_lin_EPDS)

summary (log_model_lin_EPDS_age)

# HADS

log_model_lin_HADS <- glm (high_low_HADS ~ 1 ,data = df, family = binomial)

log_model_lin_HADS_age <- glm (high_low_HADS ~ 1 + Age ,data = df, family = binomial)

summary (log_model_lin_HADS)

summary (log_model_lin_HADS_age)

#Explain data in the word file: "Finale project - explain and answers"

#### Build ROC ----

## CBTS

df <- df|>
  mutate(predict_log_model_lin_CBTS = predict (log_model_lin_CBTS, newdata = df, type = "response"),
         predict_log_model_lin_CBTS_age = predict (log_model_lin_CBTS_age, newdata = df, type = "response"))

ROC_log_model_lin_CBTS <- roc (df$high_low_CBTS, df$predict_log_model_lin_CBTS)
ROC_log_model_lin_CBTS_age <- roc (df$high_low_CBTS, df$predict_log_model_lin_CBTS_age)

#build confusion matrix
threshold <- 0.5
table (predicted_CBTS = (df$predict_log_model_lin_CBTS_age > threshold)*1, Actual = df$high_low_CBTS)

#AUC
auc (ROC_log_model_lin_CBTS)
auc (ROC_log_model_lin_CBTS_age)

# build a plot of the ROCs

plot (ROC_log_model_lin_CBTS, col = "black", main = "all the ROC curves")
plot (ROC_log_model_lin_CBTS_age,add = TRUE, col = "purple")
legend ("bottomright",
        legend = c("Model of intercept", "Model of intercept and age"),
        col = c("black", "purple"),
        lwd = 4)
## EPDS

df <- df|>
  mutate(predict_log_model_lin_EPDS = predict (log_model_lin_EPDS, newdata = df, type = "response"),
         predict_log_model_lin_EPDS_age = predict (log_model_lin_EPDS_age, newdata = df, type = "response"))

ROC_log_model_lin_EPDS <- roc (df$high_low_EPDS, df$predict_log_model_lin_EPDS)
ROC_log_model_lin_EPDS_age <- roc (df$high_low_EPDS, df$predict_log_model_lin_EPDS_age)

#build confusion matrix
threshold <- 0.5
table (predicted_CBTS = (df$predict_log_model_lin_EPDS_age > threshold)*1, Actual = df$high_low_EPDS)

#AUC
auc (ROC_log_model_lin_EPDS)
auc (ROC_log_model_lin_EPDS_age)

# build a plot of the ROCs

plot (ROC_log_model_lin_EPDS, col = "black", main = "all the ROC curves")
plot (ROC_log_model_lin_EPDS_age,add = TRUE, col = "blue")
legend ("bottomright",
        legend = c("Model of intercept", "Model of intercept and age"),
        col = c("black", "blue"),
        lwd = 4)

## HADS

df <- df|>
  mutate(predict_log_model_lin_HADS = predict (log_model_lin_HADS, newdata = df, type = "response"),
         predict_log_model_lin_HADS_age = predict (log_model_lin_HADS_age, newdata = df, type = "response"))

ROC_log_model_lin_HADS <- roc (df$high_low_HADS, df$predict_log_model_lin_HADS)
ROC_log_model_lin_HADS_age <- roc (df$high_low_HADS, df$predict_log_model_lin_HADS_age)

#build confusion matrix
threshold <- 0.5
table (predicted_CBTS = (df$predict_log_model_lin_HADS_age > threshold)*1, Actual = df$high_low_HADS)

#AUC
auc (ROC_log_model_lin_HADS)
auc (ROC_log_model_lin_HADS_age)

# build a plot of the ROCs

plot (ROC_log_model_lin_HADS, col = "black", main = "all the ROC curves")
plot (ROC_log_model_lin_HADS_age,add = TRUE, col = "red")
legend ("bottomright",
        legend = c("Model of intercept", "Model of intercept and age"),
        col = c("black", "red"),
        lwd = 4)

#### End of the project----