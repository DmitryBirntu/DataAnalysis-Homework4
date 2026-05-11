#--------------------start-------------------------------
# Get current working directory
getwd()
#----------------read dataset--------------------------
install.packages("wPerm")
library(wPerm)
data <- read.csv("data_for_analysis.csv")
data$outcome <- as.factor(data$outcome)
summary(data)
# testing for normality of distribution
shapiro.test(data$lipids1)
shapiro.test(data$lipids2)

hist(data$lipids1)  
qqnorm(data$lipids1)

# Spearman's correlation test

spearman_result<-cor.test(data$lipids1, data$lipids2, method="spearman")

print(spearman_result)


# data.frame for result
results <- data.frame(
  variable = character(),
  spearman_corr = numeric(),
  s_p_value = numeric(),
  stringsAsFactors = FALSE
)

# variables for analysis
target_vars <- c("lipids2", "lipids3", "lipids4")

# main 
for (var in target_vars) {
  # spearman
  perm_spearman <- perm.relation(
    x = data$lipids1, 
    y = data[[var]],
    method = "spearman",
    R = 10000
  )
  
  # add result
  results <- rbind(results, data.frame(
    variable = var,
    spearman_corr = perm_spearman$Observed,
    s_p_value =  perm_spearman$p.value
    ))
}


# output result
print(results)

#------visualization of significant results of correlation analysis---------

data<-data[order(data$lipids2),]

plot(data$lipids2, data$lipids1)

lines(data$lipids2, data$lipids1, col = "blue")

abline(lm(data$lipids1 ~ data$lipids2), col="red")




#_____________regression analysis________________ 

df=data
df<-df[order(df$lipids1),]


#linear regression

model_linear <- lm(lipids1 ~ lipids2, data=df)
summary(model_linear)


#second degree polynomal

model_2 <- lm(lipids1 ~ poly(lipids2, 2), data=df)
summary(model_2)

#third degree polynomal

model_3 <- lm(lipids1 ~ poly(lipids2, 3), data=df)

summary(model_3)
#exponential dependence

model_exp <- lm(log(lipids1) ~ lipids2, data=df)
summary(model_exp)
# log dependence

model_log <- lm(exp(lipids1) ~ lipids2, data=df)
summary(model_log)
#comparison of models
#table of result

rezult<-data.frame(model=c("model_linear", "model_2", "model_3", "model_exp", "model_log"), BIC_value=c(BIC(model_linear), BIC(model_2), BIC(model_3), BIC(model_exp), BIC(model_log)))

rezult<-rezult[order(rezult$BIC_value),]

rezult


# __________building graphs______________
#         linear regression graphs

plot(df$lipids2, df$lipids1)
lines(df$lipids2, fitted(model_linear), col="blue")

# Logistic regression
# Dependent variable: outcome 
sum(is.na(data$outcome))  
data <- data[!is.na(data$outcome), ]
# Example: Predicting outcome based on lipids1 and lipids2

# Simple model with one predictor
model_logit_1 <- glm(outcome ~ lipids1, data = data, family = binomial)
summary(model_logit_1)

# Multi-predictor model
model_logit_2 <- glm(outcome ~ lipids1 + lipids2, data = data, family = binomial)
summary(model_logit_2)

# Model with all variables lipids 
model_logit_all <- glm(outcome ~ lipids1 + lipids2 + lipids3 + lipids4, 
                       data = data, family = binomial)
summary(model_logit_all)


# Predicting probabilities for new data (example)
data$pred_prob <- predict(model_logit_2, type = "response")

# Classification by threshold 0.5
data$pred_class <- ifelse(data$pred_prob > 0.5, 1, 0)

# confusion matrix
table(Actual = data$outcome, Predicted = data$pred_class)

# Model Quality Assessment: ROC Curve and AUC 
if (!require(pROC)) install.packages("pROC")
library(pROC)
roc_curve <- roc(data$outcome, data$pred_prob)
plot(roc_curve, main = "ROC-Curve")
auc(roc_curve)

# Stepwise variable selection (AIC)
step_model <- step(model_logit_all, direction = "both")
summary(step_model)
#Coefficient interpretation 
exp(cbind(OR = coef(model_logit_2), confint(model_logit_2)))


#-------------------------------HOMEWORK-------------------------------------
df <- read.csv("data_for_analysis.csv")
df$outcome <- as.factor(df$outcome)

perm_spearman_test <- function(x, y, R = 10000) {
  complete_idx <- complete.cases(x, y)
  x <- x[complete_idx]
  y <- y[complete_idx]
  
  obs <- cor(x, y, method = "spearman")
  
  perm_stats <- replicate(R, {
    cor(x, sample(y), method = "spearman")
  })
  
  p_value <- mean(abs(perm_stats) >= abs(obs))
  
  list(
    Observed = obs,
    p.value = p_value
  )
}

homework_results <- data.frame(
  variable = character(),
  spearman_corr = numeric(),
  p_value_spearman = numeric(),
  p_value_perm = numeric(),
  stringsAsFactors = FALSE
)

target_vars_hw <- c("lipids2", "lipids3", "lipids4")

for (var in target_vars_hw) {
  spearman_test <- cor.test(df$lipids1, df[[var]], method = "spearman", exact = FALSE)
  
  perm_test <- perm_spearman_test(
    x = df$lipids1,
    y = df[[var]],
    R = 10000
  )
  
  homework_results <- rbind(homework_results, data.frame(
    variable = var,
    spearman_corr = as.numeric(spearman_test$estimate),
    p_value_spearman = spearman_test$p.value,
    p_value_perm = perm_test$p.value
  ))
}

print(homework_results)
write.csv(homework_results, "homework_correlation_results.csv", row.names = FALSE)

df_reg <- df[complete.cases(df[, c("lipids1", "lipids2")]), ]
df_reg <- df_reg[order(df_reg$lipids2), ]

# Linear regression
model_linear_hw <- lm(lipids1 ~ lipids2, data = df_reg)
summary(model_linear_hw)

# 2nd-degree polynomial
model_poly2_hw <- lm(lipids1 ~ poly(lipids2, 2), data = df_reg)
summary(model_poly2_hw)

# 3rd-degree polynomial
model_poly3_hw <- lm(lipids1 ~ poly(lipids2, 3), data = df_reg)
summary(model_poly3_hw)

# Exponential model
model_exp_hw <- lm(log(lipids1) ~ lipids2, data = df_reg)
summary(model_exp_hw)

# Logarithmic model
model_log_hw <- lm(lipids1 ~ log(lipids2), data = df_reg)
summary(model_log_hw)


bic_results_hw <- data.frame(
  model = c("linear", "poly2", "poly3", "exponential", "logarithmic"),
  BIC_value = c(
    BIC(model_linear_hw),
    BIC(model_poly2_hw),
    BIC(model_poly3_hw),
    BIC(model_exp_hw),
    BIC(model_log_hw)
  )
)

bic_results_hw <- bic_results_hw[order(bic_results_hw$BIC_value), ]
print(bic_results_hw)
write.csv(bic_results_hw, "homework_bic_results.csv", row.names = FALSE)

plot(df_reg$lipids2, df_reg$lipids1,
     main = "Scatter plot: lipids1 vs lipids2",
     xlab = "lipids2",
     ylab = "lipids1",
     pch = 16,
     col = "gray")
abline(model_linear_hw, col = "red", lwd = 2)



df_logit <- df[complete.cases(df[, c("outcome", "hormone1", "hormone2", "hormone3", "hormone4")]), ]

model_logit_1_hw <- glm(outcome ~ hormone1,
                        data = df_logit,
                        family = binomial)
summary(model_logit_1_hw)

model_logit_2_hw <- glm(outcome ~ hormone1 + hormone2,
                        data = df_logit,
                        family = binomial)
summary(model_logit_2_hw)

model_logit_all_hw <- glm(outcome ~ hormone1 + hormone2 + hormone3 + hormone4,
                          data = df_logit,
                          family = binomial)
summary(model_logit_all_hw)

logit_compare_hw <- data.frame(
  model = c("model_logit_1_hw", "model_logit_2_hw", "model_logit_all_hw"),
  AIC = c(AIC(model_logit_1_hw), AIC(model_logit_2_hw), AIC(model_logit_all_hw)),
  BIC = c(BIC(model_logit_1_hw), BIC(model_logit_2_hw), BIC(model_logit_all_hw))
)

print(logit_compare_hw)
write.csv(logit_compare_hw, "homework_logit_compare.csv", row.names = FALSE)


df_logit$pred_prob <- predict(model_logit_2_hw, type = "response")


df_logit$pred_class <- ifelse(df_logit$pred_prob > 0.5, 1, 0)
df_logit$pred_class <- as.factor(df_logit$pred_class)


conf_matrix_hw <- table(Actual = df_logit$outcome, Predicted = df_logit$pred_class)
print(conf_matrix_hw)


if (!require(pROC)) install.packages("pROC")
library(pROC)

roc_curve_hw <- roc(df_logit$outcome, df_logit$pred_prob)
plot(roc_curve_hw, main = "ROC curve for model_logit_2")
auc_value_hw <- auc(roc_curve_hw)
print(auc_value_hw)


step_model_hw <- step(model_logit_all_hw, direction = "both")
summary(step_model_hw)


or_table_hw <- exp(cbind(OR = coef(model_logit_2_hw), confint(model_logit_2_hw)))
print(or_table_hw)
write.csv(as.data.frame(or_table_hw), "homework_or_table.csv")

