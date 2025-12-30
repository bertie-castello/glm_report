data <- read.csv("airpollution.csv", stringsAsFactors = TRUE)
packages <- c("ggplot2", "xtable", "stargazer", "rsq", "emmeans")
install.packages(packages)
library(xtable)
library(ggplot2)
library(stargazer)
library(rsq)
library(emmeans)

#### DEFACTORED BECAUSE OF WHAT SAID IN Qe ABOUT OOS AGE PREDICT
# Convert age to factors, since they represent age ranges
# data$age <- as.factor(data$age)
attach(data) #attach lets us call the columns directly, making code more concise
summary(data)


###############################################################################
### A - Exploratory data analysis
###############################################################################

# summary table with counts and proportions for each variable
make_summary <- function(var, name) {
  tbl <- table(var)
  data.frame(
    Variable = name,
    Level = names(tbl),
    Count = as.numeric(tbl),
    Percent = round(100 * as.numeric(prop.table(tbl)), 1)
  )
}

summary_table <- rbind(
  make_summary(respd, "Respiratory Difficulty"),
  make_summary(airpollution, "Air pollution"),
  make_summary(activity, "Activity"),
  make_summary(gender, "Gender"),
  make_summary(age, "Age")
)


stargazer::stargazer(summary_table,
                     type = "latex",
                     summary = FALSE,
                     rownames = FALSE,
                     digits = 2,
                     font.size = "scriptsize",
                     out = "tables/summary_table.tex")

# proportion with respd == "Yes"
prop_tbl <- aggregate(I(respd == "Yes") ~ airpollution + activity + gender + age,
                      data = data, FUN = mean)
names(prop_tbl)[5] <- "PropYes"

# count for each combination
count_tbl <- aggregate(respd ~ airpollution + activity + gender + age,
                       data = data, FUN = length)
names(count_tbl)[5] <- "Count"

prop_tbl <- merge(prop_tbl, count_tbl,
                  by = c("airpollution", "activity", "gender", "age"))

prop_tbl <- prop_tbl[order(prop_tbl$airpollution, prop_tbl$activity, prop_tbl$gender, prop_tbl$age), ]
prop_tbl$PropYes <- sprintf("%.2f", prop_tbl$PropYes)

names(prop_tbl) <- c("Air Pollution", "Activity", "Gender", "Age",
                     "Proportion", "Count")

stargazer::stargazer(prop_tbl,
                     type = "latex",
                     summary = FALSE,
                     rownames = FALSE,
                     digits = 2,
                     font.size = "scriptsize",
                     out = "tables/combo_prop_table.tex")


# PLOTS
plot_names <- list("Air pollution", "Age", "Respiratory Difficulty", "Activity", "Gender")
names(plot_names) <- names(data)

blue_palette <- c("#084C8D", "#0072B2", "#56B4E9", "#A6D8F0")  

# Bar plot for age 
pdf("figs/age_barplot_cex1.pdf", width = 6, height = 6)
par(mfrow = c(1,1), cex = 1.1)
barplot(table(age), 
        col = blue_palette,
        xlab = "Age band",
        ylab = "Count")
dev.off()

# Plot proportions with respiratory Difficulty for binary categories
prop_plot <- function(x, y, text = FALSE) {
  x_label <- plot_names[[deparse(substitute(x))]] 
  y_label <- plot_names[[deparse(substitute(y))]] 
  table <- table(x, y)
  print(table)
  prop <- prop.table(table, 2)
  print(prop)
  bp <- barplot(prop,
          col = blue_palette[c(2, 4)],
          xlab = y_label,
          ylab = "Proportion of cohort",
          beside = TRUE, 
          ylim = c(0, max(prop) + 0.1))
  if(text == TRUE) {text(x = bp, 
       y = prop, 
       labels = signif(prop, 2),
       pos = 3,
       cex = 0.9)}
}

pdf("figs/prop_plots_initial.pdf", width = 7, height = 7)
layout(matrix(c(1, 2, 3, 4, 5, 5), nrow = 3, byrow = TRUE), 
       heights = c(1, 1, 0.3))  # Bottom row is shorter

par(mar = c(4, 4, 2, 2))
prop_plot(respd, gender, text = TRUE)
prop_plot(respd, activity, text = TRUE)
prop_plot(respd, airpollution, text = TRUE)
prop_plot(respd, age, text = TRUE)

par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", 
       legend = c("No respiratory difficulty", "Respiratory difficulty"),
       fill = blue_palette[c(2, 4)],
       bty = "n",
       horiz = TRUE) 

dev.off()

# Now breaking the data down by airpollution = yes or no, showing that gender
# is irrelevant when there is no air pollution, but very relevant when there is
# whereas activity seems to have opposite effects with/ without air pollution

data_pollutionyes <- subset(data, subset = airpollution == "Yes")
data_pollutionno <- subset(data, subset =airpollution == "No")

plot_names <- list(
  respd                         = "Respiratory difficulty",
  gender                        = "Gender",
  activity                      = "Activity",
  age                           = "Age",
  "data_pollutionno$respd"      = "Respiratory difficulty",
  "data_pollutionno$gender"     = "Gender",
  "data_pollutionno$activity"   = "Activity",
  "data_pollutionno$age"        = "Age",
  "data_pollutionyes$respd"     = "Respiratory difficulty",
  "data_pollutionyes$gender"    = "Gender",
  "data_pollutionyes$activity"  = "Activity",
  "data_pollutionyes$age"       = "Age"
)

pdf("figs/barplots_noairpollution.pdf", width = 7, height = 4)
par(mar = c(5.1, 4.1, 4.1, 2.1))
layout(matrix(c(1, 2, 3, 4, 4, 4), nrow = 2, byrow = TRUE), 
       heights = c(1, 0.1))  # Bottom row is shorter

prop_plot(data_pollutionno$respd, data_pollutionno$gender)
prop_plot(data_pollutionno$respd, data_pollutionno$activity)
prop_plot(data_pollutionno$respd, data_pollutionno$age)

par(mar = c(0, 0, 0, 0))
plot.new()

legend("center", 
       legend = c("No respiratory difficulty", "Respiratory difficulty"),
       fill = blue_palette[c(2, 4)],
       bty = "n",
       horiz = TRUE) 
dev.off()

pdf("figs/barplots_yesairpollution.pdf", width = 7, height = 4)
par(mar = c(5.1, 4.1, 4.1, 2.1))
layout(matrix(c(1, 2, 3, 4, 4, 4), nrow = 2, byrow = TRUE), 
       heights = c(1, 0.1))  # Bottom row is shorter

prop_plot(data_pollutionyes$respd, data_pollutionyes$gender)
prop_plot(data_pollutionyes$respd, data_pollutionyes$activity)
prop_plot(data_pollutionyes$respd, data_pollutionyes$age)

par(mar = c(0, 0, 0, 0))
plot.new()

legend("center", 
       legend = c("No respiratory difficulty", "Respiratory difficulty"),
       fill = blue_palette[c(2, 4)],
       bty = "n",
       horiz = TRUE) 

dev.off()

###############################################################################
### B - MODELLING
###############################################################################

# Baseline model: all interactions between airpollution and other explanatory variables 

resp.glm1 <- glm(respd ~ airpollution * (age + gender + activity), 
                 data = data, 
                 family = binomial)

summary(resp.glm1)

anova(resp.glm1) #initial anova suggests all interactions are significant
# thus by the hierarchy principle we should keep all the lower order terms too
# by the wald we should get rid of all baselines except activity. Gender is especially insignificant
# by the LR gender is extremely significant and activity insignificant. 
# this bc anova sequential and goes gender first, then activity and interactions. see glm1.1 below

LRT <- function(baseline, restricted) {
  LRT <- restricted$deviance - baseline$deviance
  p_val <- 1 - pchisq(LRT, restricted$df.residual - baseline$df.residual)
  out <- list(
    LRT = LRT, 
    p_value = p_val
  )
  return(out)
}

# likelihood ratio for model useless:

resp.glm0 <- glm(respd ~ 1, data = data, family = binomial)

L1 <- LRT(resp.glm1, resp.glm0) # approximately 0 --> the model is not useless

# Consider dropping any of the interactions
# All effects are significant, do not drop any. Agrees with Wald

resp.glm2 <- glm(respd ~ airpollution * (gender + activity) + age, 
                 data = data, 
                 family = binomial)

L2 <- LRT(resp.glm1, resp.glm2) 
resp.glm3 <- glm(respd ~ airpollution * (gender + age) + activity, 
                data = data, 
                family = binomial)
L3 <- LRT(resp.glm1, resp.glm3)

resp.glm4 <- glm(respd ~ airpollution * (activity + age) + gender, 
                 data = data, 
                 family = binomial)
L4 <- LRT(resp.glm1, resp.glm4)

# Dropping the baseline effects except activity. This is suggested by Wald
data$agexpollution <- data$age*(data$airpollution == "Yes")
data$genderxpollution <- as.integer(data$gender == "Male" & data$airpollution == "Yes")
data$activityxpollution <- as.integer(data$activity == "Yes" & data$airpollution == "Yes")

resp.glm5 <- glm(respd ~ activity + agexpollution + genderxpollution + activityxpollution, 
                 data = data, 
                 family = binomial)
L5 <- LRT(resp.glm1, resp.glm5) # p-value is 0.478, suggesting we should drop all baselines!

# drop age baseline
resp.glm6 <- glm(respd ~ airpollution*(gender + activity) + agexpollution,
                 data = data,
                 family = binomial)

L6 <- LRT(resp.glm1, resp.glm6) 

# drop gender baseline
resp.glm7 <- glm(respd ~ airpollution*(activity) + agexpollution + genderxpollution, 
                 data = data, 
                 family = binomial)
summary(resp.glm7)

L7 <- LRT(resp.glm6, resp.glm7)

# drop activity main effect (against Wald)
resp.glm8 <- glm(respd ~ airpollution + activityxpollution + agexpollution + genderxpollution, 
                 data = data, 
                 family = binomial)
summary(resp.glm8)
L8 <- LRT(resp.glm7, resp.glm8) # reject

# finally, drop airpollution main effect
# since all the coefficients are statistically significant, we should choose final model resp.glm5
# for final verification, see a plot of the AICs and a table of the R^2 KL for all the models 

# why we should keep the baseline interaction - the hierarchy principle

data$airpollutionprime <- ifelse((data$airpollution == "Yes"), 0, 1)
resp.glmairpolltoy <- glm(respd ~ airpollutionprime * (age + gender + activity), 
                          data = data, 
                          family = binomial)

resp.glmairpolltoy_nointeractions <- glm(respd ~ airpollutionprime + airpollutionprime:gender + 
                                           airpollutionprime:activity + airpollutionprime:age, 
                                         data = data, 
                                         family = binomial)
summary(resp.glmairpolltoy_nointeractions)
LRT(resp.glmairpolltoy, resp.glmairpolltoy_nointeractions) 

chosen_model <- resp.glm1
reduced_model <- resp.glm5

########### TABLES ###############
stargazer(resp.glm1,
          type = "latex",
          title = "Logistic Regression of Respiratory difficulty",
          dep.var.labels = "Respiratory difficulty",
          covariate.labels = c(
            "Air pollution (Yes)",
            "Age",
            "Male",
            "Active",
            "Air pollution × Age",
            "Air pollution × Male",
            "Air pollution × Active"
          ),
          digits = 2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE,
          float = TRUE,
          font.size = "scriptsize",
          report = "vc*sp",
          single.row = TRUE,
          out = "tables/resp.glm1.tex")

stargazer(resp.glm5,
          type = "latex",
          title = "Logistic Regression of Respiratory Difficulty",
          dep.var.labels = "Respiratory Difficulty",
          covariate.labels = c(
            "Active",
            "Air pollution × Age",
            "Air pollution × Male",
            "Air pollution × Active"
          ),
          digits = 2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE,
          float = TRUE,
          single.row = TRUE,
          font.size = "scriptsize",
          report = "vc*sp",
          out = "tables/resp.glm5.tex")

likelihood_ratios <- round(c(L1$LRT, L2$LRT, L3$LRT, L4$LRT, L5$LRT, L6$LRT, L7$LRT, L8$LRT), 2)
p_values <- round(c(L1$p_value, L2$p_value, L3$p_value, L4$p_value, L5$p_value, L6$p_value, L7$p_value, L8$p_value), 3)
full_model <- c("glm1", "glm1", "glm1", "glm1", "glm1", "glm1", "glm6", "glm7")
restricted_model <- c("glm0","glm2","glm3","glm4","glm5","glm6","glm7","glm8")
drop_vector <- c(
  "All (null test)",
  "Age x Air Pollution",
  "Activity x Air Pollution",
  "Gender x Air Pollution",
  "Air Pollution, Age, Gender",
  "Age",
  "Gender",
  "Activity"
)

likelihood_ratio_table <- data.frame(
  "Full Model" = full_model, 
  "Restricted Model" = restricted_model,
  "Drop:" = drop_vector, 
  "Likelihood Ratio Statistic" = likelihood_ratios,
  "P-values" = p_values)

stargazer(likelihood_ratio_table,
          type = "latex",
          summary = FALSE,
          rownames = FALSE,
          digits = 2,
          font.size = "scriptsize",
          out = "tables/likelihood_ratio_table.tex")

models <- list(resp.glm1, resp.glm2, resp.glm3, resp.glm4, resp.glm5, resp.glm6, resp.glm7, resp.glm8)
model_names <- c("glm1", "glm2", "glm3", "glm4", "glm5", "glm6", "glm7", "glm8")
aics <- sapply(models, AIC)
bics <- sapply(models, BIC)
deviances <- sapply(models, deviance)
rsq_vals <- round(sapply(models, rsq.kl), 3)

model_desc <- c(
  "Full model",
  "Drop Age x Air Pollution",
  "Drop Activity x Air Pollution", 
  "Drop Gender x Air Pollution",
  "Drop Air Pollution, Age, Gender",
  "Drop Age",
  "Drop Age, Gender",
  "Drop Activity, Age, Gender"
)

aic_bic_etc_table <- data.frame(
  "Model" = model_names,
  "Description" = model_desc,
  "Residual Deviance" = deviances,
  "AIC" = aics,
  "BIC" = bics,
  "R^2 Kullback-Leibler" = rsq_vals
)

stargazer(aic_bic_etc_table,
          type = "latex", 
          font.size = "scriptsize", 
          summary = FALSE,
          rownames =FALSE, 
          digits = 2, 
          out = "tables/aic_bic_etc_table.tex")

stargazer(resp.glmairpolltoy,
          type = "latex",
          title = "Toy model - Logistic Regression of Respiratory difficulty",
          dep.var.labels = "Respiratory difficulty",
          covariate.labels = c(
            "Air pollution (No)",
            "Age",
            "Male",
            "Active",
            "Air pollution × Age",
            "Air pollution × Male",
            "Air pollution × Active"
          ),
          digits = 2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE,
          float = TRUE,
          font.size = "scriptsize",
          report = "vc*sp",
          single.row = TRUE,
          out = "tables/toymodel.tex")



##############################################################################
### C - Model diagnostics 
##############################################################################

# Residual plots

set.seed(30)

pdf("figs/residual_plots.pdf", width = 9.5, height = 10)
par(mfrow = c(2, 2), margin(5, 5, 3, 3), cex = 1)

plot(jitter(predict(chosen_model, type = 'link'), 10), 
                       jitter(rstandard(chosen_model), 0), col = blue_palette[2], 
                       xlab = expression(hat(eta)), 
                       ylab = "Standardised Deviance Residuals")
abline(a = -2, b = 0, lty = 2)
abline(a = 2, b = 0, lty = 2)

# QQ plot

qqnorm(rstandard(chosen_model), main = "")
qqline(rstandard(chosen_model))

# Leverage 

p <- chosen_model$df.null - chosen_model$df.residual + 1
n <- nrow(data)

plot(jitter(influence(chosen_model)$hat/(p/n), 200), ylab = "Leverage / (p/n)", 
     col = blue_palette[2])
abline(2, 0, lty = 2)

# Cook's distance 

plot(jitter(cooks.distance(chosen_model), 100), ylab = "Cook's Distance",
     col = blue_palette[2])
abline(8/(n - 2 * p), 0, lty = 2)
dev.off()

leverage <- influence(chosen_model)$hat
cooks_d <- cooks.distance(chosen_model)

# Thresholds
leverage_threshold <- 2 * (p / n)
cooks_threshold <- 8 / (n - 2 * p)

# Problematic observations
high_leverage <- which(leverage > leverage_threshold)
high_cooks <- which(cooks_d > cooks_threshold)

# Summary table of problematic observations
problematic <- intersect(high_leverage, high_cooks)
diag_table <- data.frame(
  data[problematic, 1:5],
  Leverage = round(leverage[problematic], 4) / (p/n),
  Cooks_D = round(cooks_d[problematic], 4)
)
print(diag_table)

colnames(diag_table) <- c("Air Pollution","Age", "Respiratory Difficulty", "Activity","Gender", "Leverage", "Cook's D")

stargazer(diag_table,
          summary = FALSE,
          rownames = FALSE,
          type = "latex",
          title = "Observations with High Leverage and High Cook's Distance",
          font.size = "scriptsize",
          float = TRUE,
          out = "tables/diag_table.tex")

##############################################################################
### D - INTERPRETATION 
##############################################################################

beta <- coef(chosen_model)
se <- sqrt(diag(vcov(chosen_model)))
z <- qnorm(0.975)

# Calculate CIs
ci_lower <- beta - z * se
ci_upper <- beta + z * se

# Build table
ci_table <- data.frame(
  Variable = c(
    "Intercept",
    "Air pollution (Yes)",
    "Age",
    "Male",
    "Active",
    "Air pollution × Age",
    "Air pollution × Male",
    "Air pollution × Active"
  ),
  Estimate = sprintf("%.2f", beta),
  CI = sprintf("(%.2f, %.2f)", ci_lower, ci_upper)
)

colnames(ci_table) <- c("Variable", "Estimate", "95\\% CI")

stargazer(ci_table,
          summary = FALSE,
          rownames = FALSE,
          type = "latex",
          title = "Logistic Regression Coefficients with 95\\% Confidence Intervals",
          font.size = "scriptsize",
          float = TRUE,
          out = "tables/ci_table.tex")



odds_ratio_and_ci <- function(variable) {
  beta <- coef(chosen_model)
  V <- vcov(chosen_model)
  interaction <- paste("airpollutionYes", variable, sep = ":")
  
  # Air pollution present
  logor_airpollution1 <- beta[variable] + beta[interaction]
  variance_1 <- V[variable, variable] + V[interaction, interaction] + 2 * V[variable, interaction]
  se_1 <- sqrt(variance_1)
  logor_airpollution1_ci <- logor_airpollution1 + 1.96 * se_1 * c(-1, 1)
  
  # Air pollution absent
  logor_airpollution0 <- beta[variable]
  variance_0 <- V[variable, variable]
  se_0 <- sqrt(variance_0)
  logor_airpollution0_ci <- logor_airpollution0 + 1.96 * se_0 * c(-1, 1)
  
  # For age: multiply log-odds by 10
  if (variable == "age") {
    logor_airpollution1 <- logor_airpollution1 * 10
    logor_airpollution1_ci <- logor_airpollution1_ci * 10
    logor_airpollution0 <- logor_airpollution0 * 10
    logor_airpollution0_ci <- logor_airpollution0_ci * 10
  }
  
  data.frame(
    Air_Pollution = c("Present", "Absent"),
    Odds_Ratio = sprintf("%.2f", exp(c(logor_airpollution1, logor_airpollution0))),
    CI = c(
      sprintf("(%.2f, %.2f)", exp(logor_airpollution1_ci[1]), exp(logor_airpollution1_ci[2])),
      sprintf("(%.2f, %.2f)", exp(logor_airpollution0_ci[1]), exp(logor_airpollution0_ci[2]))
    )
  )
}

variable_names <- names(beta)[3:5]
variable_nicenames <- c("Age", "Gender", "Activity")

# calculate using the delta method
odds_ratio_and_ci_airpollution <- function(age, gender, activity) {
  beta <- coef(chosen_model)
  V <- vcov(chosen_model)
  ap <- "airpollutionYes"
  apag <- "airpollutionYes:age"
  apge <- "airpollutionYes:genderMale"
  apac <- "airpollutionYes:activityYes"
  var <- V[ap, ap] +
    sum(c(V[apag, apag], V[apge, apge], V[apac, apac]) * c(age, gender, activity)**2) +
    2 * sum(c(V[ap, apag], V[ap, apge], V[ap, apac]) * c(age, gender, activity)) +
    2 * age * sum(c(V[apag, apge], V[apag, apac]) * c(gender, activity)) +
    2 * gender * activity * V[apge, apac]
  se <- sqrt(var)
  log_odds_ratio <- sum(beta[c(ap, apag, apge, apac)] * c(1, age, gender, activity))
  log_odds_ci <- log_odds_ratio + 1.96 * c(-1, 1) * se
  odds_ratio <- exp(log_odds_ratio)
  odds_ci <- exp(log_odds_ci)
  out <- c(
    "Odds Ratio" = round(odds_ratio, 2),
    "Odds Ratio CI" = round(odds_ci, 2)
  )
  return(out)
}


######### TABLES #############

# OTHER ODDS 

combined_table <- rbind(
  cbind(Variable = "Age (10-year increase)", odds_ratio_and_ci("age")),
  cbind(Variable = "Gender (Male vs Female)", odds_ratio_and_ci("genderMale")),
  cbind(Variable = "Activity (Active vs Inactive)", odds_ratio_and_ci("activityYes"))
)
combined_table$Variable[c(2,4,6)] <- ""  # Remove duplicate labels

colnames(combined_table) <- c("Variable", "Air Pollution", "Odds Ratio", "95 % CI")

stargazer(combined_table, 
          summary = FALSE, 
          rownames = FALSE,
          type = "latex",
          font.size = "scriptsize", 
          digits = 2, 
          out = "tables/combined_odds_ratios_table.tex")

# AIRPOLLUTION ODDS 

combos <- expand.grid(
  age = c(25, 35, 45, 55),
  gender = c(1, 0),
  activity = c(1, 0)
)

results <- t(mapply(
  gender = combos$gender,
  activity = combos$activity,
  age = combos$age,
  odds_ratio_and_ci_airpollution
))

odds_table <- cbind(combos, results)
odds_table$gender <- ifelse(odds_table$gender == 1, "\texttt{M}", "\texttt{F}")
odds_table$activity <- ifelse(odds_table$activity == 1, "Active", "Inactive")
odds_table$CI <- sprintf("(%.2f, %.2f)", odds_table[,5], odds_table[,6])

odds_table <- odds_table[, c("gender", "activity", "age", "Odds Ratio", "CI")]
colnames(odds_table) <- c("Gender", "Activity", "Age", "Odds Ratio", "95\\% OR Confidence Interval")

stargazer(odds_table,
          type = "latex", 
          font.size = "scriptsize", 
          summary = FALSE,
          rownames =FALSE, 
          digits = 2, 
          out = "tables/odds_table.tex")


##############################################################################
### E - Prediction 
##############################################################################
# new prediction data frames
predict_1 <- data.frame(
  activity = "Yes",
  age = 45,
  gender = "Female",
  airpollution = "Yes"
)

predict_2 <- data.frame(
  activity = "No",
  age = 85,
  gender = "Male",
  airpollution = "Yes"
)

# linear regression first (for conf. int's)
pred1_link <- predict(chosen_model, newdata = predict_1, type = "link", se.fit = TRUE)
pred2_link <- predict(chosen_model, newdata = predict_2, type = "link", se.fit = TRUE)

get_prob_ci <- function(pred_link) {
  fit <- pred_link$fit
  se <- pred_link$se.fit
  ci_link <- fit + 1.96 * se * c(-1, 1)
  prob <- plogis(fit)
  ci_prob <- plogis(ci_link)
  return(c(prob, ci_prob))
}

prob1 <- get_prob_ci(pred1_link)
prob2 <- get_prob_ci(pred2_link)

pred_table <- data.frame(
  Activity = c("Yes", "No"),
  Age = c(45, 85),
  Gender = c("Female", "Male"),
  Air_Pollution = c("Yes", "Yes"),
  Probability = sprintf("%.3f", c(prob1[1], prob2[1])),
  CI = c(
    sprintf("(%.3f, %.3f)", prob1[2], prob1[3]),
    sprintf("(%.3f, %.3f)", prob2[2], prob2[3])
  )
)

colnames(pred_table) <- c("Activity", "Age", "Gender", "Air Pollution", 
                          "Predicted Probability", "95 % CI")

stargazer(pred_table, 
          summary = FALSE, 
          rownames = FALSE, 
          type = "latex",
          title = "Predicted Probabilities of Respiratory Difficulty - Full Model",
          font.size = "scriptsize",
          out = "tables/predicted_probs.tex")


# notice that while we can get confidence intervals for the *probabilities*, we cannot get 
# prediction intervals for the probabilities, since the realisation will always jsut be 0 or 1 for resp
# so prediction intervals are meaningless here

##############################################################################
### APPENDIX CODE 
##############################################################################

### THE FACTOR MODEL ###

data$agef <- as.factor(data$age)

respd.glmf <- glm(respd ~ airpollution *(agef + gender + activity), 
                  data = data, 
                  family = binomial)
respd.glmf.1 <- glm(respd ~ airpollution *(agef + activity + gender), 
                    data = data, 
                    family = binomial)
respd.glmf.2 <- glm(respd ~ airpollution *(activity + agef + gender), 
                    data = data, 
                    family = binomial)
summary(respd.glmf)
anova(respd.glmf)
anova(respd.glmf.1)
anova(respd.glmf.2)

stargazer(respd.glmf,
          type = "latex",
          title = "Logistic Regression of Respiratory difficulty",
          dep.var.labels = "Respiratory difficulty",
          covariate.labels = c(
            "Air pollution (Yes)",
            "Age 35",
            "Age 45",
            "Age 55",
            "Male",
            "Active",
            "Air pollution × Age 35",
            "Air pollution × Age 45",
            "Air pollution × Age 55",
            "Air pollution × Male",
            "Air pollution × Active"
          ),
          digits = 2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE,
          float = TRUE,
          font.size = "scriptsize",
          single.row = TRUE,
          out = "tables/resp.glmf.tex")

# in all cases in the factor model we see that the baseline age effect is not significant
# this provides further evidence for dropping
# activity is significant when added first, gender is always highly significant in the factor model


### extra plot - residuals vs fitted values 

pdf("figs/residual_vs_mu.pdf")
plot(jitter(predict(chosen_model, type = 'response'), 10), 
     jitter(rstandard(chosen_model), 0), col = blue_palette[2], 
     xlab = expression(hat(mu)), 
     ylab = "Standardised Deviance Residuals")
abline(a = -2, b = 0, lty = 2)
abline(a = 2, b = 0, lty = 2)
dev.off()


######### FOR THE REDUCED MODEL ##############

##### DIAGNOSTICS #######


# residual plots

set.seed(30)

pdf("figs/residual_plots_reducedmodel.pdf", width = 9.5, height = 10)
par(mfrow = c(2, 2), margin(5, 5, 3, 3), cex = 1)

plot(jitter(predict(reduced_model, type = 'link'), 10), 
     jitter(rstandard(reduced_model), 0), col = blue_palette[2], 
     xlab = expression(hat(eta)), 
     ylab = "Standardised Deviance Residuals")
abline(a = -2, b = 0, lty = 2)
abline(a = 2, b = 0, lty = 2)

# qq plot

qqnorm(rstandard(reduced_model), main = "")
qqline(rstandard(reduced_model))

# leverage 

p <- reduced_model$df.null - reduced_model$df.residual + 1
n <- nrow(data)

plot(jitter(influence(reduced_model)$hat/(p/n), 200), ylab = "Leverage / (p/n)", 
     col = blue_palette[2])
abline(2, 0, lty = 2)

# cook's distance 

plot(jitter(cooks.distance(reduced_model), 100), ylab = "Cook's Distance",
     col = blue_palette[2])
abline(8/(n - 2 * p), 0, lty = 2)
dev.off()

# ACTIVITY 
# odds ratio depends on air pollution 

beta <- coef(reduced_model)
V <- vcov(reduced_model)

se_sum <- function(b1_name, b2_name) {
  b_sum <- beta[b1_name] + beta[b2_name]
  var_sum <- V[b1_name, b1_name] + V[b2_name, b2_name] + 2 * V[b1_name, b2_name]
  return(se_sum <- sqrt(var_sum))
}

OR_activity_1 <- exp (beta[activity])
or_act_ap0 <- exp(reduced_model$coefficients[2])

CI_or_act_ap1 <- exp(summary(reduced_model)$coefficients[2,1] + summary(reduced_model)$coefficients[5,1]
                     + 1.96 * se_sum("activityYes", "activityxpollution") *
                       c(-1, 1))

CI_or_act_ap0 <- exp(summary(reduced_model)$coefficients[2,1]
                     + 1.96 * summary(reduced_model)$coefficients[2,2] *
                       c(-1, 1))

# Genderxairpollution 
# odds ratio again depends on air pollution, but there is no term for airpo = 0 

or_gender_ap1 <- exp(reduced_model$coefficients[4])
ci_or_gender_ap1 <- exp(summary(reduced_model)$coefficients[4,1]
                        + 1.96 * summary(reduced_model)$coefficients[4,2] *
                          c(-1, 1))

# age --> measured for 10 years 
or_age_ap1 <- exp(reduced_model$coefficients[3] * 10)
ci_or_age_ap1 <- exp(summary(reduced_model)$coefficients[3,1] * 10
                     + 1.96 * 10 *summary(reduced_model)$coefficients[3,2] *
                       c(-1, 1))

# airpollution
# start with age = 25
act1_gender1 <- function(age) {
  or_ap_a1g1 <- exp(reduced_model$coefficients[3]*age +
                      reduced_model$coefficients[4] +
                      reduced_model$coefficients[5])
  
  se_sum_three <- sqrt(se_sum("genderxpollution", "activityxpollution") + 
                         age ** 2 * V["agexpollution", "agexpollution"] +
                         2 * age * (V["agexpollution", "genderxpollution"] + 
                                      V["agexpollution", "activityxpollution"])
  )
  
  CI_or_ap_a1g1 <- exp(reduced_model$coefficients[3]* age +
                         reduced_model$coefficients[4] +
                         reduced_model$coefficients[5] + 
                         1.96 * 
                         se_sum_three * c(-1, 1))
  output <- c(odds_ratio = or_ap_a1g1,
              conf_int = CI_or_ap_a1g1)
  return(output)
}

act1g1confints <- lapply(c(25,35,45,55), act1_gender1)

act1_gender0 <- function(age) {
  or_ap_a1g1 <- exp(reduced_model$coefficients[3]*age +
                      reduced_model$coefficients[5])
  
  se_sum_three <- sqrt(V["activityxpollution", "activityxpollution"]+ 
                         age ** 2 * V["agexpollution", "agexpollution"] +
                         2 * age * V["agexpollution", "activityxpollution"]
  )
  
  CI_or_ap_a1g1 <- exp(reduced_model$coefficients[3]* age +
                         reduced_model$coefficients[5] + 
                         1.96 * 
                         se_sum_three * c(-1, 1))
  output <- c(odds_ratio = or_ap_a1g1,
              conf_int = CI_or_ap_a1g1)
  return(output)
}

act1g0confints <- lapply(c(25,35,45,55), act1_gender0)

act0_gender1 <- function(age) {
  or_ap_a1g1 <- exp(reduced_model$coefficients[3]*age +
                      reduced_model$coefficients[4])
  
  se_sum_three <- sqrt(V["genderxpollution", "genderxpollution"]+ 
                         age ** 2 * V["agexpollution", "agexpollution"] +
                         2 * age * V["agexpollution", "genderxpollution"]
  )
  
  CI_or_ap_a1g1 <- exp(reduced_model$coefficients[3]* age +
                         reduced_model$coefficients[4] + 
                         1.96 * 
                         se_sum_three * c(-1, 1))
  output <- c(odds_ratio = or_ap_a1g1,
              conf_int = CI_or_ap_a1g1)
  return(output)
}

act0g1confints <- lapply(c(25,35,45,55), act0_gender1)

act0_gender0 <- function(age) {
  or_ap_a1g1 <- exp(reduced_model$coefficients[3]*age)
  
  se_sum_three <- sqrt(age ** 2 * V["agexpollution", "agexpollution"])
  
  CI_or_ap_a1g1 <- exp(reduced_model$coefficients[3]* age +
                         1.96 * 
                         se_sum_three * c(-1, 1))
  output <- c(odds_ratio = or_ap_a1g1,
              conf_int = CI_or_ap_a1g1)
  return(output)
}

act0g0confints <- lapply(c(25,35,45,55), act0_gender0)


# new prediction data frames - reduced model 
predict_1_rm <- data.frame(
  activity = "Yes",
  agexpollution = 45,
  genderxpollution = 0,
  activityxpollution = 1
)

predict_2_rm <- data.frame(
  activity = "No",
  agexpollution = 85,
  genderxpollution = 1,
  activityxpollution = 0
)

# linear regression first (for confidence intervals)
pred1_link_rm <- predict(reduced_model, newdata = predict_1_rm, type = "link", se.fit = TRUE)
pred2_link_rm <- predict(reduced_model, newdata = predict_2_rm, type = "link", se.fit = TRUE)

get_prob_ci(pred1_link_rm)  # Female, 45, active, pollution
get_prob_ci(pred2_link_rm)  # Male, 85, inactive, pollution

prob1_rm <- get_prob_ci(pred1_link_rm)
prob2_rm <- get_prob_ci(pred2_link_rm)

pred_table <- data.frame(
  Activity = c("Yes", "No"),
  Age = c(45, 85),
  Gender = c("Female", "Male"),
  Air_Pollution = c("Yes", "Yes"),
  Probability = sprintf("%.3f", c(prob1_rm[1], prob2_rm[1])),
  CI = c(
    sprintf("(%.3f, %.3f)", prob1_rm[2], prob1_rm[3]),
    sprintf("(%.3f, %.3f)", prob2_rm[2], prob2_rm[3])
  )
)

colnames(pred_table) <- c("Activity", "Age", "Gender", "Air Pollution", 
                          "Predicted Probability", "95 % CI")

stargazer(pred_table, 
          summary = FALSE, 
          rownames = FALSE, 
          type = "latex",
          title = "Predicted Probabilities of Respiratory Difficulty - Full Model",
          font.size = "scriptsize",
          out = "tables/predicted_probs_rm.tex")
