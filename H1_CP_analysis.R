library('readxl')

#select the data file path
file_path = file.choose()

#load the data
data <- read_excel(file_path, sheet = 'All_data')


# Load library for GLMM
library(lme4)
data$Complexity <- factor(data$Complexity, levels = c("low", "mid", "high"))
data$Complexity_val <- as.numeric(log(data$Complexity_val, base = 10))
data$Score <- as.numeric(data$NewScore)
#data$log_complexity <- log(data$complexity)
model1 <- lmer(NewScore ~ log_com + (1 | log_com), data = llm.dat)
summary(model1)

emm <- emmeans(model1, ~ Com_G3)

# Calculate the estimated marginal trends (slopes) for 'complexity' within each 'group'
#slopes <- emtrends(model, ~ group, var = "log_complexity")
slopes <-emtrends(model1, ~Com_G6, var = "log_com")

# Compare slopes between groups
slope_comparisons <- summary(pairs(slopes))

# Print the results
print(slope_comparisons)

# Optional: Plot the slopes
plot(slopes)
# Plotting residuals against fitted values
ggplot(data, aes(x=log_complexity, y=stage, color=group)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE)



#####################################################################################

# Fit a model with only random intercepts for comparison
model_with_intercept <- lmer(stage ~ complexity + (1 | suject), data = data)

# Perform a likelihood ratio test
anova(model_with_intercept, model)

# Get the fixed effect for 'predictor'
fixed_effect <- fixef(model)["complexity"]

# Extract the random slopes for 'predictor'
random_slopes <- ranef(model)$suject[,"complexity"]

# Calculate the rate of change for each participant
participant_effects <- fixed_effect + random_slopes


library(ggplot2)

# Convert to data frame for ggplot
participant_effects_df <- data.frame(participant = c("p1","p2","p3","p4","p5","p6","p7","p8"), effect = participant_effects)

# Plot
ggplot(participant_effects_df, aes(x = participant, y = effect)) +
  geom_point() +
  geom_hline(yintercept = fixed_effect, linetype = "dashed", color = "red") +
  labs(y = "Rate of Change", x = "Participant", title = "Participant-specific Rate of Change")





########################### comparing intercept #########################

# Fit linear regression models
model_group1 <- lm(NewScore ~ 1, data = llm.dat[llm.dat$Com_G3 == "low", ])
model_group2 <- lm(NewScore ~ 1, data = llm.dat[llm.dat$Com_G3 == "high", ])

# Extract intercepts
intercept_group1 <- coef(model_group1)[1]
intercept_group2 <- coef(model_group2)[1]

# Compare intercepts using t-test
t_test <- t.test(model_group1$residuals, model_group2$residuals)

# Print results
print(paste("Intercept Group1:", intercept_group1))
print(paste("Intercept Group2:", intercept_group2))
print(t_test)


###################### Change Point analysis ############################
library(segmented)

## including outliers
initial_model <- lm(NewScore~log_com, llm.dat)
#summary(initial_model)

seg_model<- segmented(initial_model)
summary(seg_model)
AIC(initial_model, seg_model)
llm.dat$fitted_val <- seg_model$fitted.values

#excluding outliers
initial_model <- lm(NewScore~log_com, dat_without_outliers)
#summary(initial_model)

seg_model<- segmented(initial_model)
summary(seg_model)
confint(seg_model)
# check if there exist other break points
davies.test(seg_model)


## using multiple breakpoints
# Fit segmented regression with multiple breakpoints
seg_model_multi <- segmented(initial_model, seg.Z = ~log_com, psi = list(log_com = c(-0.111, 2.5912)))
summary(seg_model_multi)

# setting the psi between the 1 to 4 billions window
seg_model_multi1 <- segmented(initial_model, seg.Z = ~log_com, psi = list(log_com = c(0, 0.60206)))
seg_model_multi2 <- segmented(
  initial_model,
  seg.Z = ~log_com,
  npsi = 2
)


summary(seg_model_multi1)
summary(seg_model_multi2)

# davies test to identify if there exit 3rd breakpoint
davies.test(seg_model_multi1)

### sanity test #######################
num_tests <- 10
results <- data.frame(initial_psi1 = numeric(num_tests), 
                      initial_psi2 = numeric(num_tests), 
                      estimated_psi1 = numeric(num_tests), 
                      estimated_psi2 = numeric(num_tests))

for (i in 1:num_tests) {
  set.seed(100 + i)  # Change seed each time
  psi_start <- sort(runif(2, min = -0.5, max = 2.5))
  cat("Iteration", i, "- Initial psi:", psi_start, "\n")
  
  # Safe model fitting
  seg_model_test <- try(segmented(
    initial_model, 
    seg.Z = ~log_com, 
    psi = list(log_com = psi_start)
  ), silent = TRUE)
  
  if (inherits(seg_model_test, "try-error")) {
    cat("⚠️ Skipping iteration", i, "due to model fitting error.\n")
    next
  }
  
  results[i, ] <- c(psi_start[1], psi_start[2],
                    seg_model_test$psi[1, "Est."],
                    seg_model_test$psi[2, "Est."])
}
print(results)

#################### Model fit comparison ######################################
simple_model <- lm(NewScore ~ log_com, data = dat_without_outliers)

# Fit the one-breakpoint model
one_breakpoint_model <- segmented(simple_model, seg.Z = ~log_com)

# Fit the two-breakpoint model
two_breakpoint_model <- segmented(simple_model, seg.Z = ~log_com, psi = list(log_com = c(0, 0.60206)))

# Extract metrics for comparison
compare_models <- data.frame(
  Model = c("Simple Regression", "One-Breakpoint Model", "Two-Breakpoint Model"),
  AIC = c(AIC(simple_model), AIC(one_breakpoint_model), AIC(two_breakpoint_model)),
  BIC = c(BIC(simple_model), BIC(one_breakpoint_model), BIC(two_breakpoint_model)),
  RSS = c(
    sum(residuals(simple_model)^2),
    sum(residuals(one_breakpoint_model)^2),
    sum(residuals(two_breakpoint_model)^2)
  ),
  Adj_R2 = c(
    summary(simple_model)$adj.r.squared,
    summary(one_breakpoint_model)$adj.r.squared,
    summary(two_breakpoint_model)$adj.r.squared
  )
)

# Print the comparison table
print(compare_models)

anova_results <- anova(simple_model, one_breakpoint_model, two_breakpoint_model)

# Display the ANOVA table
print(anova_results)

# Optional: Format the output as a clean table
library(knitr)
anova_table <- data.frame(
  Model = c("Simple Regression", "One-Breakpoint Model", "Two-Breakpoint Model"),
  RES_DF = anova_results$Df,
  RSS = anova_results$`RSS`,
  DF = c(NA, diff(anova_results$Df[1:2]), diff(anova_results$Df[2:3])),
  SUM_OF_SQ = c(NA, diff(anova_results$`RSS`[1:2]), diff(anova_results$`RSS`[2:3])),
  F = c(NA, anova_results$`F`[2], anova_results$`F`[3]),
  P_VALUE = c(NA, anova_results$`Pr(>F)`[2], anova_results$`Pr(>F)`[3])
)

kable(anova_table, align = "c", col.names = c(
  "Model", "RES. DF", "RSS", "DF", "SUM OF SQ", "F", "P-VALUE"
))


