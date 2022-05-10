# Logistic Regression

# Datat setup ----
obh <- readRDS("C:/Users/cullo/OneDrive/Desktop/One Cloud/Resume 2022/Github Portfolio/rds/obh_logit_ex.rds")


# Logistic Regression Analysis ----

# Run logistic regression model for select variables
obh_log <- obh %>% 
  select(gender, age_intake, adopted:transport, z_dif_levels, rci) %>% 
  filter(age_intake >= 13) %>% 
  na.omit()

# Removing na's brings data down to 2192 from 2418
obh_log_model <- glm(rci ~ ., data = obh_log, family = "binomial")
summary(obh_log_model)

# Create table of logistic model
obh_log_model_df <- as.data.frame(summary(obh_log_model)$coef)
# Add column on odds ratio
obh_log_model_df$oddrat <- exp(coef(obh_log_model))

# Show table with rounded values
obh_log_round <- round(obh_log_model_df,3)


