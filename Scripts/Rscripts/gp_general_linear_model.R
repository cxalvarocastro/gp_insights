#Lets load the DFs we need for the models
source("C:/Users/alvar/OneDrive/Escritorio/gp_insights/Scripts/Rscripts/gp_general_cleaning_up.R")

#we first make the model for ltr considering dfs with the rule for correlation no more than .7 & .8

ltr_model_7 <- lm(ltr ~ .,df_ltr_model_7)
ltr_model_8 <- lm(ltr ~ .,df_ltr_model_8)

summary(ltr_model_7)
summary(ltr_model_8)

#based on the summary lets remove the predictors with no significance for each model
rm_predictors_ltr_7 <- c(2,9,14,16,17,19)
rm_predictors_ltr_8 <- c(2,12,17,19,20,22)


df_ltr_model_7_significate <- df_ltr_model_7[,-rm_predictors_ltr_7]
df_ltr_model_8_significate <- df_ltr_model_8[,-rm_predictors_ltr_8]

summary(lm(ltr ~ .,df_ltr_model_8_significate))
