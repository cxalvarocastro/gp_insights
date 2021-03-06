#Lets load the DFs we need for the models
source("C:/Users/alvar/OneDrive/Escritorio/gp_insights/Scripts/Rscripts/gp_general_cleaning_up.R")

#libraries
library(plotly)

#functions
hline <- function(y = 0, color = "red") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color)
  )
}

#we first make the model for ltr considering dfs with the rule for correlation no more than .7 & .8

#################################### LTR STARTS #################################### 
ltr_model_7 <- lm(ltr ~ .,df_ltr_model_7)
ltr_model_8 <- lm(ltr ~ .,df_ltr_model_8)

summary(ltr_model_7)
summary(ltr_model_8)

#based on the summary lets remove the predictors with no significance for each model
rm_predictors_ltr_7 <- c(2,9,14,16,17,19)
rm_predictors_ltr_8 <- c(2,12,17,19,20,22)


df_ltr_model_7_significate <- df_ltr_model_7[,-rm_predictors_ltr_7]
df_ltr_model_8_significate <- df_ltr_model_8[,-rm_predictors_ltr_8]

#we create the new models
lm_ltr_7 <- lm(ltr ~ .,df_ltr_model_7_significate)
lm_ltr_8 <- lm(ltr ~ .,df_ltr_model_8_significate)

summary(lm_ltr_7)
summary(lm_ltr_8)

#now we look at each variable's weight 
sum_tot_7 <- sum(abs(lm_ltr_7$coefficients[-1]))
sum_tot_8 <- sum(abs(lm_ltr_8$coefficients[-1]))

coef_ltr_7 <- abs(lm_ltr_7$coefficients[-1])/sum_tot_7 
coef_ltr_8 <- abs(lm_ltr_8$coefficients[-1])/sum_tot_8

#ordering values to plot them
df_coef_ltr_7 <- data.frame(name=names(coef_ltr_7), 
                            weight=as.numeric(coef_ltr_7), 
                            stringsAsFactors = FALSE)

df_coef_ltr_8 <- data.frame(name=names(coef_ltr_8), 
                            weight=as.numeric(coef_ltr_8), 
                            stringsAsFactors = FALSE)
#
df_coef_ltr_7$name <- factor(df_coef_ltr_7$name, 
                             levels = unique(df_coef_ltr_7$name)[order(df_coef_ltr_7$weight, 
                                                                       decreasing = TRUE)])
df_coef_ltr_8$name <- factor(df_coef_ltr_8$name, 
                             levels = unique(df_coef_ltr_8$name)[order(df_coef_ltr_8$weight, 
                                                                       decreasing = TRUE)])
#
plot_ltr_7 <- plot_ly(df_coef_ltr_7, x = ~name, y = ~weight, type = "bar") %>%
              layout(shapes = list( hline(.1))) %>%
              layout(title = "LTR most important predictors - 0.7",
                     xaxis = list(title = "Predictors"),
                     yaxis = list(title = "Weight"))

plot_ltr_8 <- plot_ly(df_coef_ltr_8, x = ~name, y = ~weight, type = "bar") %>%
              layout(shapes = list( hline(.1))) %>%
              layout(title = "LTR most important predictors - 0.8",
                     xaxis = list(title = "Predictors"),
                     yaxis = list(title = "Weight"))

#################################### LTR ENDS #################################### 

#################################### NRS STARTS #################################### 
nrs_model_7 <- lm(nrs ~ .,df_nrs_model_7)
nrs_model_8 <- lm(nrs ~ .,df_nrs_model_8)

summary(nrs_model_7)
summary(nrs_model_8)

#based on the summary lets remove the predictors with no significance for each model
rm_predictors_nrs_7 <- c(2,10,14:17)
rm_predictors_nrs_8 <- c(2,9,17:20)


df_nrs_model_7_significate <- df_nrs_model_7[,-rm_predictors_nrs_7]
df_nrs_model_8_significate <- df_nrs_model_8[,-rm_predictors_nrs_8]

#we create the new models
lm_nrs_7 <- lm(nrs ~ .,df_nrs_model_7_significate)
lm_nrs_8 <- lm(nrs ~ .,df_nrs_model_8_significate)

summary(lm_nrs_7)
summary(lm_nrs_8)

#now we look at each variable's weight 
sum_tot_7 <- sum(abs(lm_nrs_7$coefficients[-1]))
sum_tot_8 <- sum(abs(lm_nrs_8$coefficients[-1]))

coef_nrs_7 <- abs(lm_nrs_7$coefficients[-1])/sum_tot_7 
coef_nrs_8 <- abs(lm_nrs_8$coefficients[-1])/sum_tot_8

#ordering values to plot them
df_coef_nrs_7 <- data.frame(name=names(coef_nrs_7), 
                            weight=as.numeric(coef_nrs_7), 
                            stringsAsFactors = FALSE)

df_coef_nrs_8 <- data.frame(name=names(coef_nrs_8), 
                            weight=as.numeric(coef_nrs_8), 
                            stringsAsFactors = FALSE)
#
df_coef_nrs_7$name <- factor(df_coef_nrs_7$name, 
                             levels = unique(df_coef_nrs_7$name)[order(df_coef_nrs_7$weight, 
                                                                       decreasing = TRUE)])
df_coef_nrs_8$name <- factor(df_coef_nrs_8$name, 
                             levels = unique(df_coef_nrs_8$name)[order(df_coef_nrs_8$weight, 
                                                                       decreasing = TRUE)])
#
plot_nrs_7 <- plot_ly(df_coef_nrs_7, x = ~name, y = ~weight, type = "bar") %>%
                layout(shapes = list( hline(.1))) %>%
                layout(title = "NRS most important predictors - 0.7",
                     xaxis = list(title = "Predictors"),
                     yaxis = list(title = "Weight"))

plot_nrs_8 <- plot_ly(df_coef_nrs_8, x = ~name, y = ~weight, type = "bar") %>%
                layout(shapes = list( hline(.1))) %>%
                layout(title = "NRS most important predictors - 0.8",
                      xaxis = list(title = "Predictors"),
                      yaxis = list(title = "Weight"))

#################################### NRS ENDS #################################### 

#################################### SAT STARTS #################################### 
sat_model_7 <- lm(sat ~ .,df_sat_model_7)
sat_model_8 <- lm(sat ~ .,df_sat_model_8)

summary(sat_model_7)
summary(sat_model_8)

#based on the summary lets remove the predictors with no significance for each model
rm_predictors_sat_7 <- c(8:9,14,16:17,19)
rm_predictors_sat_8 <- c(2,12,17,19:20,22)


df_sat_model_7_significate <- df_sat_model_7[,-rm_predictors_sat_7]
df_sat_model_8_significate <- df_sat_model_8[,-rm_predictors_sat_8]

#we create the new models
lm_sat_7 <- lm(sat ~ .,df_sat_model_7_significate)
lm_sat_8 <- lm(sat ~ .,df_sat_model_8_significate)

summary(lm_sat_7)
summary(lm_sat_8)

#now we look at each variable's weight 
sum_tot_7 <- sum(abs(lm_sat_7$coefficients[-1]))
sum_tot_8 <- sum(abs(lm_sat_8$coefficients[-1]))

coef_sat_7 <- abs(lm_sat_7$coefficients[-1])/sum_tot_7 
coef_sat_8 <- abs(lm_sat_8$coefficients[-1])/sum_tot_8

#ordering values to plot them
df_coef_sat_7 <- data.frame(name=names(coef_sat_7), 
                            weight=as.numeric(coef_sat_7), 
                            stringsAsFactors = FALSE)

df_coef_sat_8 <- data.frame(name=names(coef_sat_8), 
                            weight=as.numeric(coef_sat_8), 
                            stringsAsFactors = FALSE)
#
df_coef_sat_7$name <- factor(df_coef_sat_7$name, 
                             levels = unique(df_coef_sat_7$name)[order(df_coef_sat_7$weight, 
                                                                       decreasing = TRUE)])
df_coef_sat_8$name <- factor(df_coef_sat_8$name, 
                             levels = unique(df_coef_sat_8$name)[order(df_coef_sat_8$weight, 
                                                                       decreasing = TRUE)])
#
plot_sat_7 <- plot_ly(df_coef_sat_7, x = ~name, y = ~weight, type = "bar") %>%
              layout(shapes = list( hline(.1))) %>%
              layout(title = "SAT most important predictors - 0.7",
                     xaxis = list(title = "Predictors"),
                     yaxis = list(title = "Weight"))

plot_sat_8 <- plot_ly(df_coef_sat_8, x = ~name, y = ~weight, type = "bar") %>%
              layout(shapes = list( hline(.1))) %>%
              layout(title = "SAT most important predictors - 0.8",
                     xaxis = list(title = "Predictors"),
                     yaxis = list(title = "Weight"))

#################################### SAT ENDS #################################### 

