library(dplyr)
library(caret)

################################################# CLEANING  STARTS #################################################

#we read de data set
df <- read.csv("C:/Users/Lenovo/Downloads/gp_export_insights.csv",
               sep = "|", 
               encoding = "UTF-8",
               na.strings = c(""," "))

#we separate the numeric and categorical variables
var_enum <- df[,c(146,9,15:26,33,35,105:121,125:136)]
var_cate <- df[,c(146,27:32,34,36:104,138:139)]

#we verify which variables have at least 70% of non NA values
var_index_enum <- as.character()
for(i in 2:length(colnames(var_enum))){

  l_all <- length(var_enum[,1])
  l_na <- length(which(is.na(var_enum[,i])))

    if(l_na/l_all<=.3){
      var_index_enum <- append(var_index_enum,i)
      
    }

}

var_index_cate <- as.character()
for(i in 2:length(colnames(var_cate))){
  
  l_all <- length(var_cate[,1])
  l_na <- length(which(is.na(var_cate[,i])))
  
  if(l_na/l_all<=.3){
    var_index_cate <- append(var_index_cate,i)
    
  }
  
}

#we drop the variables that do not match the condition
df_var_enum <- var_enum[,as.numeric(c(1,var_index_enum))]
df_var_cate <- var_cate[,as.numeric(c(1,var_index_cate))]

summary(df_var_enum)

#as we can see the variable gp_conexion_internet has "No lo usé" (did not use it) values
#we will remove the records with this value
df_var_enum <- df_var_enum %>% 
              filter(gp_conexion_internet != "No lo usé") 

#we convert this last variable to numeric
df_var_enum$gp_conexion_internet <- as.numeric(as.character(df_var_enum$gp_conexion_internet))

#lets replace the NAs with the median for numeric variables
df_inpute_enum <- preProcess(df_var_enum[,-1], method = "medianImpute")
df_var_enum_fxd <- predict(df_inpute_enum,df_var_enum)

summary(df_var_enum_fxd)
str(df_var_enum_fxd)

#we change Sí & Male to 1 and No & Female to 0
df_var_cate[,-1] <-lapply(df_var_cate[,-1], function(col){
                                            ifelse(col %in% c("Sí","Male"),
                                                   1,
                                                   ifelse(col %in% c("No","Female"),0,col) )
                                                          } 
                          )

#lets replace the NAs with the median 
df_inpute_cate <- preProcess(df_var_cate[,-1], method = "medianImpute")
df_var_cate_fxd <- predict(df_inpute_cate,df_var_cate)

#unif_01 <- round(runif(length(df_var_cate$surveyid),0,1))
#NA2Runif<- function(x){ replace(x, is.na(x), unif_01) }
#df_var_cate_fxd <- replace(df_var_cate[,-1], TRUE, lapply(df_var_cate[,-1], NA2Runif))

summary(df_var_cate_fxd)

#we make an inner join to have numeric and categorical variables in the same df
df_cleaned <- merge(df_var_enum_fxd,df_var_cate_fxd,by = "surveyid")
df_cleaned <- df_cleaned[,c(1,3,5,2,4,6:28)]


#Lets split the clenaed df into targets df & predictors df
df_targets <- df_cleaned[,2:3]
df_predictors<- df_cleaned[,4:28]

#we look at the correlation between the predictors
correlation <- cor(df_predictors) 
summary(correlation[upper.tri(correlation)])

#we remove the variables correlated more than .7
highlyCor <- findCorrelation(correlation, cutoff = .7)
df_predictors_filtered <- df_predictors[,-highlyCor]

#with the below commented code we can see the difference when removing the correlated variables
#corr2 <- cor(df_predictors_filtered)
#summary(corr2[upper.tri(corr2)])

#we verify possible linear combination
findLinearCombos(df_predictors_filtered)

#lets scale all the predictors because the 0 & 1 values and the 0 to 10 ones
#we will make the model using the scaled and non scaled predictors
preProcValues <- preProcess(df_predictors_filtered, method = c("center", "scale"))
df_transformed <- predict(preProcValues, df_predictors_filtered)

summary(df_transformed)

df_final_ltr <- cbind(ltr = df_targets[,1],df_transformed)
df_final_nrs <- cbind(nrs = df_targets[,2],df_transformed)
################################################# CLEANING  ENDS #################################################



################################################# BASE LINEAR MODEL #################################################
#lets first try the R base linear model

#we create de linear model
ltr_model <- lm(ltr ~. ,df_final_ltr)
nrs_model <- lm(nrs ~. ,df_final_nrs)


summary(nrs_model)

################################################# BASE LINEAR MODEL ENDS #################################################




