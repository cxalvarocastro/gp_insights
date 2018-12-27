library(dplyr)
library(caret)

#we read de data set
df <- read.csv("C:/Users/alvar/OneDrive/Escritorio/GP/Bases/gp_export_insights.csv",
               sep = "|", 
               encoding = "UTF-8",
               na.strings = c(""," "))


#we drop the records with marca in "FIESTA INN LOFT","LIVE AQUA"
df <- df %>% filter(!gp_marca_insights %in% c("FIESTA INN LOFT","LIVE AQUA"))

#we separate the numeric and categorical variables
var_enum <- df[,c(146,9,15:26,33,35,105:121,125:136)]
var_cate <- df[,c(146,27:32,34,36:104,138:139)]


var_index_enum <- character()
for(i in 2:length(colnames(var_enum))){
  
  l_all <- length(var_enum[,1])
  l_na <- length(which(is.na(var_enum[,i])))
  
  if(l_na/l_all<=.3){
    var_index_enum <- append(var_index_enum,i)
    
  }
  
}


var_index_cate <- character()
for(i in 2:length(colnames(var_cate))){
  
  l_all <- length(var_cate[,1])
  l_na <- length(which(is.na(var_cate[,i])))
  
  if(l_na/l_all<=.3){
    var_index_cate <- append(var_index_cate,i)
    
  }
  
}


#we drop the rows with more than 30% NA's
df_var_enum <- var_enum[,as.numeric(c(1,var_index_enum))]
df_var_cate <- var_cate[,as.numeric(c(1,var_index_cate))]



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
df_cleaned <- df_cleaned[,c(1,3,5,4,2,6:28)]


#Lets split the cleaned df into targets df & predictors df
df_targets <- df_cleaned[,2:4]
df_predictors<- df_cleaned[,5:28]

#we look at the correlation between the predictors
correlation <- cor(df_predictors) 
summary(correlation[upper.tri(correlation)])

#we remove the variables correlated more than .7 & .8
highlyCor_7 <- findCorrelation(correlation, cutoff = .7)
highlyCor_8 <- findCorrelation(correlation, cutoff = .8)

predictors_7 <- names(df_predictors[,highlyCor_7])
predictors_8 <- names(df_predictors[,highlyCor_8])


df_predictors_filtered_7 <- df_predictors[,-highlyCor_7]
df_predictors_filtered_8 <- df_predictors[,-highlyCor_8]

#we look at the correlation after we droped the correlated predictors
corr_7 <- cor(df_predictors_filtered_7)
corr_8 <- cor(df_predictors_filtered_8)


#lets scale all the predictors because the 0 & 1 values and the 0 to 10 ones
preProcValues_7 <- preProcess(df_predictors_filtered_7, method = c("center", "scale"))
df_transformed_7 <- predict(preProcValues_7, df_predictors_filtered_7)

preProcValues_8 <- preProcess(df_predictors_filtered_8, method = c("center", "scale"))
df_transformed_8 <- predict(preProcValues_8, df_predictors_filtered_8)

# we generate the cleaned df for each target variable
#ltr
df_ltr_model_7 <- cbind(ltr=df_targets[,1],df_transformed_7)
df_ltr_model_8 <- cbind(ltr=df_targets[,1],df_transformed_8)

#nrs
df_nrs_model_7 <- cbind(nrs=df_targets[,2],df_transformed_7)
df_nrs_model_8 <- cbind(nrs=df_targets[,2],df_transformed_8)

#sat
df_sat_model_7 <- cbind(sat=df_targets[,3],df_transformed_7)
df_sat_model_8 <- cbind(sat=df_targets[,3],df_transformed_8)

#we remove all the objects but the las 6 defined in this script to save RAM
list_no_remove <-     c("df_ltr_model_7","df_ltr_model_8",
                        "df_nrs_model_7","df_nrs_model_8",
                        "df_sat_model_7","df_sat_model_8")

rm(list=setdiff(ls(), list_no_remove))

