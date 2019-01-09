library(dplyr)
library(caret)

#we read de data set
df <- read.csv("C:/Users/alvar/OneDrive/Escritorio/GP/Bases/gp_export_insights.csv",
               sep = "|", 
               encoding = "UTF-8",
               na.strings = c(""," ","No lo usÃ©","NA"))


#we drop the reasons we dont want and rename them for shorter names
df_pos <- df %>% filter(!is.na(gp_reason_travel_ennum)) %>%
          # filter(!gp_reason_travel_ennum %in% c("Salud y atención médica",
          #                                       "Celebración especial - Cumpleaños  / Aniversario/ Luna de miel",
          #                                       "Quería conocer un nuevo lugar / Atractivos turísticos",
          #                                       "Pasar tiempo en familia/amigos/seres queridos",
          #                                       "Iba a visitar a Amigos / Familiares") 
          #        )%>%
          mutate(purpose_of_stay = case_when(
            gp_reason_travel_ennum=="Celebración especial - Cumpleaños  / Aniversario/ Luna de miel" ~ "Leisure",
            gp_reason_travel_ennum=="Quería conocer un nuevo lugar / Atractivos turísticos"~ "Leisure",
            gp_reason_travel_ennum=="Pasar tiempo en familia/amigos/seres queridos"~ "Leisure",
            gp_reason_travel_ennum=="Iba a visitar a Amigos / Familiares"~ "Leisure",
            gp_reason_travel_ennum=="Tenía un evento - Concierto / Deportivo / Graduación / Boda"~ "Leisure",
            gp_reason_travel_ennum=="Quería relajarme / Descansar"~ "Leisure",
            gp_reason_travel_ennum=="De trabajo y aproveche para descansar/ aproveche para llevar a mi familia/ aproveche para conocer el lugar"~ "Bleisure",
            gp_reason_travel_ennum=="Tenía una junta o asunto de trabajo que atender / Ayudar en la implementación de proyecto / Capacitación / Reclutar personal / Solicitud de cliente / Trabajo" ~ "Business"
                                                    ))

#we drop the records with marca in "FIESTA INN LOFT","LIVE AQUA"
df_pos <- df_pos %>% filter(!gp_marca_insights %in% c("FIESTA INN LOFT","LIVE AQUA"))

brands <- as.character(unique(df_pos$gp_marca_insights))
purpose <- as.character(unique(df_pos$purpose_of_stay))
purpose <- purpose[-4]






############################### BRAND SEGMENTATION STARTS ############################### 

#list with dfs for numeric variables and targets
list_df_targets <- list()
list_df_predictors_enum <- list()
for(k in brands){

df_tmp_vars <- df_pos %>% filter(gp_marca_insights == k)

#we separate the numeric and categorical variables
var_enum <- df_tmp_vars[,c(146,9,15:26,33,35,105:121,125:136)]
    
var_index_enum <- character()
for(i in 2:length(colnames(var_enum))){
  
  l_all <- length(var_enum[,1])
  l_na <- length(which(is.na(var_enum[,i])))
  
  if(l_na/l_all<=.3){
    var_index_enum <- append(var_index_enum,i)
  }
}

#we drop the rows with more than 30% NA's
df_var_enum <- var_enum[,as.numeric(var_index_enum)]

#lets replace the NAs with the median for numeric variables
df_inpute_enum <- preProcess(df_var_enum, method = "medianImpute")
df_var_enum_fxd <- predict(df_inpute_enum,df_var_enum)

#we separate the predictors and targets
df_targets <- df_var_enum_fxd[,c(2,3,4)]
df_predictors <- df_var_enum_fxd[,-c(2,3,4)]


list_df_targets[[k]] <- df_targets
list_df_predictors_enum[[k]] <- df_predictors


}

#list with dfs for categorical variables
list_df_predictors_cate <- list()
for(k in brands){
  
  df_tmp_vars <- df_pos %>% filter(gp_marca_insights == k)
  
  #we separate the numeric and categorical variables
  var_cate <- df_tmp_vars[,c(146,27:32,34,36:104,138:139)]
  
  var_index_cate <- character()
  for(i in 2:length(colnames(var_cate))){
    
    l_all <- length(var_cate[,1])
    l_na <- length(which(is.na(var_cate[,i])))
    
    if(l_na/l_all<=.3){
      var_index_cate <- append(var_index_cate,i)
    }
  }
  
  #we drop the rows with more than 30% NA's
  df_var_cate<- var_cate[,as.numeric(var_index_cate)]
  
  #we change Sí & Male to 1 and No & Female to 0
  df_var_cate <-lapply(df_var_cate, function(col){
    ifelse(col %in% c("Sí","Male"),
           1,
           ifelse(col %in% c("No","Female"),0,col) )
  } 
  )
  
  df_var_cate <- as.data.frame(df_var_cate)
  
  
  #lets replace the NAs with the median 
  df_inpute_cate <- preProcess(df_var_cate, method = "medianImpute")
  df_var_cate_fxd <- predict(df_inpute_cate,df_var_cate)
  
  
  list_df_predictors_cate[[k]] <- df_var_cate_fxd
  
  
}


#the code below look at the correlation and scale & centerpredictors
##
df_predictors_final_brand <- list()
##
list_dfs_scaled <- list()
for(l in brands){
  
  b <- list_df_predictors_enum[[l]]
  c <- list_df_predictors_cate[[l]]
  bc <- cbind(b,c)
  
  #we look at the correlation between the predictors
  correlation <- cor(bc)
  
  #we remove the variables correlated more than .8
  highlyCor_8 <- findCorrelation(correlation, cutoff = .8)
  ifelse(length(highlyCor_8)>0,
         df_predictors_filtered_8 <- bc[,-highlyCor_8],
         df_predictors_filtered_8 <- bc)
  ##
  df_predictors_final_brand[[l]] <- df_predictors_filtered_8
  ##
  #lets scale all the predictors
  preProcValues_8 <- preProcess(df_predictors_filtered_8, method = c("center", "scale"))
  df_predictors_8 <- predict(preProcValues_8, df_predictors_filtered_8)
  
  list_dfs_scaled[[l]] <- df_predictors_8
  
}


#Finally we generate a list joining targets, numeric predictors and categorical predictors
#and we generate these lists by target (ltr, sat, nrs)

list_dfs_ltr_brand <- list()
list_dfs_sat_brand <- list()
list_dfs_nrs_brand <- list()
for(m in brands){
  
  #we separate ecah target in different df
  a <- list_df_targets[[m]]
  a1 <- a[,c(1)]
  a2 <- a[,c(2)]
  a3 <- a[,c(3)]
  
  #we locate in b the df with all the predictors
  b <- list_dfs_scaled[[m]]
  
  
  #we generate the lists of dfs for each target
  list_dfs_ltr_brand[[m]] <- cbind(ltr=a1,b)
  list_dfs_sat_brand[[m]] <- cbind(sat=a2,b)
  list_dfs_nrs_brand[[m]] <- cbind(nrs=a3,b)
  
  
}
############################### BRAND SEGMENTATION ENDS ############################### 


############################### PURPOSE SEGMENTATION STARTS ###############################
#list with dfs for numeric variables and targets
list_df_targets <- list()
list_df_predictors_enum <- list()
for(k in purpose){
  
  df_tmp_vars <- df_pos %>% filter(purpose_of_stay == k)
  
  #we separate the numeric and categorical variables
  var_enum <- df_tmp_vars[,c(146,9,15:26,33,35,105:121,125:136)]
  
  var_index_enum <- character()
  for(i in 2:length(colnames(var_enum))){
    
    l_all <- length(var_enum[,1])
    l_na <- length(which(is.na(var_enum[,i])))
    
    if(l_na/l_all<=.3){
      var_index_enum <- append(var_index_enum,i)
    }
  }
  
  #we drop the rows with more than 30% NA's
  df_var_enum <- var_enum[,as.numeric(var_index_enum)]
  
  #lets replace the NAs with the median for numeric variables
  df_inpute_enum <- preProcess(df_var_enum, method = "medianImpute")
  df_var_enum_fxd <- predict(df_inpute_enum,df_var_enum)
  
  #we separate the predictors and targets
  df_targets <- df_var_enum_fxd[,c(2,3,4)]
  df_predictors <- df_var_enum_fxd[,-c(2,3,4)]
  
  
  list_df_targets[[k]] <- df_targets
  list_df_predictors_enum[[k]] <- df_predictors
  
  
}

#list with dfs for categorical variables
list_df_predictors_cate <- list()
for(k in purpose){
  
  df_tmp_vars <- df_pos %>% filter(purpose_of_stay == k)
  
  #we separate the numeric and categorical variables
  var_cate <- df_tmp_vars[,c(146,27:32,34,36:104,138:139)]
  
  var_index_cate <- character()
  for(i in 2:length(colnames(var_cate))){
    
    l_all <- length(var_cate[,1])
    l_na <- length(which(is.na(var_cate[,i])))
    
    if(l_na/l_all<=.3){
      var_index_cate <- append(var_index_cate,i)
    }
  }
  
  #we drop the rows with more than 30% NA's
  df_var_cate<- as.data.frame(var_cate[,as.numeric(var_index_cate)])
  
  if(length(var_index_cate)==1){
    names(df_var_cate) <- "gp_gender_yn"
  }
      
  
  
  #we change Sí & Male to 1 and No & Female to 0
  df_var_cate <-lapply(df_var_cate, function(col){
    ifelse(col %in% c("Sí","Male"),
           1,
           ifelse(col %in% c("No","Female"),0,col) )
  } 
  )
  
  df_var_cate <- as.data.frame(df_var_cate)
  
  
  #lets replace the NAs with the median 
  df_inpute_cate <- preProcess(df_var_cate, method = "medianImpute")
  df_var_cate_fxd <- predict(df_inpute_cate,df_var_cate)
  
  
  list_df_predictors_cate[[k]] <- df_var_cate_fxd
  
  
}


#the code below look at the correlation and scale & centerpredictors
##
df_predictors_final_pos <- list()
##
list_dfs_scaled <- list()
for(l in purpose){
  
  b <- list_df_predictors_enum[[l]]
  c <- list_df_predictors_cate[[l]]
  bc <- cbind(b,c)
  
  #we look at the correlation between the predictors
  correlation <- cor(bc)
  
  #we remove the variables correlated more than .8
  highlyCor_8 <- findCorrelation(correlation, cutoff = .8)
  ifelse(length(highlyCor_8)>0,
         df_predictors_filtered_8 <- bc[,-highlyCor_8],
         df_predictors_filtered_8 <- bc)
  ##
  df_predictors_final_pos[[l]] <- df_predictors_filtered_8
  ##
  #lets scale all the predictors
  preProcValues_8 <- preProcess(df_predictors_filtered_8, method = c("center", "scale"))
  df_predictors_8 <- predict(preProcValues_8, df_predictors_filtered_8)
  
  list_dfs_scaled[[l]] <- df_predictors_8
  
}


#Finally we generate a list joining targets, numeric predictors and categorical predictors
#and we generate these lists by target (ltr, sat, nrs)

list_dfs_ltr_pos <- list()
list_dfs_sat_pos <- list()
list_dfs_nrs_pos  <- list()
for(m in purpose){
  
  #we separate ecah target in different df
  a <- list_df_targets[[m]]
  a1 <- a[,c(1)]
  a2 <- a[,c(2)]
  a3 <- a[,c(3)]
  
  #we locate in b the df with all the predictors
  b <- list_dfs_scaled[[m]]
  
  
  #we generate the lists of dfs for each target
  list_dfs_ltr_pos[[m]] <- cbind(ltr=a1,b)
  list_dfs_sat_pos[[m]] <- cbind(sat=a2,b)
  list_dfs_nrs_pos[[m]] <- cbind(nrs=a3,b)
  
  
}
############################### PURPOSE SEGMENTATION ENDS ############################### 


#we remove all the objects but the las 6 defined in this script to save RAM
list_no_remove <-     c("list_dfs_ltr_brand","list_dfs_nrs_brand",
                        "list_dfs_sat_brand","list_dfs_ltr_pos",
                        "list_dfs_sat_pos","list_dfs_nrs_pos",
                        "df_predictors_final_pos","df_predictors_final_brand")

rm(list=setdiff(ls(), list_no_remove))
