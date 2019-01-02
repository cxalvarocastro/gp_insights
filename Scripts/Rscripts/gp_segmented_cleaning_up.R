library(dplyr)
library(caret)

#we read de data set
df <- read.csv("C:/Users/alvar/OneDrive/Escritorio/GP/Bases/gp_export_insights.csv",
               sep = "|", 
               encoding = "UTF-8",
               na.strings = c(""," ","No lo usé"))
#we generate a table with % for each reason
pos <- df %>% group_by(gp_reason_travel_ennum) %>% summarise(n=n()) %>% 
        mutate(pc = n*100/sum(n)) %>%
        arrange(pc)

#we drop the reasons we dont want and rename them for shorter names
df_pos <- df %>% filter(!is.na(gp_reason_travel_ennum)) %>%
          filter(!gp_reason_travel_ennum %in% c("Salud y atenci�n m�dica",
                                                "Celebraci�n especial - Cumplea�os  / Aniversario/ Luna de miel",
                                                "Quer�a conocer un nuevo lugar / Atractivos tur�sticos",
                                                "Pasar tiempo en familia/amigos/seres queridos",
                                                "Iba a visitar a Amigos / Familiares") )%>%
          mutate(purpose_of_stay = case_when(
            gp_reason_travel_ennum=="Celebraci�n especial - Cumplea�os  / Aniversario/ Luna de miel" ~ "Celebraci�n especial",
            gp_reason_travel_ennum=="Quer�a conocer un nuevo lugar / Atractivos tur�sticos"~ "Conocer lugar",
            gp_reason_travel_ennum=="Pasar tiempo en familia/amigos/seres queridos"~ "Pasar tiempo con seres queridos",
            gp_reason_travel_ennum=="Iba a visitar a Amigos / Familiares"~ "Visitar Amigos/Familia",
            gp_reason_travel_ennum=="Ten�a un evento - Concierto / Deportivo / Graduaci�n / Boda"~ "Evento",
            gp_reason_travel_ennum=="Quer�a relajarme / Descansar"~ "Relajarse/Descansar",
            gp_reason_travel_ennum=="De trabajo y aproveche para descansar/ aproveche para llevar a mi familia/ aproveche para conocer el lugar"~ "De trabajo y aprovech�",
            gp_reason_travel_ennum=="Ten�a una junta o asunto de trabajo que atender / Ayudar en la implementaci�n de proyecto / Capacitaci�n / Reclutar personal / Solicitud de cliente / Trabajo" ~ "Junta o asunto/Implementaci�n"
                                                    ))

#we drop the records with marca in "FIESTA INN LOFT","LIVE AQUA"
df_pos <- df_pos %>% filter(!gp_marca_insights %in% c("FIESTA INN LOFT","LIVE AQUA"))

brands <- as.character(unique(df_pos$gp_marca_insights))
purpose <- as.character(unique(df_pos$purpose_of_stay))

############################### BRAND SEGMENTATION STARTS ############################### 

#list with dfs for numeric variables
list_df_brand_enum <- list()
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

list_df_brand_enum[[k]] <- df_var_enum_fxd

}

#list with dfs for categorical variables
list_df_brand_cate <- list()
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
  
  #we change S� & Male to 1 and No & Female to 0
  df_var_cate <-lapply(df_var_cate, function(col){
    ifelse(col %in% c("S�","Male"),
           1,
           ifelse(col %in% c("No","Female"),0,col) )
  } 
  )
  
  df_var_cate <- as.data.frame(df_var_cate)
  
  
  #lets replace the NAs with the median 
  df_inpute_cate <- preProcess(df_var_cate, method = "medianImpute")
  df_var_cate_fxd <- predict(df_inpute_cate,df_var_cate)
  
  list_df_brand_cate[[k]] <- df_var_cate_fxd
  
}



############################### BRAND SEGMENTATION ENDS ############################### 
