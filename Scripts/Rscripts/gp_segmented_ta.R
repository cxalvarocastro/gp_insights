#we load the functions we need
source("C:/Users/alvar/OneDrive/Escritorio/gp_insights/Scripts/Rscripts/gp_ta_functions.R")

#loading libraries
library(tm)
library(wordcloud)
library(textclean)
library(svDialogs)
library(SnowballC)
library(dplyr)

#we load the data sets
a <- read.csv("C:/Users/alvar/OneDrive/Escritorio/GP/Bases/gp_export_insights.csv",
              sep = "|", 
              encoding = "UTF-8")
#rename them for shorter names
a <- a %>% filter(!is.na(gp_reason_travel_ennum)) %>%
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
a  <- a  %>% filter(!gp_marca_insights %in% c("FIESTA INN LOFT","LIVE AQUA"))

brands <- as.character(unique( a$gp_marca_insights))
purpose <- as.character(unique( a$purpose_of_stay))
pos <- purpose[-3]

b <- read.csv("C:/Users/alvar/Downloads/gp_export_insights.csv",
              sep = "|", 
              encoding = "UTF-8")

#c ic the df with the comments added
c <- merge(a,b,by="surveyid",all.x=T)
c <- data.frame(ltr_com=c$gp_ltr_com,ltr=c$gp_ltr_enum.x,nrs=c$gp_nrs.x,
                osat=c$gp_overall_satisfaction_q.x,marca=c$gp_marca_insights.x,
                pos=c$purpose_of_stay)

#we define the list of word we will exclude
exclude <- c("por","que","del","muy","las","la","el","los","a","ante","bajo","con","de",
             "desde","durante","en", "entre", "excepto", "hasta","mediante","para",
             "salvo","segun","sin","sobre","tras","buen","buena","un","una","bien",
             "fue","son","esta","sus","bueno","mas","más","está","nos","fue","unos","uno",
             "dos","tres","siempre","mis","pero","porque","como","algo","mucho","tengo",
             "este","esté","hace","vez","había","estaba","tiene","buenas","buenos","hay",
             "cuando","todo","toda","todos","todas","for","and","one","two","three","mal",
             "great","otro","otros","otra","otras","tener","tenía","solo","les","four","ten",
             "hundred","five","serviciomuy","tienen","mala","mal","nunca","falta","era","dia",
             "día","noche","sucio","sucia","mucho","mucha","muchos","muchas","sentir",
             "twenty","como","cual","beautiful","pues","hacer","mejor","get")

#we define the index of comments we want to considere
l <- 3000

#LTR Brand
createCleanTDM_ngram_ltr_brand_pro <- list()
createCleanTDM_ngram_ltr_brand_det <- list()
for(i in brands){
  df_det <- c %>% filter(ltr <= 6 & marca==i) %>% select(ltr_com)
  df_pro <- c %>% filter(ltr>=9 & marca==i) %>% select(ltr_com)
  
  if(l<length(df_det$ltr_com)){
    df_det <- sample(df_det$ltr_com,l)
  }
  if(l<length(df_pro$ltr_com)){
    df_pro <- sample(df_pro$ltr_com,l)
    
  }
  
  
  # Create a comparison TDM, comparing Detractors vs Promoters, setting the column names to "Detractor" and "Promoter", and excluding the words "basf" and "customer"
  createCleanTDM_ngram_ltr_brand_pro[[i]] <- createCleanTDM_ngram(
    df_pro, 
    excludeWords = exclude)
  
  createCleanTDM_ngram_ltr_brand_det[[i]] <- createCleanTDM_ngram(
    df_det, 
    excludeWords = exclude)
  
  
}


#LTR POS
createCleanTDM_ngram_ltr_pos_pro <- list()
createCleanTDM_ngram_ltr_pos_det<- list()
for(i in pos){
  df_det <- c %>% filter(ltr <= 6 & pos==i) %>% select(ltr_com)
  df_pro <- c %>% filter(ltr>=9 & pos==i) %>% select(ltr_com)
  
  if(l<length(df_det$ltr_com)){
    df_det <- sample(df_det$ltr_com,l)
  }
  if(l<length(df_pro$ltr_com)){
    df_pro <- sample(df_pro$ltr_com,l)
    
  }
  
  
  # Create a comparison TDM, comparing Detractors vs Promoters, setting the column names to "Detractor" and "Promoter", and excluding the words "basf" and "customer"
  createCleanTDM_ngram_ltr_pos_pro[[i]] <- createCleanTDM_ngram(
    df_pro, 
    excludeWords = exclude)
  
  createCleanTDM_ngram_ltr_pos_det[[i]] <- createCleanTDM_ngram(
    df_det, 
    excludeWords = exclude)
  
}


##general case
df_det <- c %>% filter(ltr <= 6) %>% select(ltr_com)
df_pro <- c %>% filter(ltr>=9) %>% select(ltr_com)

df_det <- sample(df_det$ltr_com,3000)
df_pro <- sample(df_pro$ltr_com,3000)


createCleanTDM_ngram_gral_det <- createCleanTDM_ngram(
  df_det,
  excludeWords = exclude)

createCleanTDM_ngram_gral_pro <- createCleanTDM_ngram(
  df_pro,
  excludeWords = exclude)
