#Lets load the DFs we need for the models
source("C:/Users/alvar/OneDrive/Escritorio/gp_insights/Scripts/Rscripts/gp_segmented_cleaning_up.R")

#libraries
library(plotly)



##
brands <- names(list_dfs_ltr_brand)
pos <- names(list_dfs_ltr_pos)


#################################### BRAND LTR STARTS #################################### 
list_lm_ltr_brand <- list()
for(i in brands){
  list_lm_ltr_brand[[i]] <- lm(ltr ~ .,list_dfs_ltr_brand[[i]])
}

## Lets generate the list of variable that we wont considere
list_lm_ltr_brand_ex <- list()
list_lm_ltr_brand_ex[[brands[1]]] <- c(2,7,9:10,12,14,15,24)
list_lm_ltr_brand_ex[[brands[2]]] <- c(2,3,5,10,12,15:21)
list_lm_ltr_brand_ex[[brands[3]]] <- c(2,12,19:23)
list_lm_ltr_brand_ex[[brands[4]]] <- c(2,5:6,10,15,19:20,23:25,27:29)
list_lm_ltr_brand_ex[[brands[5]]] <- c(2,3,5,8:9,12:13,18,21,22,27:31)
list_lm_ltr_brand_ex[[brands[6]]] <- c(2,5,8:9,17:19,21,25:27,29:57)
list_lm_ltr_brand_ex[[brands[7]]] <- c(2:10,12,15:19,22,26:32)
list_lm_ltr_brand_ex[[brands[8]]] <- c(5:9,12:15,19:20,23:24)
list_lm_ltr_brand_ex[[brands[9]]] <- c(2:20,22:23,27:32)
list_lm_ltr_brand_ex[[brands[10]]] <- c(2,5,8:9,11:15,19:24)

## lets remove these variables 
list_dfs_ltr_brand_sig <- list()
for(i in brands){
  list_dfs_ltr_brand_sig[[i]] <- list_dfs_ltr_brand[[i]][,-list_lm_ltr_brand_ex[[i]]]
} 

##we run the models again
list_lm_ltr_brand_sig <- list()
summ_list_lm_ltr_brand_sig <- list()
for(i in brands){
  list_lm_ltr_brand_sig[[i]] <- lm(ltr ~ .,list_dfs_ltr_brand_sig[[i]])
  summ_list_lm_ltr_brand_sig[[i]] <- summary(list_lm_ltr_brand_sig[[i]])
}

#now we look at the sum of impacts 
list_sum_tot_ltr <- list()
for(i in brands){
  list_sum_tot_ltr[[i]] <- sum(abs(list_lm_ltr_brand_sig[[i]]$coefficients[-1]))
}

#now we look at each variable's weight 
list_weight_ltr <- list()
for(i in brands){
  list_weight_ltr[[i]] <- abs(list_lm_ltr_brand_sig[[i]]$coefficients[-1])/list_sum_tot_ltr[[i]] 
}

#ordering values to plot them
list_values_to_plot <- list()
for(i in brands){
  list_values_to_plot[[i]] <- data.frame(name=names(list_weight_ltr[[i]]), 
                                         weight=as.numeric(list_weight_ltr[[i]]), 
                                         stringsAsFactors = FALSE) 
  list_values_to_plot[[i]]$name <- factor(list_values_to_plot[[i]]$name, 
                               levels = unique(list_values_to_plot[[i]]$name)[order(list_values_to_plot[[i]]$weight, 
                                                                         decreasing = TRUE)])
  
}

##finally we generate the bar plots
list_plots_ltr_brand <- list()
for(i in brands){
  
  list_plots_ltr_brand[[i]] <- plot_ly(list_values_to_plot[[i]], x = ~name, y = ~weight, type = "bar") %>%
    
    layout(title = paste("LTR most important predictors -",i),
           xaxis = list(title = "Predictors"),
           yaxis = list(title = "Weight"))
  
}

#################################### BRAND LTR ENDS #################################### 


#################################### BRAND NRS STARTS #################################### 
list_lm_nrs_brand <- list()
for(i in brands){
  list_lm_nrs_brand[[i]] <- lm(nrs ~ .,list_dfs_nrs_brand[[i]])
}

## Lets generate the list of variable that we wont considere
list_lm_nrs_brand_ex <- list()
list_lm_nrs_brand_ex[[brands[1]]] <- c(2,10,12,13:15,18:23)
list_lm_nrs_brand_ex[[brands[2]]] <- c(2:3,5,8,10,12,15:21)
list_lm_nrs_brand_ex[[brands[3]]] <- c(2,11,22:23)
list_lm_nrs_brand_ex[[brands[4]]] <- c(2,6,10,14:16,20,23:27,29)
list_lm_nrs_brand_ex[[brands[5]]] <- c(2,3,8:9,12:13,18,21,22,26:28,31)
list_lm_nrs_brand_ex[[brands[6]]] <- c(2,5:6,17:22,25:57)
list_lm_nrs_brand_ex[[brands[7]]] <- c(2:7,9,12,15:20,22,26:32)
list_lm_nrs_brand_ex[[brands[8]]] <- c(2,6,8:10,14:15,19:24)
list_lm_nrs_brand_ex[[brands[9]]] <- c(2:3,5:8,10,12:15,17:21,23,25,27,28,29:32)
list_lm_nrs_brand_ex[[brands[10]]] <- c(2,4,5,8:9,11:15,19:24)

## lets remove these variables 
list_dfs_nrs_brand_sig <- list()
for(i in brands){
  list_dfs_nrs_brand_sig[[i]] <- list_dfs_nrs_brand[[i]][,-list_lm_nrs_brand_ex[[i]]]
} 

##we run the models again
list_lm_nrs_brand_sig <- list()
summ_list_lm_nrs_brand_sig <- list()
for(i in brands){
  list_lm_nrs_brand_sig[[i]] <- lm(nrs ~ .,list_dfs_nrs_brand_sig[[i]])
  summ_list_lm_nrs_brand_sig[[i]] <- summary(list_lm_nrs_brand_sig[[i]])
}

#now we look at the sum of impacts 
list_sum_tot_nrs <- list()
for(i in brands){
  list_sum_tot_nrs[[i]] <- sum(abs(list_lm_nrs_brand_sig[[i]]$coefficients[-1]))
}

#now we look at each variable's weight 
list_weight_nrs <- list()
for(i in brands){
  list_weight_nrs[[i]] <- abs(list_lm_nrs_brand_sig[[i]]$coefficients[-1])/list_sum_tot_nrs[[i]] 
}

#ordering values to plot them
list_values_to_plot <- list()
for(i in brands){
  list_values_to_plot[[i]] <- data.frame(name=names(list_weight_nrs[[i]]), 
                                         weight=as.numeric(list_weight_nrs[[i]]), 
                                         stringsAsFactors = FALSE) 
  list_values_to_plot[[i]]$name <- factor(list_values_to_plot[[i]]$name, 
                                          levels = unique(list_values_to_plot[[i]]$name)[order(list_values_to_plot[[i]]$weight, 
                                                                                               decreasing = TRUE)])
  
}

##finally we generate the bar plots
list_plots_nrs_brand <- list()
for(i in brands){
  
  list_plots_nrs_brand[[i]] <- plot_ly(list_values_to_plot[[i]], x = ~name, y = ~weight, type = "bar") %>%
    
    layout(title = paste("NRS most important predictors -",i),
           xaxis = list(title = "Predictors"),
           yaxis = list(title = "Weight"))
  
}
#################################### BRAND NRS ENDS #################################### 


#################################### BRAND SAT STARTS #################################### 
list_lm_sat_brand <- list()
for(i in brands){
  list_lm_sat_brand[[i]] <- lm(sat ~ .,list_dfs_sat_brand[[i]])
}

## Lets generate the list of variable that we wont considere
list_lm_sat_brand_ex <- list()
list_lm_sat_brand_ex[[brands[1]]] <- c(2,5,9,12,14:15,24)
list_lm_sat_brand_ex[[brands[2]]] <- c(2,3,5,10,12,15:21)
list_lm_sat_brand_ex[[brands[3]]] <- c(2,12,19:21,23)
list_lm_sat_brand_ex[[brands[4]]] <- c(2,5:6,15,23,25,28:29)
list_lm_sat_brand_ex[[brands[5]]] <- c(2,5,9,12:13,18,21,22,26,28:31)
list_lm_sat_brand_ex[[brands[6]]] <- c(2,5,9,17:19,21,25:57)
list_lm_sat_brand_ex[[brands[7]]] <- c(2,4:7,9,12,15:19,26:32)
list_lm_sat_brand_ex[[brands[8]]] <- c(3:4,8,13:15,19:20,23:24)
list_lm_sat_brand_ex[[brands[9]]] <- c(2:8,10,12:21,22,26:32)
list_lm_sat_brand_ex[[brands[10]]] <- c(2,4,5,8:9,11:15,19:24)

## lets remove these variables 
list_dfs_sat_brand_sig <- list()
for(i in brands){
  list_dfs_sat_brand_sig[[i]] <- list_dfs_sat_brand[[i]][,-list_lm_sat_brand_ex[[i]]]
} 

##we run the models again
list_lm_sat_brand_sig <- list()
summ_list_lm_sat_brand_sig <- list()
for(i in brands){
  list_lm_sat_brand_sig[[i]] <- lm(sat ~ .,list_dfs_sat_brand_sig[[i]])
  summ_list_lm_sat_brand_sig[[i]] <- summary(list_lm_sat_brand_sig[[i]])
}

#now we look at the sum of impacts 
list_sum_tot_sat <- list()
for(i in brands){
  list_sum_tot_sat[[i]] <- sum(abs(list_lm_sat_brand_sig[[i]]$coefficients[-1]))
}

#now we look at each variable's weight 
list_weight_sat <- list()
for(i in brands){
  list_weight_sat[[i]] <- abs(list_lm_sat_brand_sig[[i]]$coefficients[-1])/list_sum_tot_sat[[i]] 
}

#ordering values to plot them
list_values_to_plot <- list()
for(i in brands){
  list_values_to_plot[[i]] <- data.frame(name=names(list_weight_sat[[i]]), 
                                         weight=as.numeric(list_weight_sat[[i]]), 
                                         stringsAsFactors = FALSE) 
  list_values_to_plot[[i]]$name <- factor(list_values_to_plot[[i]]$name, 
                                          levels = unique(list_values_to_plot[[i]]$name)[order(list_values_to_plot[[i]]$weight, 
                                                                                               decreasing = TRUE)])
  
}

##finally we generate the bar plots
list_plots_sat_brand <- list()
for(i in brands){
  
  list_plots_sat_brand[[i]] <- plot_ly(list_values_to_plot[[i]], x = ~name, y = ~weight, type = "bar") %>%
    
    layout(title = paste("OSat most important predictors -",i),
           xaxis = list(title = "Predictors"),
           yaxis = list(title = "Weight"))
  
}
#################################### BRAND SAT ENDS #################################### 



#################################### POS LTR STARTS #################################### 
list_lm_ltr_pos <- list()
for(i in pos){
  list_lm_ltr_pos[[i]] <- lm(ltr ~ .,list_dfs_ltr_pos[[i]])
}

## Lets generate the list of variable that we wont considere
list_lm_ltr_pos_ex <- list()
list_lm_ltr_pos_ex[[pos[1]]] <- c(2,12:13,15,19)
list_lm_ltr_pos_ex[[pos[2]]] <- c(2,11,16)
list_lm_ltr_pos_ex[[pos[3]]] <- c(2,9,23)

## lets remove these variables 
list_dfs_ltr_pos_sig <- list()
for(i in pos){
  list_dfs_ltr_pos_sig[[i]] <- list_dfs_ltr_pos[[i]][,-list_lm_ltr_pos_ex[[i]]]
} 

##we run the models again
list_lm_ltr_pos_sig <- list()
summ_list_lm_ltr_pos_sig <- list()
for(i in pos){
  list_lm_ltr_pos_sig[[i]] <- lm(ltr ~ .,list_dfs_ltr_pos_sig[[i]])
  summ_list_lm_ltr_pos_sig[[i]] <- summary(list_lm_ltr_pos_sig[[i]])
}

#now we look at the sum of impacts 
list_sum_tot_ltr <- list()
for(i in pos){
  list_sum_tot_ltr[[i]] <- sum(abs(list_lm_ltr_pos_sig[[i]]$coefficients[-1]))
}

#now we look at each variable's weight 
list_weight_ltr <- list()
for(i in pos){
  list_weight_ltr[[i]] <- abs(list_lm_ltr_pos_sig[[i]]$coefficients[-1])/list_sum_tot_ltr[[i]] 
}

#ordering values to plot them
list_values_to_plot <- list()
for(i in pos){
  list_values_to_plot[[i]] <- data.frame(name=names(list_weight_ltr[[i]]), 
                                         weight=as.numeric(list_weight_ltr[[i]]), 
                                         stringsAsFactors = FALSE) 
  list_values_to_plot[[i]]$name <- factor(list_values_to_plot[[i]]$name, 
                                          levels = unique(list_values_to_plot[[i]]$name)[order(list_values_to_plot[[i]]$weight, 
                                                                                               decreasing = TRUE)])
  
}

##finally we generate the bar plots
list_plots_ltr_pos <- list()
for(i in pos){
  
  list_plots_ltr_pos[[i]] <- plot_ly(list_values_to_plot[[i]], x = ~name, y = ~weight, type = "bar") %>%
    
    layout(title = paste("LTR most important predictors -",i),
           xaxis = list(title = "Predictors"),
           yaxis = list(title = "Weight"))
  
}

#################################### POS LTR ENDS #################################### 


#################################### POS NRS STARTS #################################### 
list_lm_nrs_pos <- list()
for(i in pos){
  list_lm_nrs_pos[[i]] <- lm(nrs ~ .,list_dfs_nrs_pos[[i]])
}

## Lets generate the list of variable that we wont considere
list_lm_nrs_pos_ex <- list()
list_lm_nrs_pos_ex[[pos[1]]] <- c(2,15)
list_lm_nrs_pos_ex[[pos[2]]] <- c(2,5,10,16)
list_lm_nrs_pos_ex[[pos[3]]] <- c(2,9,12,18:23)

## lets remove these variables 
list_dfs_nrs_pos_sig <- list()
for(i in pos){
  list_dfs_nrs_pos_sig[[i]] <- list_dfs_nrs_pos[[i]][,-list_lm_nrs_pos_ex[[i]]]
} 

##we run the models again
list_lm_nrs_pos_sig <- list()
summ_list_lm_nrs_pos_sig <- list()
for(i in pos){
  list_lm_nrs_pos_sig[[i]] <- lm(nrs ~ .,list_dfs_nrs_pos_sig[[i]])
  summ_list_lm_nrs_pos_sig[[i]] <- summary(list_lm_nrs_pos_sig[[i]])
}

#now we look at the sum of impacts 
list_sum_tot_nrs <- list()
for(i in pos){
  list_sum_tot_nrs[[i]] <- sum(abs(list_lm_nrs_pos_sig[[i]]$coefficients[-1]))
}

#now we look at each variable's weight 
list_weight_nrs <- list()
for(i in pos){
  list_weight_nrs[[i]] <- abs(list_lm_nrs_pos_sig[[i]]$coefficients[-1])/list_sum_tot_nrs[[i]] 
}

#ordering values to plot them
list_values_to_plot <- list()
for(i in pos){
  list_values_to_plot[[i]] <- data.frame(name=names(list_weight_nrs[[i]]), 
                                         weight=as.numeric(list_weight_nrs[[i]]), 
                                         stringsAsFactors = FALSE) 
  list_values_to_plot[[i]]$name <- factor(list_values_to_plot[[i]]$name, 
                                          levels = unique(list_values_to_plot[[i]]$name)[order(list_values_to_plot[[i]]$weight, 
                                                                                               decreasing = TRUE)])
  
}

##finally we generate the bar plots
list_plots_nrs_pos <- list()
for(i in pos){
  
  list_plots_nrs_pos[[i]] <- plot_ly(list_values_to_plot[[i]], x = ~name, y = ~weight, type = "bar") %>%
    
    layout(title = paste("NRS most important predictors -",i),
           xaxis = list(title = "Predictors"),
           yaxis = list(title = "Weight"))
  
}

#################################### POS NRS ENDS #################################### 


#################################### POS SAT STARTS #################################### 
list_lm_sat_pos <- list()
for(i in pos){
  list_lm_sat_pos[[i]] <- lm(sat ~ .,list_dfs_sat_pos[[i]])
}

## Lets generate the list of variable that we wont considere
list_lm_sat_pos_ex <- list()
list_lm_sat_pos_ex[[pos[1]]] <- c(2,12:13,15,19)
list_lm_sat_pos_ex[[pos[2]]] <- c(2,8,10:12,16)
list_lm_sat_pos_ex[[pos[3]]] <- c(2,23)

## lets remove these variables 
list_dfs_sat_pos_sig <- list()
for(i in pos){
  list_dfs_sat_pos_sig[[i]] <- list_dfs_sat_pos[[i]][,-list_lm_sat_pos_ex[[i]]]
} 

##we run the models again
list_lm_sat_pos_sig <- list()
summ_list_lm_sat_pos_sig <- list()
for(i in pos){
  list_lm_sat_pos_sig[[i]] <- lm(sat ~ .,list_dfs_sat_pos_sig[[i]])
  summ_list_lm_sat_pos_sig[[i]] <- summary(list_lm_sat_pos_sig[[i]])
}

#now we look at the sum of impacts 
list_sum_tot_sat <- list()
for(i in pos){
  list_sum_tot_sat[[i]] <- sum(abs(list_lm_sat_pos_sig[[i]]$coefficients[-1]))
}

#now we look at each variable's weight 
list_weight_sat <- list()
for(i in pos){
  list_weight_sat[[i]] <- abs(list_lm_sat_pos_sig[[i]]$coefficients[-1])/list_sum_tot_sat[[i]] 
}

#ordering values to plot them
list_values_to_plot <- list()
for(i in pos){
  list_values_to_plot[[i]] <- data.frame(name=names(list_weight_sat[[i]]), 
                                         weight=as.numeric(list_weight_sat[[i]]), 
                                         stringsAsFactors = FALSE) 
  list_values_to_plot[[i]]$name <- factor(list_values_to_plot[[i]]$name, 
                                          levels = unique(list_values_to_plot[[i]]$name)[order(list_values_to_plot[[i]]$weight, 
                                                                                               decreasing = TRUE)])
  
}

##finally we generate the bar plots
list_plots_sat_pos <- list()
for(i in pos){
  
  list_plots_sat_pos[[i]] <- plot_ly(list_values_to_plot[[i]], x = ~name, y = ~weight, type = "bar") %>%
   
    layout(title = paste("OSat most important predictors -",i),
           xaxis = list(title = "Predictors"),
           yaxis = list(title = "Weight"))
  
}

#################################### POS SAT ENDS #################################### 


#we remove all the objects but the las 6 defined in this script to save RAM
list_no_remove <-     c("summ_list_lm_sat_pos_sig","list_plots_sat_pos",
                        "summ_list_lm_nrs_pos_sig","list_plots_nrs_pos",
                        "summ_list_lm_ltr_pos_sig","list_plots_ltr_pos",
                        "summ_list_lm_sat_brand_sig","list_plots_sat_brand",
                        "summ_list_lm_nrs_brand_sig","list_plots_nrs_brand",
                        "summ_list_lm_ltr_brand_sig","list_plots_ltr_brand",
                        "brands","pos")

rm(list=setdiff(ls(), list_no_remove))
