is_binary = function(data.frame){
  if(length(unique(data.frame))>2){
    return(FALSE)
  }
  return(TRUE)
}

balance.factor = function(cov,dat,weight,dat_weight){
  unmatched_treat = filter(dat_weight, treatment == 1)
  unmatched_control = filter(dat_weight,  treatment == 0)
  matched_treat = filter(dat_weight,  treatment == 1, weight >0)
  matched_control = filter(dat_weight, treatment == 0,weight >=1)
  #------------ means ----------------------------------------------
  mean_table = replicate(4*length(cov),0)
  dim(mean_table) = c(length(levels(cov)),4)
  colnames(mean_table) =c("mn1","mn0","mn1.m","mn0.m")
  row.names(mean_table) = levels(cov)
  #-----------mean diff -------------------------------#
  mean_diff = replicate(2*length(levels(cov)),0)
  dim(mean_diff) = c(length(levels(cov)),2)
  colnames(mean_diff) =c("diff","diff.m")
  row.names(mean_diff) = levels(cov)
  # ------------------------------------------ SD RAtio!! ----------------------------------- #
  sd_ratio = replicate(2*length(levels(cov)),0)
  dim(sd_ratio) = c(length(levels(cov)),2)
  colnames(sd_ratio) =c("ratio","ratio.m")
  row.names(sd_ratio) = levels(cov)
  #means!
  length(cov)
  weights = matched_control$weight
  for(i in 1:length((cov)){
    for(j in 1:length(levels(cov)))
      covar = cov[i]
      ### ----------------------------------------------- All the Means ---------- #
      mean_table[i,1] = mean(unmatched_treat[[covar]])
      mean_table[i,2] = mean(unmatched_control[[covar]])
      mean_table[i,3] = mean(matched_treat[[covar]])
      mean_table[i,4] = sum((matched_control[[covar]])*weights) / sum(weights)
      # ######-------------------------------------------------------- Mean Diff ------------- ##
      
      if(is_binary(unmatched_control[[covar]])){
        mean_diff[i,1] = mean_table[i,1] - mean_table[i,2]
      }else{
        std_dev_a = sd(unmatched_treat[[covar]])
        mean_diff[i,1] =  (mean_table[i,1] -mean_table[i,2])   / std_dev_a
      }
      if(is_binary(matched_control[[covar]])){
        mean_diff[i,2] = mean_table[i,3] - mean_table[i,4]
      }else{
        std_dev_b = sd(matched_treat[[covar]])
        mean_diff[i,2] = (mean_table[i,3] - mean_table[i,4])/ std_dev_b
      }
      ###---------------------------------------------------------- Ratio of Std. dev. -------- #
      
      #unmatched
      if(!is_binary(unmatched_control[[covar]])){
        sd_ratio[i,1] = sd(unmatched_control[[covar]]) / sd(unmatched_treat[[covar]])
        sd_ratio[i,2] = sqrt(wtd.var((matched_control[[covar]]))) / sd(matched_treat[[covar]])
      }else{
        sd_ratio[i,1] = 0
        sd_ratio[i,2] = 0
      }
    }
  }
  mean_table = round(mean_table,3)
  mean_diff = round(mean_diff,3)
  sd_ratio = round(sd_ratio,3)
  
  final_table = cbind(mean_table,mean_diff, sd_ratio)
  return(final_table)
  
}





balance = function(cov, dat,weight,dat_weight){
  #set up subsets of data
  unmatched_treat = filter(dat_weight, treatment == 1)
  unmatched_control = filter(dat_weight,  treatment == 0)
  matched_treat = filter(dat_weight,  treatment == 1, weight >0)
  matched_control = filter(dat_weight, treatment == 0,weight >=1)
  #--------------------------------------------Means! -----------------------------------------#
  #set up table
  mean_table = replicate(4*length(cov),0)
  dim(mean_table) = c(length(cov),4)
  colnames(mean_table) =c("mn1","mn0","mn1.m","mn0.m")
  row.names(mean_table) = cov
  #--------------------------------------------------------  Mean Difference! ----------------------------------- #
  #Unmatched mean difference (standardized for continuous variables, not standardized for binary variables)
  mean_diff = replicate(2*length(cov),0)
  dim(mean_diff) = c(length(cov),2)
  colnames(mean_diff) =c("diff","diff.m")
  row.names(mean_diff) = cov
  # ------------------------------------------ SD RAtio!! ----------------------------------- #
  sd_ratio = replicate(2*length(cov),0)
  dim(sd_ratio) = c(length(cov),2)
  colnames(sd_ratio) =c("ratio","ratio.m")
  row.names(sd_ratio) = cov
  #means!
  length(cov)
  weights = matched_control$weight
  for(i in 1:length(cov)){
    covar = cov[i]
    ### ----------------------------------------------- All the Means ---------- #
    mean_table[i,1] = mean(unmatched_treat[[covar]])
    mean_table[i,2] = mean(unmatched_control[[covar]])
    mean_table[i,3] = mean(matched_treat[[covar]])
    mean_table[i,4] = sum((matched_control[[covar]])*weights) / sum(weights)
    # ######-------------------------------------------------------- Mean Diff ------------- ##
    
    if(is_binary(unmatched_control[[covar]])){
      mean_diff[i,1] = mean_table[i,1] - mean_table[i,2]
    }else{
      std_dev_a = sd(unmatched_treat[[covar]])
      mean_diff[i,1] =  (mean_table[i,1] -mean_table[i,2])   / std_dev_a
    }
    if(is_binary(matched_control[[covar]])){
      mean_diff[i,2] = mean_table[i,3] - mean_table[i,4]
    }else{
      std_dev_b = sd(matched_treat[[covar]])
      mean_diff[i,2] = (mean_table[i,3] - mean_table[i,4])/ std_dev_b
    }
    ###---------------------------------------------------------- Ratio of Std. dev. -------- #
    
    #unmatched
    if(!is_binary(unmatched_control[[covar]])){
      sd_ratio[i,1] = sd(unmatched_control[[covar]]) / sd(unmatched_treat[[covar]])
      sd_ratio[i,2] = sqrt(wtd.var((matched_control[[covar]]))) / sd(matched_treat[[covar]])
    }else{
      sd_ratio[i,1] = 0
      sd_ratio[i,2] = 0
    }
    
  }
  mean_table = round(mean_table,3)
  mean_diff = round(mean_diff,3)
  sd_ratio = round(sd_ratio,3)
  
  final_table = cbind(mean_table,mean_diff, sd_ratio)
  return(final_table)
}
