#Propensity score #################################
#1. treatment ~ disburse_iss
#first attempt - potential problem, using outcomes?
mylogit<- stan_glm(treatment ~ disburse_iss
, data = phase1)
#take a look at the odds
prop.score <- apply(rstanarm::posterior_linpred(mylogit,type="link"),2,mean)
phase1$prop.score = prop.score
control = phase1 %>% filter(treatment == 0)
treat = phase1 %>% filter(treatment == 1)
hist(control$prop.score,col=rgb(0,0,1,alpha=0.7),main="Propensity Scores", xlab="Scores", breaks=20, xlim=c(0,1.5))
hist(treat$prop.score, col=rgb(0,1,0,alpha=0.7),add=T, breaks=20)
legend('topright',col=c("blue","green"), lty=1:1, legend=c('control','treatment'))
#I really can't work with this freq graph, onto the next one
#seems like there's some but not much overlap

################################################
#2. treatment ~ fullvacc + disburse_iss
mylogit2<- stan_glm(treatment ~ disburse_iss + fullvacc, data = phase1)
prop.score2 <- apply(rstanarm::posterior_linpred(mylogit2,type="link"),2,mean)
phase1$prop.score2 = prop.score2
control = phase1 %>% filter(treatment == 0)
treat = phase1 %>% filter(treatment == 1)
hist(control$prop.score2,col=rgb(0,0,1,alpha=0.7),main="Propensity Scores", xlab="Scores", breaks=40, xlim=c(0.4,1.1))
hist(treat$prop.score2, col=rgb(0,1,0,alpha=0.7),add=T, breaks=40)
legend('topright',col=c("blue","green"), lty=1:1, legend=c('control','treatment'))

#same thing,another phase?
##################################################################################################################################3. treatment ~ hepb_net+hib_net+dpt_net+mcv_net
#mylogit3<- stan_glm(treatment ~ inc + region , data = phase1)
#mylogit3<- stan_glm(treatment ~ inc + region +  iss_todate, data = phase1)
#mylogit3<- stan_glm(treatment ~ malnourished_who + wasted_who + u5mr_dhs + u5mr + imr_dhs + imr , data = phase1)
#mylogit3<- stan_glm(treatment ~ malnourished_who + wasted_who, data = phase1)
phase1[is.na(phase1)] = 0
mylogit3<- stan_glm(treatment ~ hepb_net+hib_net+dpt_net+mcv_net, data = phase1)
prop.score3 <- apply(rstanarm::posterior_linpred(mylogit3,type="link"),2,mean)
phase1$prop.score3 = prop.score3
control = phase1 %>% filter(treatment == 0)
treat = phase1 %>% filter(treatment == 1)
hist(treat$prop.score3,col=rgb(0,1,0,alpha=0.7),main="Propensity Scores", xlab="Scores", breaks=20, xlim=c(0,2.3))
hist(control$prop.score3, col=rgb(0,0,1,alpha=0.7),add=T, breaks=20,lty=1:1, legend=c('control','treatment'))

#there is a good amount of overlap between 0 and 0.4 propensity scores

################################################################################
# Nearest Neighbor Matching W/0 Replacement
nearest_neighbor_match =arm::matching(z = phase1$treatment,prop.score3,replace = FALSE)
data_1b = phase1[nearest_neighbor_match$matched,] %>%
  mutate(weight = 1)
data_2b = phase1[!nearest_neighbor_match$matched,] %>%
  mutate(weight = 0)
dat_weight = rbind(data_1b,data_2b)

weight = dat_weight$weight

relevant_names = dat_weight %>% select(pct_govimm,pct_govvax,pop_5,pop_1,pop,lgni,u5mr,u5mr_dhs,imr_dhs,imr,malnourished_dhs,malnourished_dhs,wasted_who,I)
cov = names(relevant_names)

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
 
  }
  
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
is_binary = function(data.frame){
  if(length(unique(data.frame))>2){
    return(FALSE)
  }
  return(TRUE)
}
library(Hmisc)
bal = balance(cov, phase1, weight, dat_weight)

#checking for overlap
#hopefully using function I built earlier

################################################################################
#3. 
mylogit4<- stan_glm(treatment ~inc + region + malnourished_who + wasted_who + u5mr_dhs + u5mr + imr_dhs + imr , data = phase1)
prop.score4 <- apply(rstanarm::posterior_linpred(mylogit4,type="link"),2,mean)
phase1$prop.score4 = prop.score4
control = phase1 %>% filter(treatment == 0)
treat = phase1 %>% filter(treatment == 1)
hist(control$prop.score4,col=rgb(0,0,1,alpha=0.5),main="Propensity Scores", xlab="Scores", breaks=20, xlim=c(-0.2,1.05))
hist(treat$prop.score4, col=rgb(0,1,0,alpha=0.5),add=T, breaks=20,lty=1:1, legend=c('control','treatment'))

nearest_neighbor_match =arm::matching(z = phase1$treatment,prop.score4,replace = FALSE)
data_1b = phase1[nearest_neighbor_match$matched,] %>%
  mutate(weight = 1)
data_2b = phase1[!nearest_neighbor_match$matched,] %>%
  mutate(weight = 0)
dat_weight = rbind(data_1b,data_2b)

weight = dat_weight$weight

relevant_names = dat_weight %>% select(pct_govimm,pct_govvax,pop_5,pop_1,pop,lgni,u5mr,u5mr_dhs,imr_dhs,imr,malnourished_dhs,malnourished_dhs,wasted_who,I)
cov = names(relevant_names)
bal = balance(cov, phase1, weight, dat_weight)


################################################################################
#4. treatment ~inc + region + malnourished_who + wasted_who + u5mr_dhs + u5mr + imr_dhs + imr  + pop
mylogit5<- stan_glm(treatment ~inc + region + malnourished_who + wasted_who + u5mr_dhs + u5mr + imr_dhs + imr +pop, data = phase1)
prop.score5 <- apply(rstanarm::posterior_linpred(mylogit5,type="link"),2,mean)
phase1$prop.score5 = prop.score5
control = phase1 %>% filter(treatment == 0)
treat = phase1 %>% filter(treatment == 1)
hist(control$prop.score5,col=rgb(0,0,1,alpha=0.5),main="Propensity Scores", xlab="Scores", breaks=20, xlim=c(-0.2,1.05))
hist(treat$prop.score5, col=rgb(0,1,0,alpha=0.5),add=T, breaks=20,lty=1:1, legend=c('control','treatment'))

nearest_neighbor_match =arm::matching(z = phase1$treatment,prop.score5,replace = FALSE)
data_1b = phase1[nearest_neighbor_match$matched,] %>%
  mutate(weight = 1)
data_2b = phase1[!nearest_neighbor_match$matched,] %>%
  mutate(weight = 0)
dat_weight = rbind(data_1b,data_2b)

weight = dat_weight$weight

relevant_names = dat_weight %>% select(pct_govimm,pct_govvax,pop_5,pop_1,pop,lgni,u5mr,u5mr_dhs,imr_dhs,imr,malnourished_dhs,malnourished_dhs,wasted_who,I)
cov = names(relevant_names)
bal = balance(cov, phase1, weight, dat_weight)


