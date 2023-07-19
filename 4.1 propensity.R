# new propensity scores
#calculated on pre-gavi things

#List of possible covariates for matching:
  # Population: everyone, under 5, under 1 (in millions)
  # Under 5 mortality rate, DHS + WHO
  # Infant mortality rate, DHS + WHO
  # Malnourished (WHO): % children 2 s.d. below weight for age median
  # Wasted (WHO): % of children 2 s.d. below weight for height median
  # Country’s income (factor, with levels NA, L, LM, LM*, UM)
  # region
  # expendature of the government on vax and immunizations x
  # % of under 2's who are fully vacced x
  # GDP? -- test if it is as strong as the instrument, highly correlated with the instrument
  # per capita expenditure by gov on vax and immunizations
  # number of people stunted in growth

################################################################################
#4. treatment ~inc + malnourished_who + wasted_who + u5mr_dhs + u5mr + imr_dhs + imr  + pop
mylogit5<- stan_glm(treatment ~ave_malnourished+ ave_imr*inc + ave_lpop1 +  ave_u5mr, data = phase0)
#mylogit5<- stan_glm(treatment ~inc + region + malnourished_who + wasted_who + u5mr_dhs + u5mr + imr_dhs + imr, data = pregavi)
prop.score5 <- apply(rstanarm::posterior_linpred(mylogit5,type="link"),2,mean,replace=TRUE)
phase0$prop.score5 = prop.score5

###################### Check Overlap ################
control = phase0 %>% filter(treatment == 0)
treat = phase0 %>% filter(treatment == 1)
hist(control$prop.score5,col=rgb(0,0,1,alpha=0.5),main="Propensity Scores", xlab="Scores", xlim = c(-0.2,1))
hist(treat$prop.score5, col=rgb(0,1,0,alpha=0.5),add=T, lty=1:1, legend=c('control','treatment'))
# looks good

###################### Make Matches ################
nearest_neighbor_match =arm::matching(z = phase0$treatment,prop.score5,replace = FALSE)
data_1b = phase0[nearest_neighbor_match$matched,] %>%
  mutate(weight = 1)
data_2b = phase0[!nearest_neighbor_match$matched,] %>%
  mutate(weight = 0)
dat_weight = rbind(data_1b,data_2b)

weight = dat_weight %>% select(ccode,treatment,weight)
phase1_w = join(phase1,weight)
#phase1_w = phase1_w * phase1_w$weight
phase1_w = phase1_w[phase1_w$weight==1,]
################ Check Balance ################
relevant_names = dat_weight %>% select(ave_lpop5,ave_lpop1,ave_lgni_pc,ave_u5mr,ave_imr,ave_malnourished)
cov = names(relevant_names)
source("balance.R")
bal = balance(cov, phase1_w, weight, dat_weight)
#dat1 = dat_weight


#################### Now for GBM approach ###########
phase0$inc = as.factor(phase0$inc)
gbm = twang::ps(treatment ~ave_malnourished + ave_u5mr + ave_imr + ave_lpop1 + ave_lpop5 + inc,data = as.data.frame(phase0),estimand ="ATT",stop.method=c("es.mean","ks.max"))
plot(gbm,plot=2)
bal.table(gbm)


# The plots have similar minima, meaning that the model will not be sensitive to which stopping rule is used.  Minimum balance is being achieved with a low number of iterations, so iterations do not need to be increased.
summary(gbm)
summary(gbm$gbm.obj,n.trees= gbm$desc$es.mean.ATT$n.trees,plot=TRUE)

gbm_df = phase1
gbm_df$weights = get.weights(gbm)




