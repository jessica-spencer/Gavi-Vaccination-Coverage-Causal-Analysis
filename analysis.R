############ Outcome Variable #######
# Basic density
p <- ggplot(phase1, aes(x=Y_imr)) + 
  geom_density()
p
# Add mean line
p+ geom_vline(aes(xintercept=mean(Y_imr)),
              color="blue", linetype="dashed", size=1)


#################### Matched with Logit + Nearest Neighbors effect ###
tot_W = lm(Y_imr~treatment,data = phase1_w)
summary(tot_W)


#############robust estimate
tot_W_r = lm(Y_imr~treatment+ave_malnourished+ ave_imr*inc + ave_lpop1 +  ave_u5mr,data = phase1_w)
summary(tot_W_r)
dotwhisker::dwplot(tot_W)

################### GBM for matching #################################  
design_gbm<- svydesign(ids=~1, weights=~weights, data=gbm_df)
tot_gbm <- svyglm(Y_imr ~ treatment, design=design_gbm)
summary(tot_gbm) 

#############robust estimate
tot_gbm_r <- svyglm(Y_imr ~ treatment+ave_malnourished+ ave_imr*inc + ave_lpop1 +  ave_u5mr, design=design_gbm)
summary(tot_gbm_r) 
dotwhisker::dwplot(tot_gbm)



a = stargazer(tot_W, tot_W_r, tot_gbm, tot_gbm_r, title="Results", align=TRUE)
lig
print(a)
