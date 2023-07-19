#IV design
dat1 = dat1 %>% filter(weight>0)

model1.reg = lm(fullvacc~ treatment + ccode + year + malnourished_who + wasted_who + u5mr_dhs + u5mr + imr_dhs + imr, dat = dat1)
summary(model1.reg)
model1.reg$coefficients[2]

