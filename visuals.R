##################################################################
#1. Discontinuity Plots - Goal: create compelling plots on outcomes for discontinuity
#looking at the mortality rates pre 2000 per country
#only 2 datapoints - one every 5 years
ggplot(data=pregavi, aes(x=year, y=u5mr, group=country)) +
  geom_line()+
  geom_point()

ggplot(data=pregavi, aes(x=year, y=lu5mr, group=country)) +
  geom_line()+
  geom_point()

#do the same with coverage estimates - they're annual
ggplot(data=pregavi, aes(x=year, y=imr, group=country)) +
  geom_line()+
  geom_point()

ggplot(data=pregavi, aes(x=year, y=dpt, group=country)) +
  geom_line()+
  geom_point()
ggplot(data=phase1, aes(x=year, y=dpt, group=country)) +
  geom_line()+
  geom_point()


##################################################################
#2. Exploratory Plots of Variables

#2a - DPT vaccination rates - m outcome variable?
control = phase1 %>% filter(treatment == 0)
treat = phase1 %>% filter(treatment == 1)
hist(control$dpt ,col=rgb(0,0,1,alpha=0.7),main="DPT Vaccination Rates", xlab="DPT vaccination rates", breaks=30, xlim=c(0,100))
hist(treat$dpt, col=rgb(0,1,0,alpha=0.7),add=T, breaks=30)
legend('topleft',col=c("blue","green"), lty=1:1, legend=c('control','treatment'))
