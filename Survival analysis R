library(survival)
install.packages("xlsx")
library(xlsx)
data=read.xlsx("C:/Users/preethi/Desktop/study material/pgcba/pred/survival/aids_survival.xlsx",header=T,sheetIndex=1)
View(data)
attach(data)
###Kaplan-Meier estimate and pointwise bounds
my.surv.object=Surv(time,time_d,censor_d)
summary(survfit(Surv(time,time_d,censor_d)~1))
my.surv.object
my.fit=survfit(my.surv.object~1)
summary(my.fit)$surv # outputs the Kaplan-Meier estimate at each t_i
summary(my.fit)$time # {t_i}
summary(my.fit)$n.risk # {Y_i}
summary(my.fit)$n.event # {d_i}
summary(my.fit)$std.err # standard error of the K-M estimate at {t_i}
summary(my.fit)$lower # lower pointwise estimates (alternatively, $upper)
plot(my.fit)
plot(my.fit, main="Kaplan-Meier estimate with 95% confidence bounds",xlab="time", ylab="survival function")
myfit1=survfit(my.surv.object~txgrp)
summary(myfit1)$strata
plot(my.fit)
plot(myfit1, main="Kaplan-Meier estimate for two types of treatment groups",xlab="time", ylab="survival function")
source('C:/Users/preethi/Desktop/study material/pgcba/pred/survival/conf.bands.r')
#####Confidence bands
my.cb=conf.bands(critical.value = 0.95,survfit.obj=my.fit, band.type="EP", conf.type="plain",plot=T)
####Cumulative Hazard
H.hat <- -log(my.fit$surv)
H.hat <- c(H.hat, H.hat[length(H.hat)])
plot(c(my.fit$time, 250), H.hat, xlab="time", ylab="cumulative hazard",main="cumulative hazard", type="s")
#legend(locator(1), legend=c("H.hat","H.tilde"), lty=1:2)
#### difference between groups
survdiff(Surv(time_d,censor_d==1) ~ sex, data=data)
###Cox proportional hazards model, constant covariates
cox.fit=coxph(my.surv.object ~ as.factor(txgrp)+as.factor(karnof)+cd4+priorzdv, method="breslow")
summary(cox.fit)
source('C:/Users/preethi/Desktop/study material/pgcba/pred/survival/local.coxph.test.r')
local.coxph.test(coxph.fit,1:2)
my.survfit.object <- survfit(coxph.fit)
