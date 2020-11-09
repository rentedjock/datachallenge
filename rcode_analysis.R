getwd()
library(rms)
library(survival)
library(survminer)
library(dplyr)
### Question 1 ####

length(which(new$time.tot >9))

### 704

### Question 2 ####
new$Province[which(new$Province=="UNKWN")]<- "NA"
new$OtherChemo[which(new$OtherChemo=="Null")]<- "NA"

newdata <- new[which(new$Province!='NA' & new$OtherChemo!='NA'),]

length(newdata[,1])

dd <- datadist(newdata)
options(datadist='dd')

survived <- Surv(newdata$time.tot, newdata$status)

survival_age         <- cph(survived~Age, data = newdata, x=TRUE, y=TRUE)
survival_sex         <- cph(survived~Sex, data = newdata, x=TRUE, y=TRUE)
survival_otherchemo  <- cph(survived~OtherChemo, data = newdata, x=TRUE, y=TRUE)
survival_province    <- cph(survived~Province, data = newdata, x=TRUE, y=TRUE)

survival_all         <- cph(survived~Age + Sex + OtherChemo + Province, data = newdata,
                            x=TRUE, y=TRUE)

## Issue in the otherchemo - potential violation of proportional hazard assumption
cox.zph(survival_age)
cox.zph(survival_sex)
cox.zph(survival_otherchemo)
cox.zph(survival_province)
cox.zph(survival_all)

## Province is significantly associated with an different hazard risk of 
print(survival_age)
print(survival_sex)
print(survival_otherchemo)
print(survival_province)
print(survival_all)

summary(survival_sex)
summary(survival_otherchemo)
summary(survival_province)
summary(survival_all)


survplot(survival_sex, xlab = "Months of treatment", xlim = c(0,max(newdata$time.tot)), conf.int = 0.95, 
         col = c("red", "black"), conf.type = )

### rate of ####
mean(1/new$time.tot)

### ggplot
survival_raw         <- survfit(Surv(newdata$time.tot, newdata$status)~1, data = newdata)
survival_agegg       <- survfit(Surv(newdata$time.tot, newdata$status)~Age, data = newdata)
survival_sexgg       <- survfit(Surv(newdata$time.tot, newdata$status)~Sex, data = newdata)
survival_otherchemogg<- survfit(Surv(newdata$time.tot, newdata$status)~OtherChemo, data = newdata)
survival_provincegg  <- survfit(Surv(newdata$time.tot, newdata$status)~Province, data = newdata)

survival_allgg       <- survfit(Surv(newdata$time.tot, newdata$status)~Age + Sex + Province, 
                                data = newdata, na.rm = TRUE)

ggsurvplot(survival_raw, data = newdata)
ggsurvplot(survival_agegg, data = newdata)
ggsurvplot(survival_sexgg, data = newdata)
ggsurvplot(survival_otherchemogg, data = newdata)
ggsurvplot(survival_provincegg, data = newdata)

### table 1 ####


