getwd()
library(rms)
library(survival)
library(survminer)
library(dplyr)
### Question 1 ####

length(which(new$time.tot >9))

### 704
### Question 2 ####

dd <- datadist(new)
options(datadist='dd')

survived <- Surv(new$time.tot, new$status)

survival_age         <- cph(survived~Age, data = new, x=TRUE, y=TRUE)
survival_sex         <- cph(survived~Sex, data = new, x=TRUE, y=TRUE)
survival_otherchemo  <- cph(survived~OtherChemo, data = new, x=TRUE, y=TRUE)
survival_province    <- cph(survived~Province, data = new, x=TRUE, y=TRUE)

survival_all         <- cph(survived~Age + Sex + Province, data = new, x=TRUE, y=TRUE)

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
summary(survival_province)
summary(survival_all)


survplot(survival_sex, xlab = "Months of treatment", xlim = c(0,max(new$time.tot)), conf.int = 0.95, 
         col = c("red", "black"), conf.type = )

### rate of ####
mean(1/new$time.tot)

### ggplot
survival_agegg       <- survfit(Surv(new$time.tot, new$status)~Age, data = new)
survival_sexgg       <- survfit(Surv(new$time.tot, new$status)~Sex, data = new)
survival_otherchemogg<- survfit(Surv(new$time.tot, new$status)~OtherChemo, data = new)
survival_provincegg  <- survfit(Surv(new$time.tot, new$status)~Province, data = new)

survival_allgg       <- survfit(Surv(new$time.tot, new$status)~Age + Sex + Province, data = new)

ggsurvplot(survival_agegg, data = new)
ggsurvplot(survival_sexgg, data = new)
ggsurvplot(survival_otherchemogg, data = new)
ggsurvplot(survival_provincegg, data = new)

