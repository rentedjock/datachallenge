# Data Challenge
#purpose: Turn aggregate data to individual data
#author : Abhinav Thakral
#Date: 11/6/2020


mydata<- read.csv("data.csv")
library(tidyverse)
colnames(mydata) <- c("Province", colnames(mydata[, 2:ncol(mydata)]))

#is it numeric
is.numeric(mydata$X1)

# all refers to all categories: remove that

#Verifying if null categories
is.null(mydata$OtherChemo[210])
9+
#checking levels
is.factor(mydata$ï..Province)

levels(mydata$ï..Province)

is.factor(mydata$Sex)

is.factor(mydata$Age)
levels(mydata$Age)
levels(mydata$Sex)
unique.x1 <- mydata[mydata$OtherChemo != "ALL" & mydata$Sex != "All", "X1"]


#Finding the sample size
mydata[mydata$OtherChemo == "ALL" & mydata$Sex == "ALL" & mydata$ï..Province=="ALL" & mydata$Age == "ALL", "X1"] #1441, NA, NA

max(mydata$X1, na.rm =T) #1441


#therefore, sample size =1441


#####Creating the study data frame


newdata <- data.frame()
subset <- mydata[mydata$OtherChemo != "ALL" & mydata$Sex != "ALL" & mydata$Province!="ALL" & mydata$Age != "ALL",]


#working with X1

#because

sum(subset$X1, na.rm = T)#1441

##Filling the variables

subset.treat <- subset[subset$Treat=="Tx", ]
id <- seq(1, sum(subset$X1, na.rm = T), 1)
r <- sum(subset$X1, na.rm = T)

province <- vector("character")
otherchemo <- vector("character" )
sex <- vector("character")

agegroup <- vector("character")




newdata <- data.frame( province,otherchemo, sex, agegroup, stringsAsFactors = T)


subset.treat$Age <- as.factor(subset.treat$Age)

#repeating the rows


for (i in 1:nrow(subset.treat)) {
  
  newdata <- rbind(newdata, subset.treat[rep(i, subset.treat[i, 6]), 1:4] )
 
}

#Adding time variables


time.event <- vector("numeric", r)
time.censor <- vector("numeric", r)

#putting them in the data frame
newdata <- data.frame(id, newdata,  time.event, time.censor)





#setting counter at 1
counter <- 1 

for ( i in 1:length(id)){
 
  
  #checking if any element in the event row is not missing
if (any(!is.na(subset[3*i-1, 6:45]))){
  #if yes, which are they?
  event.rep <- which(!is.na ( subset[3*i-1, 6:45]) ) # this number also gives the time of event
  
  #for all the columns with non missing values
  for (j in seq_along(event.rep)  ){
    
    #find out the number of patients with that event
    p <- subset[3*i-1, 5+ event.rep[j]]
   
    if (p!=0){
      for (k in 1:p){
      #add that time to event for the patients with those chaarcteristics
      newdata$time.event [counter] <-event.rep[j]
      #update counter
      counter<- counter +1
    }
  } 
    
  }
} 
  
  #checking if any element in the censor row is not missing and repeating above steps
  #counter already updated so that patients with events and censors are mutually exclusive

  
  if (any(!is.na(subset[3*i, 6:45]))){
 
   censor.rep <- which(!is.na ( subset[3*i, 6:45]) )
  
  for (j in seq_along(censor.rep)  ){
    
    p <- subset[3*i, 5+ censor.rep[j] ]
    if (p!=0){
      for (k in 1:p){
      newdata$time.censor [counter] <-censor.rep[j]
      
      counter<- counter +1
    }
  }  
    
    
  }
}  


  #jump the counter to next X1, so that those qithout censoring and event are skipped, ie, remain 0
  counter <- sum(subset$X1[1:(3*i-2) ], na.rm = T)+ 1
  
  
}

#ta-daa!



#----Analysis ------------------------#

#new$time.censor <- new$time.censor-1
#new$time.event <- new$time.event -1
new <-newdata%>%
  mutate(time.tot = time.censor+time.event, status= ifelse(time.event  !=0, 1,0 ))

new$time.tot <- new$time.tot-1


new[new$time.tot==0, 2:5]

new$OtherChemo[new$OtherChemo== "Null"] <- NA

new$Province[new$Province=="UNKWN"] <- NA

write.csv(newdata, "analysis_set.csv")




new$time.tot[which(new$time.tot==-1)]<- 39


#answer 1: 704

length(which(new$time.tot >9))
library(rms)

fit <- npsurv(Surv(time.tot, status)~1, data = new)
plot(fit)

summary(fit)

#new[new$time.tot==40, 2:5]
mean(1/new$time.tot)
#answer 3: 0.2372147

#answer 2:

new<- droplevels(new)

fit.cox <- cph (Surv(time.tot, status)~+Age+Sex+OtherChemo+ Province , data= new)

fit.age <- npsurv (Surv(time.tot, status)~Age , data= new)
fit.sex <- npsurv (Surv(time.tot, status)~Sex , data= new)
fit.oth <-npsurv (Surv(time.tot, status)~otherchemo , data= new)
fit.cox
