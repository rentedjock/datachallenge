# Data Challenge
#purpose: Turn aggregate data to individual data

mydata<- read.csv("data.csv")
library(tidyverse)


#is it numeric
is.numeric(mydata$X1)

# all refers to all categories: remove that

#Verifying if null categories
is.null(mydata$OtherChemo[210])

#checking levels
is.factor(mydata$ï..Province)

levels(mydata$ï..Province)

is.factor(mydata$Sex)

levels(mydata$Sex)
unique.x1 <- mydata[mydata$OtherChemo != "ALL" & mydata$Sex != "All", "X1"]

# id variable
id <- seq(1, sum(unique.x1, na.rm =T), 1)
newdata <- data.frame(id)


