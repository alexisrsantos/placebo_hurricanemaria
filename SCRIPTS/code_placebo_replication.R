library(readxl)
library(coefplot)
Data <- read_excel("~/Data.xlsx")

#With Control for Year
#January 
January<-subset(Data,Month==1)
January<-subset(January, Year<2018)

model1<-(lm(Deaths~Dummy+Year,data=January))
summary(lm(Deaths~Dummy+Year,data=January))

#February 
February<-subset(Data,Month==2)
February<-subset(February, Year<2018)

summary(lm(Deaths~Dummy+Year,data=February))
model2<-lm(Deaths~Dummy+Year,data=February)

#March
March<-subset(Data,Month==3)
March<-subset(March, Year<2018)

summary(lm(Deaths~Dummy+Year,data=March))
model3<-lm(Deaths~Dummy+Year,data=March)

#April
April<-subset(Data,Month==4)
April<-subset(April, Year<2018)

summary(lm(Deaths~Dummy+Year,data=April))
model4<-lm(Deaths~Dummy+Year,data=April)

#May
May<-subset(Data,Month==5)
May<-subset(May, Year<2018)

summary(lm(Deaths~Dummy+Year,data=May))
model5<-lm(Deaths~Dummy+Year,data=May)

#June
June<-subset(Data,Month==6)
June<-subset(June, Year<2018)

summary(lm(Deaths~Dummy+Year,data=June))
model6<-lm(Deaths~Dummy+Year,data=June)

#July
July<-subset(Data,Month==7)
July<-subset(July, Year<2018)

summary(lm(Deaths~Dummy+Year,data=July))
model7<-lm(Deaths~Dummy+Year,data=July)

#August
August<-subset(Data,Month==8)
August<-subset(August, Year<2018)

summary(lm(Deaths~Dummy+Year,data=August))
model8<-lm(Deaths~Dummy+Year,data=August)

#September
September<-subset(Data,Month==9)
September<-subset(September, Year<2018)

summary(lm(Deaths~Dummy+Year,data=September))
model9<-lm(Deaths~Dummy+Year,data=September)

#October
October<-subset(Data,Month==10)
October<-subset(October, Year<2018)

summary(lm(Deaths~Dummy+Year,data=October))
model10<-lm(Deaths~Dummy+Year,data=October)

#November
November<-subset(Data,Month==11)
November<-subset(November, Year<2018)

summary(lm(Deaths~Dummy+Year,data=November))
model11<-lm(Deaths~Dummy+Year,data=November)

#December
December<-subset(Data,Month==12)
December<-subset(December, Year<2018)

summary(lm(Deaths~Dummy+Year,data=December))
model12<-lm(Deaths~Dummy+Year,data=December)

#Intervals shown in Table 1
confint(model1,level=0.95)
confint(model2,level=0.95)
confint(model3,level=0.95)
confint(model4,level=0.95)
confint(model5,level=0.95)
confint(model6,level=0.95)
confint(model7,level=0.95)
confint(model8,level=0.95)
confint(model9,level=0.95)
confint(model10,level=0.95)
confint(model11,level=0.95)
confint(model12,level=0.95)

###############################
#### 2018 VS PRE-2017##########
###############################

#January 
January2<-subset(Data,Month==1)
January2<-subset(January2, Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=January2))

model1.2<-(lm(Deaths~Dummy+Year,data=January2))

#February 
February2<-subset(Data,Month==2)
February2<-subset(February2,Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=February2))

model2.2<-lm(Deaths~Dummy+Year,data=February2)

#March
March2<-subset(Data,Month==3)
March2<-subset(March2,Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=March2))

model3.2<-lm(Deaths~Dummy+Year,data=March2)

#April
April2<-subset(Data,Month==4)
April2<-subset(April2, Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=April2))

model4.2<-lm(Deaths~Dummy+Year,data=April2)

#May
May2<-subset(Data,Month==5)
May2<-subset(May2, Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=May2))

model5.2<-lm(Deaths~Dummy+Year,data=May2)

#June
June2<-subset(Data,Month==6)
June2<-subset(June2, Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=June2))

model6.2<-lm(Deaths~Dummy+Year,data=June2)

#July
July2<-subset(Data,Month==7)
July2<-subset(July2, Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=July2))

model7.2<-lm(Deaths~Dummy+Year,data=July2)

#August
August2<-subset(Data,Month==8)
August2<-subset(August2, Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=August2))
model8.2<-lm(Deaths~Dummy+Year,data=August2)

#September
September2<-subset(Data,Month==9)
September2<-subset(September2, Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=September2))
model9.2<-(lm(Deaths~Dummy+Year,data=September2))


#October
October2<-subset(Data,Month==10)
October2<-subset(October2, Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=October2))
model10.2<-lm(Deaths~Dummy+Year,data=October2)

#November
November2<-subset(Data,Month==11)
November2<-subset(November2,Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=November2))
model11.2<-(lm(Deaths~Dummy+Year,data=November2))

#December
December2<-subset(Data,Month==12)
December2<-subset(December2,Year<2017|Year>2017)

summary(lm(Deaths~Dummy+Year,data=December2))
model12.2<-lm(Deaths~Dummy+Year,data=December2)

#Intervals Shown in Table 1
confint(model1.2,level=0.95)
confint(model2.2,level=0.95)
confint(model3.2,level=0.95)
confint(model4.2,level=0.95)
confint(model5.2,level=0.95)
confint(model6.2,level=0.95)
confint(model7.2,level=0.95)
confint(model8.2,level=0.95)
confint(model9.2,level=0.95)
confint(model10.2,level=0.95)
confint(model11.2,level=0.95)
confint(model12.2,level=0.95)
