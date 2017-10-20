#ME 547 Krishna Prasad Varadarajan Srinivasan
#Individual design project -------- Redesigning insoles

install.packages("scatterplot3d")
library(scatterplot3d)
library(Hmisc)

#measures to be used: foot length, ball of foot length, foot breadth, horizontal
#reading ANSUR men and women data from local hard drive
Am <- read.csv("user apt file path here.csv")
Aw <- read.csv("user apt file path here.csv")

#adding BMI data
Am$BMI<- (Am$WEIGHT/9.81)/(Am$STATURE/1000)^2
Aw$BMI<- (Aw$WEIGHT/9.81)/(Aw$STATURE/1000)^2

#creating a subset with the measures that we are concerned with
AmR <- subset (Am, select=c('FOOT_BRTH', 'INSTEP_LNTH','FOOT_LNTH','BMI','STATURE'))
AwR <- subset (Aw, select=c('FOOT_BRTH', 'INSTEP_LNTH','FOOT_LNTH','BMI','STATURE'))

#-----------------DESIGNING FOR MEN FIRST------------------------------
#finding correlations between BMI, stature and various measures
plot (AmR$FOOT_LNTH, AmR$STATURE, pch = 19, cex = 0.4, col='blue', xlab="foot length (in mm)", ylab = "Stature (in mm)")
legend(locator(1),c("stature Vs foot length plot for men"))
plot (AmR$FOOT_LNTH, AmR$BMI, pch = 19, cex = 0.4, col='red',xlab="foot length (in mm)", ylab = "BMI(in kg/m**2)")
legend(locator(1),c("BMI Vs foot length plot for men"))
scatterplot3d(AmR$FOOT_LNTH, AmR$BMI, AmR$STATURE , xlab = "foot length (in mm)", zlab = "stature (in mm)", ylab = "BMI (in kg/m**2)", color = "orange")
legend(locator(1),c("BMI and stature Vs foot length plot for men"))
cor(AmR$FOOT_LNTH, AmR$STATURE)
cor(AmR$FOOT_LNTH, AmR$BMI)
cor(AmR$FOOT_LNTH, AmR$BMI+Am$STATURE)

plot (AmR$INSTEP_LNTH, AmR$STATURE, pch = 19, cex = 0.4, col='blue', xlab="ball of foot length (in mm)", ylab = "Stature (in mm)")
legend(locator(1),c("stature Vs ball of foot length plot for men"))
plot (AmR$INSTEP_LNTH, AmR$BMI, pch = 19, cex = 0.4, col='red', xlab="ball of foot length (in mm)", ylab = "BMI(in kg/m**2)")
legend(locator(1),c("BMI Vs ball of foot length plot for men"))
scatterplot3d(AmR$INSTEP_LNTH, AmR$BMI, AmR$STATURE , xlab = "ball of foot length(in mm)", zlab = "stature (in mm)", ylab = "BMI (in kg/m**2)", color = "orange")
legend(locator(1),c("BMI and stature Vs ball of foot length plot for men"))
cor(AmR$INSTEP_LNTH, AmR$STATURE)
cor(AmR$INSTEP_LNTH, AmR$BMI)
cor(AmR$INSTEP_LNTH, AmR$BMI+Am$STATURE)

plot (AmR$FOOT_BRTH, AmR$STATURE, pch = 19, cex = 0.4, col='blue', xlab="foot breadth (in mm)", ylab = "Stature (in mm)")
legend(locator(1),c("stature Vs foot breadth plot for men"))
plot (AmR$FOOT_BRTH, AmR$BMI, pch = 19, cex = 0.4, col='red',xlab="foot breadth (in mm)", ylab = "BMI(in kg/m**2)")
legend(locator(1),c("BMI Vs foot breadth plot for men"))
scatterplot3d(AmR$FOOT_BRTH, AmR$BMI, AmR$STATURE , xlab = "foot breadth(in mm)", zlab = "stature (in mm)", ylab = "BMI (in kg/m**2)", color = "orange")
legend(locator(1),c("BMI and stature Vs foot breadth plot for men"))
cor(AmR$FOOT_BRTH, AmR$STATURE)
cor(AmR$FOOT_BRTH, AmR$BMI)
cor(AmR$FOOT_BRTH, AmR$BMI+Am$STATURE)




#building regression models using ANSUR data - stature for all three
footLnthmodM <- lm(AmR$FOOT_LNTH ~ AmR$STATURE+AmR$BMI)
instepLnthmodM <- lm(AmR$INSTEP_LNTH ~ AmR$STATURE+AmR$BMI)
footBrthmodM <- lm(AmR$FOOT_BRTH ~ AmR$STATURE+Am$BMI)

#now adding residual variance to it #all values in mm
footLnthhEstM <- (AmR$BMI/1000000)*footLnthmodM$coefficients[3]+ AmR$STATURE*footLnthmodM$coefficients[2] + footLnthmodM$coefficients[1]+ rnorm(1774, mean = 0, sd = 9.129)
instepLnthhEstM <-  (AmR$BMI/1000000)*instepLnthmodM$coefficients[3]+AmR$STATURE*instepLnthmodM$coefficients[2] + instepLnthmodM$coefficients[1]+ rnorm(1774, mean = 0, sd = 7.922)
footBrthEstM <- (AmR$BMI/1000000)*footBrthmodM$coefficients[3]+AmR$STATURE*footBrthmodM$coefficients[2] + footBrthmodM$coefficients[1]+ rnorm(1774, mean = 0, sd = 4.213)

s1<-scatterplot3d(AmR$FOOT_LNTH, AmR$BMI, AmR$STATURE , xlab = "foot length(in mm)", zlab = "stature (in mm)", ylab = "BMI (in kg/m**2)", type="h", color = "blue", pch=16)
s1$points3d(footLnthhEstM,AmR$BMI,AmR$STATURE, col = "green")
legend(locator(1),fill=c("blue","green"), c("raw data set", "with residual variance"))

s2<-scatterplot3d(AmR$INSTEP_LNTH, AmR$BMI, AmR$STATURE , xlab = "ball of foot length(in mm)", zlab = "stature (in mm)", ylab = "BMI (in kg/mm**2)", color = "orange", type = "h", pch = 16)
s2$points3d(instepLnthhEstM,AmR$BMI,AmR$STATURE, col = "green")
legend(locator(1),fill=c("orange","green"), c("without residual variance", "with residual variance"))

s3<-scatterplot3d(AmR$FOOT_BRTH, AmR$BMI, AmR$STATURE , xlab = "foot breadth(in mm)", zlab = "stature (in mm)", ylab = "BMI (in kg/mm**2)", color = "orange", type = "h", pch = 16)
s3$points3d(footBrthEstM,AmR$BMI,AmR$STATURE, col = "green")
legend(locator(1),fill=c("orange","green"), c("without residual variance", "with residual variance"))


#regression model for ball of foot length Vs foot length from ANSUR data
bflflMod <- lm(AmR$INSTEP_LNTH~AmR$FOOT_LNTH)
instepLnthhEstM <- AmR$FOOT_LNTH*bflflMod$coefficients[2] + bflflMod$coefficients[1]+ rnorm(1774, mean = 0, sd = 4.153)

plot(AmR$FOOT_LNTH,AmR$INSTEP_LNTH,xlab = "foot length(in mm)", ylab = "ball of foot length(in mm)", pch=16, cex=0.4, col="red")
points(AmR$FOOT_LNTH, instepLnthhEstM, col="green")
legend("topleft",fill=c("red","green"), c("raw data", "with residual variance"))


#generting data for NHANES Population
load("C:/Penn State/Fall 2016 - courses/ME 547/Data files/nhanes_11_12")
load("C:/Penn State/Fall 2016 - courses/ME 547/Data files/nhanes_13_14")
N11R<- subset(Nhanes1112, select=c("BMXBMI", "BMXHT", "RIAGENDR", "RIDAGEYR","WTMEC2YR"))
N13R<- subset(Nhanes1314, select=c("BMXBMI", "BMXHT", "RIAGENDR", "RIDAGEYR","WTMEC2YR"))
N1234R<-rbind(N11R, N13R) #combining the data from 11-12 and 13-14
N1234Rm <- subset(N1234R,RIAGENDR==1) #NHANES male population


#replacing the NA values with mean for BMI and stature
N1234Rm$BMXBMI[which(is.na(N1234Rm$BMXBMI))] <- mean(N1234Rm$BMXBMI, na.rm = TRUE)
N1234Rm$BMXHT[which(is.na(N1234Rm$BMXHT))] <- mean(N1234Rm$BMXHT, na.rm = TRUE)


#adding foot length to NHANES data #in mm
N1234Rm$FL <- (N1234Rm$BMXBMI/1000000)*footLnthmodM$coefficients[3]+ N1234Rm$BMXHT*10*footLnthmodM$coefficients[2] + footLnthmodM$coefficients[1]+ rnorm(9482, mean = 0, sd = 9.129)

#using bflfl reg model to generate ball of foot length data for NHANES population
N1234Rm$BFL<-N1234Rm$FL*bflflMod$coefficients[2] + bflflMod$coefficients[1]+ rnorm(9482, mean = 0, sd = 4.153)

par(mar=c(5,5,2,1)) #changing margins in the plot to make y-label visible
plot(N1234Rm$FL, N1234Rm$BFL, pch=19, cex=0.4, col="red", xlab="Foot length (in mm)", ylab = "Ball of foot length (in mm)")
legend(locator(1),fill=c("red"),c("generated with residual variance"))


#-----calculating accommodation for benchmarked designs-------

#accomodation for Nike (size 9 and 10)
subsetmeasuredNike <- subset(N1234Rm, FL<274 & FL>263 & BFL<207 & BFL>203) #people accommodated a/c to current design
subsetanalyticalNike <- subset(N1234Rm, FL<274 & FL>263)

accoNikeMen <- sum(subsetmeasuredNike$WTMEC2YR)/sum(subsetanalyticalNike$WTMEC2YR) #accom % for Nike

plot(subsetanalyticalNike$FL, subsetanalyticalNike$BFL, pch=16, cex=0.4, col="cyan", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(subsetmeasuredNike$FL,subsetmeasuredNike$BFL, pch=16, cex=0.4, col="red", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
legend(locator(1), fill=c("cyan","red"),c("population left out","population accommodated"))

#accomodation for Adidas (size 9 and 10)
subsetmeasuredAdidas <- subset(N1234Rm, FL<280 & FL>273 & BFL<230 & BFL>215) #people accommodated a/c to current design
subsetanalyticalAdidas <- subset(N1234Rm, FL<280 & FL>273)

accoAdidasMen <- sum(subsetmeasuredAdidas$WTMEC2YR)/sum(subsetanalyticalAdidas$WTMEC2YR) #accom % for Adidas

plot(subsetanalyticalAdidas$FL, subsetanalyticalAdidas$BFL, pch=16, cex=0.4, col="black", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(subsetmeasuredAdidas$FL,subsetmeasuredAdidas$BFL, pch=20, cex=0.4, col="red", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
legend(locator(1), fill=c("black","red"),c("population left out","population accommodated"))

#accomodation for Reebok (size 9 and 10)
subsetmeasuredReebok <- subset(N1234Rm, FL<279.4 & FL>269 & BFL<220 & BFL>210) #people accommodated a/c to current design
subsetanalyticalReebok <- subset(N1234Rm, FL<279.4 & FL>269)

accoReebokMen <- sum(subsetmeasuredReebok$WTMEC2YR)/sum(subsetanalyticalReebok$WTMEC2YR) #accom % for Reebok

plot(subsetanalyticalReebok$FL, subsetanalyticalReebok$BFL, pch=16, cex=0.4, col="seagreen", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(subsetmeasuredReebok$FL,subsetmeasuredReebok$BFL, pch=20, cex=0.4, col="red", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
legend(locator(1), fill=c("seagreen","red"),c("population left out","population accommodated"))


# #------improving the accommodation levels
#weighted quantiles for Nike, Addidast and Reebok BFL data
wtd.quantile(subsetanalyticalNike$BFL, weights = subsetanalyticalNike$WTMEC2YR/2, probs=c(5,25,50,75,95,99)/100)
wtd.quantile(subsetanalyticalAdidas$BFL, weights = subsetanalyticalAdidas$WTMEC2YR/2, probs=c(5,25,50,75,95,99)/100)
wtd.quantile(subsetanalyticalReebok$BFL, weights = subsetanalyticalReebok$WTMEC2YR/2, probs=c(5,25,50,75,95,99)/100)

#----------------Part 1------------------------

#IF shoes sizes were based on ball of foot lengths - \label{flipped}

reversetryNike<-subset(N1234Rm, BFL<207 & BFL>203)
reversetryAdidas<-subset(N1234Rm, BFL<230 & BFL>215)
reversetryReebok<-subset(N1234Rm, BFL<220 & BFL>210)

#accomodation levesl
accoreverseNike <- sum(subsetmeasuredNike$WTMEC2YR)/sum(reversetryNike$WTMEC2YR) #22.88%
accoreverseAdidas <- sum(subsetmeasuredAdidas$WTMEC2YR)/sum(reversetryAdidas$WTMEC2YR) #0%
accoreverseReebok <- sum(subsetmeasuredReebok$WTMEC2YR)/sum(reversetryReebok$WTMEC2YR) #9.7%

#plots
plot(reversetryNike$FL, reversetryNike$BFL, pch=16, cex=0.4, col="cyan", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(subsetmeasuredNike$FL,subsetmeasuredNike$BFL, pch=16, cex=0.4, col="red", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
legend(locator(1), fill=c("cyan","red"),c("population left out","population accommodated"))

plot(reversetryAdidas$FL, reversetryAdidas$BFL, pch=16, cex=0.4, col="black", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(subsetmeasuredAdidas$FL,subsetmeasuredAdidas$BFL, pch=20, cex=0.4, col="red", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
legend(locator(1), fill=c("black","red"),c("population left out","population accommodated"))

plot(reversetryReebok$FL, reversetryReebok$BFL, pch=16, cex=0.4, col="seagreen", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(subsetmeasuredReebok$FL,subsetmeasuredReebok$BFL, pch=20, cex=0.4, col="red", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
legend(locator(1), fill=c("seagreen","red"),c("population left out","population accommodated"))

#-----------------Part 2----------------------
#incorporating a range of ball of foot length -- step determined by std. dev.

sdNikeBFL <- sd(subsetanalyticalNike$BFL)

#-------NIKE---------SIZE 10 SHOULD HAVE THESE 4 VARIETIES OF BALL OF FOOT LENGTH
NikeMen25th <- subset(N1234Rm, FL<274 & FL>263 & BFL<190)
NikeMen50th <- subset(N1234Rm, FL<274 & FL>263 &BFL<195 & BFL>190)
NikeMen75th <- subset(N1234Rm, FL<274 & FL>263 & BFL<200 &BFL>195)
NikeMen99th <- subset(N1234Rm, FL<274 & FL>263 & BFL<205 &BFL>200) #name should be 95th and not 99th

#now accommodation
accoNewNike <- (sum(NikeMen25th$WTMEC2YR)+sum(NikeMen50th$WTMEC2YR)+sum(NikeMen75th$WTMEC2YR)+sum(NikeMen99th$WTMEC2YR))/sum(subsetanalyticalNike$WTMEC2YR) 

#then plots now
plot(subsetanalyticalNike$FL, subsetanalyticalNike$BFL, pch=16, cex=0.4, col="sea green", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(NikeMen25th$FL,NikeMen25th$BFL, pch=16, cex=0.4, col="red", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(NikeMen50th$FL,NikeMen50th$BFL, pch=16, cex=0.4, col="orange", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(NikeMen75th$FL,NikeMen75th$BFL, pch=16, cex=0.4, col="blue", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(NikeMen99th$FL,NikeMen99th$BFL, pch=16, cex=0.4, col="violet", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")

legend("topleft", fill=c("sea green","red", "orange","blue", "violet"),c("population left out","accommodated by KP1", "accommodated by KP2", "accommodated by KP3", "accommodated by KP4"))


#-------ADDIDAS---------SIZE 10 SHOULD HAVE THESE 4 VARIETIES OF BALL OF FOOT LENGTH

sdAdidasBFL <- sd(subsetanalyticalAdidas$BFL)

AddidasMen25th <- subset(N1234Rm, FL<280 & FL>273 & BFL<195)
AddidasMen50th <- subset(N1234Rm, FL<280 & FL>273 & BFL<200 & BFL>195)
AddidasMen75th <- subset(N1234Rm, FL<280 & FL>273 & BFL<205 &BFL>200)
AddidasMen99th <- subset(N1234Rm, FL<280 & FL>273 & BFL<210 &BFL>205) #name should be 95th, not 99th

#now accommodation
accoNewAddidas <- (sum(AddidasMen25th$WTMEC2YR)+sum(AddidasMen50th$WTMEC2YR)+sum(AddidasMen75th$WTMEC2YR)+sum(AddidasMen99th$WTMEC2YR))/sum(subsetanalyticalAdidas$WTMEC2YR) 

plot(subsetanalyticalAdidas$FL, subsetanalyticalAdidas$BFL, pch=16, cex=0.4, col="sea green", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(AddidasMen25th$FL,AddidasMen25th$BFL, pch=16, cex=0.4, col="red", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(AddidasMen50th$FL,AddidasMen50th$BFL, pch=16, cex=0.4, col="orange", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(AddidasMen75th$FL,AddidasMen75th$BFL, pch=16, cex=0.4, col="blue", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(AddidasMen99th$FL,AddidasMen99th$BFL, pch=16, cex=0.4, col="violet", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")

legend("topleft", fill=c("sea green","red", "orange","blue", "violet"),c("population left out","accommodated by KP1", "accommodated by KP2", "accommodated by KP3", "accommodated by KP4"))


#-------REEBOK---------SIZE 10 SHOULD HAVE THESE 4 VARIETIES OF BALL OF FOOT LENGTH

sdReebokBFL <- sd(subsetanalyticalReebok$BFL)

ReebokMen25th <- subset(N1234Rm, FL<279.4 & FL>269 & BFL<195)
ReebokMen50th <- subset(N1234Rm, FL<279.4 & FL>269 & BFL<200 & BFL>195)
ReebokMen75th <- subset(N1234Rm, FL<279.4 & FL>269 & BFL<205 &BFL>200)
ReebokMen99th <- subset(N1234Rm, FL<279.4 & FL>269 & BFL<210 &BFL>205)

#now accommodation
accoNewReebok <- (sum(ReebokMen25th$WTMEC2YR)+sum(ReebokMen50th$WTMEC2YR)+sum(ReebokMen75th$WTMEC2YR)+sum(ReebokMen99th$WTMEC2YR))/sum(subsetanalyticalReebok$WTMEC2YR) 

plot(subsetanalyticalReebok$FL, subsetanalyticalReebok$BFL, pch=16, cex=0.4, col="sea green", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(ReebokMen25th$FL,ReebokMen25th$BFL, pch=16, cex=0.4, col="red", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(ReebokMen50th$FL,ReebokMen50th$BFL, pch=16, cex=0.4, col="orange", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(ReebokMen75th$FL,ReebokMen75th$BFL, pch=16, cex=0.4, col="blue", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(ReebokMen99th$FL,ReebokMen99th$BFL, pch=16, cex=0.4, col="violet", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")

legend("topleft", fill=c("sea green","red", "orange","blue", "violet"),c("population left out","accommodated by KP1", "accommodated by KP2", "accommodated by KP3", "accommodated by KP4"))

