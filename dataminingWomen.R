# ME 547 individual design project
# Redesiging shoe insole for men, and women
# Course Instructor: Dr. Matt Parkinson


Aw <- read.csv("use apt file path here.csv")
Aw$BMI<- (Aw$WEIGHT/9.81)/(Aw$STATURE/1000)^2
AwR <- subset (Aw, select=c('FOOT_BRTH', 'INSTEP_LNTH','FOOT_LNTH','BMI','STATURE'))

N1234Rf <- subset(N1234R,RIAGENDR==2) #NHANES female population\
N1234Rf$BMXBMI[which(is.na(N1234Rf$BMXBMI))] <- mean(N1234Rf$BMXBMI, na.rm = TRUE)
N1234Rf$BMXHT[which(is.na(N1234Rf$BMXHT))] <- mean(N1234Rf$BMXHT, na.rm = TRUE)


footLnthmodW <- lm(AwR$FOOT_LNTH ~ AwR$STATURE+AwR$BMI)
bflflModW <- lm(AwR$INSTEP_LNTH~AwR$FOOT_LNTH)

#adding foot length to NHANES data #in mm
N1234Rf$FL <- (N1234Rf$BMXBMI/1000000)*footLnthmodW$coefficients[3]+ N1234Rf$BMXHT*10*footLnthmodW$coefficients[2] + footLnthmodW$coefficients[1]+ rnorm(9669, mean = 0, sd = 8.83)

#using bflfl reg model to generate ball of foot length data for NHANES population
N1234Rf$BFL<-N1234Rf$FL*bflflModW$coefficients[2] + bflflModW$coefficients[1]+ rnorm(9669, mean = 0, sd = 3.021)

plot(N1234Rf$FL, N1234Rf$BFL, pch=19, cex=0.4, col="pink", xlab="Foot length (in mm)", ylab = "Ball of foot length (in mm)")
legend(locator(1),fill=c("pink"),c("generated with residual variance"))

#---------------current acco-------------------------------------

#accomodation for Nike (size 8 and 9.5)
subsetmeasuredNikeW <- subset(N1234Rf, FL<255 & FL>245 & BFL<195 & BFL>180) #people accommodated a/c to current design
subsetanalyticalNikeW <- subset(N1234Rf, FL<255 & FL>245)

accoNikeWomen <- sum(subsetmeasuredNikeW$WTMEC2YR)/sum(subsetanalyticalNikeW$WTMEC2YR) #accom % for Nike women


#accomodation for Power (size 8 and 9)
subsetmeasuredPowerW <- subset(N1234Rf, FL<245 & FL>230 & BFL<190 & BFL>175) #people accommodated a/c to current design
subsetanalyticalPowerW <- subset(N1234Rf, FL<245 & FL>230)

accoPowerWomen <- sum(subsetmeasuredPowerW$WTMEC2YR)/sum(subsetanalyticalPowerW$WTMEC2YR) #accom % for Power women

wtd.quantile(subsetanalyticalNikeW$BFL, weights = subsetanalyticalNikeW$WTMEC2YR/2, probs=c(5,25,50,75,95,99)/100)
wtd.quantile(subsetanalyticalPowerW$BFL, weights = subsetanalyticalPowerW$WTMEC2YR/2, probs=c(5,25,50,75,95,99)/100)

sdNikeWBFL <- sd(subsetanalyticalNikeW$BFL)
sdPowerWBFL <- sd(subsetanalyticalPowerW$BFL)

#-------NIKE---------SIZE 9 SHOULD HAVE THESE 3 VARIETIES OF BALL OF FOOT LENGTH
NikeWomen25th <- subset(N1234Rf, FL<255 & FL>245 & BFL<180)
NikeWomen75th <- subset(N1234Rf, FL<255 & FL>245 & BFL<185 &BFL>180)
NikeWomen99th <- subset(N1234Rf, FL<255 & FL>245 & BFL<190 &BFL>185) #name should be 95th and not 99th

#then plots now
plot(subsetanalyticalNikeW$FL, subsetanalyticalNikeW$BFL, pch=16, cex=0.4, col="sea green", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(NikeWomen25th$FL,NikeWomen25th$BFL, pch=16, cex=0.4, col="red", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(NikeWomen75th$FL,NikeWomen75th$BFL, pch=16, cex=0.4, col="blue", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(NikeWomen99th$FL,NikeWomen99th$BFL, pch=16, cex=0.4, col="violet", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")

legend("bottomright", fill=c("sea green","red","blue", "violet"),c("population left out","accommodated by KP1", "accommodated by KP2", "accommodated by KP3"))

#now accommodation
accoNewNikeW <- (sum(NikeWomen25th$WTMEC2YR)+sum(NikeWomen75th$WTMEC2YR)+sum(NikeWomen99th$WTMEC2YR))/sum(subsetanalyticalNikeW$WTMEC2YR) 

#-------POWER---------SIZE 9 SHOULD HAVE THESE 3 VARIETIES OF BALL OF FOOT LENGTH
PowerWomen25th <- subset(N1234Rf, FL<245 & FL>230 & BFL<180)
PowerWomen75th <- subset(N1234Rf, FL<245 & FL>230 & BFL<185 &BFL>180)
PowerWomen99th <- subset(N1234Rf, FL<245 & FL>230 & BFL<190 &BFL>185) #name should be 95th and not 99th

#now accommodation
accoNewPowerW <- (sum(PowerWomen25th$WTMEC2YR)+sum(PowerWomen75th $WTMEC2YR)+sum(PowerWomen99th$WTMEC2YR))/sum(subsetanalyticalPowerW$WTMEC2YR) 


#then plots now
plot(subsetanalyticalPowerW$FL, subsetanalyticalPowerW$BFL, pch=16, cex=0.4, col="sea green", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(PowerWomen25th$FL,PowerWomen25th$BFL, pch=16, cex=0.4, col="red", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(PowerWomen75th$FL,PowerWomen75th$BFL, pch=16, cex=0.4, col="blue", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")
points(PowerWomen99th$FL,PowerWomen99th$BFL, pch=16, cex=0.4, col="violet", xlab = "Foot length (in mm)", ylab = "Ball of foot length (in mm)")

legend("bottomright", fill=c("sea green","red","blue", "violet"),c("population left out","accommodated by KP1", "accommodated by KP2", "accommodated by KP3"))

