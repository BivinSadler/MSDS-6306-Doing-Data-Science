## UNIT 12 LIVE SESSION! ##

#Website to update airline passenger numbers: 

#https://bitre.gov.au/statistics/aviation/domestic.aspx
#Great Source of DATA!!!!  QILK!!! DATAMARKET
#https://datamarket.com/data/set/14lt/air-transport-passengers-carried#!ds=14lt!gmr=28&display=line

library(fpp)

# 1. SES MODEL FOR AUS AIR 
data(ausair)

air = window(ausair, start = 1990, end = 2004)

plot(air,ylab = "Airline Passegners", xlab = "Year")

fit1 = ses(air, initial = "simple",alpha = .2,h = 3)
fit2 = ses(air,initial = "simple",alpha = .6, h = 3)
fit3 = ses(air, h = 3) #defaults

accuracy(fit1, ausair)
accuracy(fit2, ausair)
accuracy(fit3, ausair)

plot(air,ylab = "Airline Passegners", xlab = "Year", type = "o", xlim = c(1990, 2008),ylim = c(15,50))


lines(fitted(fit1), col = "blue", type = "o")
lines(fitted(fit2), col = "red", type = "o")
lines(fitted(fit3), col = "green", type = "o")

lines(fit1$mean, col = "blue", type = "o")
lines(fit2$mean, col = "red", type = "o")
lines(fit3$mean, col = "green", type = "o")

air2008 = window(ausair, start = 1990, end = 2007)
points(air2008, type = "o")




#2 Holt's Linear Trend Model for AUS AIR
fit1h = holt(air, alpha = .8, beta = .2, initial = "simple", h = 5)
fit2h = holt(air, alpha = .8, beta = .2, initial = "simple", exponential = TRUE, h = 5)

fitted(fit1h)
fit1h$mean

plot(air,ylab = "Airline Passegners", xlab = "Year", type = "o", xlim = c(1990, 2009),ylim = c(15,60))
lines(fitted(fit1h),col = "blue", type= "o")
lines(fitted(fit2h), col = "red", type= "o")
lines(fit1h$mean, col = "blue", type= "o")
lines(fit2h$mean,col = "red", type= "o")

fit3h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "simple", h = 5)
lines(fitted(fit3h), col = "darkgreen", type= "o")
lines(fit3h$mean,col = "darkgreen", type= "o")

fit4h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "simple", exponential = TRUE, h = 5)
lines(fitted(fit4h), col = "cyan", type= "o")
lines(fit4h$mean,col = "cyan", type= "o")

# with implicit Test set... it figures out by the time which are training and which are test. 
accuracy(fit1h, ausair)
accuracy(fit2h, ausair)
accuracy(fit3h, ausair)

#with explicit Test set
airTest = window(ausair, start = 2005)
accuracy(fit1h, airTest)
accuracy(fit2h, airTest)
accuracy(fit3h, airTest)

air2008 = window(ausair, start = 1990, end = 2009)
points(air2008, type = "o")

