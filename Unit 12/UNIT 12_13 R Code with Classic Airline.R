## UNIT 12 LIVE SESSION! ##
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


accuracy(fit1h, ausair)
accuracy(fit2h, ausair)
accuracy(fit3h, ausair)


air2008 = window(ausair, start = 1990, end = 2009)
points(air2008, type = "o")



#2.1 UP TO DATE! ######

ausair2 = read.csv("/Users/bivin/Desktop/OLD COMPUTER ARCHIVES/KadAfrica/MSDS/DDS/MSDS 6306/UNIT 12/ausair.csv",header = TRUE)

ausair2ts = ts(ausair2$Passengers,start = c(1970,1), frequency = 1)

#Holt's Linear Trend Model
fit1h = holt(air, alpha = .8, beta = .2, initial = "simple", h = 13)
fit2h = holt(air, alpha = .8, beta = .2, initial = "simple", exponential = TRUE, h = 13)

fitted(fit1h)
fit1h$mean

plot(air,ylab = "Airline Passegners", xlab = "Year", type = "o", xlim = c(1990, 2017),ylim = c(15,75))
lines(fitted(fit1h),col = "blue", type= "o")
lines(fitted(fit2h), col = "red", type= "o")
lines(fit1h$mean, col = "blue", type= "o")
lines(fit2h$mean,col = "red", type= "o")

fit3h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "simple", h = 13)
lines(fitted(fit3h), col = "darkgreen", type= "o")
lines(fit3h$mean,col = "darkgreen", type= "o")

fit4h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "simple", exponential = TRUE, h = 13)
lines(fitted(fit4h), col = "cyan", type= "o")
lines(fit4h$mean,col = "cyan", type= "o")


accuracy(fit1h, ausair)
accuracy(fit2h, ausair)
accuracy(fit3h, ausair)

points(ausair2, type = "o")

#Ensemble Model
lines((fit1h$mean+fit2h$mean+fit3h$mean)/3,col = "brown",type = "o")

lines((fit1h$mean+fit2h$mean)/2,col = "purple",type = "o")

#2.2 Plot it using dygraph
library(dygraphs)
ausair2 = read.csv("/Users/bivin/Desktop/OLD COMPUTER ARCHIVES/KadAfrica/MSDS/DDS/MSDS 6306/UNIT 12/ausair.csv",header = TRUE)
ausair2$TheYear2 = as.Date(ausair2$TheYear,format="%Y-%d-%m")
xss = xts(ausair2$Passengers,ausair2$TheYear2)
dygraph(xss) %>% dyRangeSelector(height = 100)






#3. Seasonal Trend

data("austourists")

plot(austourists)

aust = window(austourists,start = 1999, end = 2004)

fit1s = hw(aust,seasonal = "additive",h = 40)
fit2s = hw(aust,seasonal = "multiplicative",h = 40)

plot(aust,ylab = "Australian Tourists", xlab = "Year", type = "o", xlim = c(1999, 2014),ylim = c(15,60))
lines(fitted(fit1s),col = "blue", type= "o")
lines(fitted(fit2s), col = "red", type= "o")

lines(fit1s$mean, col = "blue", type= "o")
lines(fit2s$mean,col = "red", type= "o")

accuracy(fit1s,austourists)
accuracy(fit2s,austourists)
points(austourists, type = "o")




# Airline data from TSWGE ... Classic

airline = read.csv("/Users/bivin/Desktop/OLD COMPUTER ARCHIVES/KadAfrica/MSDS/DDS/MSDS 6306/UNIT 12/AirlinePasngr.csv",header = TRUE)

airlineTS = ts(airline$Passengers, start = c(1949,1),frequency=12)
airlineTS

airlineTS1 = window(airlineTS, end = 1957)

fit1s = hw(airlineTS1,seasonal = "additive",h = 35, alpha = .1)
fit2s = hw(airlineTS1,seasonal = "multiplicative",h = 35)

plot(airlineTS1,ylab = "Classic Airline Data", xlab = "Time", type = "o", xlim = c(1949,1961), ylim = c(100,500))
lines(fitted(fit1s),col = "blue", type= "o")
lines(fitted(fit2s), col = "red", type= "o")

lines(fit1s$mean, col = "purple", type= "o")
lines(fit2s$mean,col = "red", type= "o")

accuracy(fit1s,airlineTS)
accuracy(fit2s,airlineTS)

points(airlineTS, type = "o")


