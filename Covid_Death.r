####read data####
dead = read.csv(file.choose())
dead[153:432,4]
#dead[153:432,2]
d = data.frame(dead[153:432,2],dead[153:432,4])
View(d)
data = d[,2]
data = data[-c(23,69)]
####plot data####
ts.plot(data)

####model####
t = 1:length(data)
t2 = t ^ 2
model = lm(data ~ t + t2)
summary(model)
resids = residuals(model)
fits = fitted(model)

####plots####
hist(resids)
qqnorm(resids)
qqline(resids)
plot(resids ~ fits)
plot(resids ~ t, type = 'o')

####norm test####
shapiro.test(resids)
library(nortest)
ad.test(resids)

####runs test####
library(randtests)
runs.test(resids)

####variance test####
fac = factor(rep(1:23, each = 12))
fac = c(fac , c(23,23))
#fac = c(fac , c(36,36,36))
library(car)
leveneTest(resids, group = fac)

####variance test####
fac = factor(rep(1:23, each = 12))
fac = c(fac , c(23,23))
#fac = c(fac , c(36,36,36))
library(car)
leveneTest(data, group = fac)

####box-cox####
t = 1:length(data)
library(MASS)
library(car)
data = data + 0.01
boxCox(data ~ t, lambda = seq(from = -2, to = 2, by = .1))
data.new = (data^0.5-1)/0.5
leveneTest(data.new, group = fac)

####mean test####
kruskal.test(data.new, g = fac)
data.new.diff = diff(data.new)
fac.diff = fac[-1]
kruskal.test(data.new.diff, g = fac.diff)
leveneTest(data.new.diff, group = fac.diff)

####ACF-PACF####
acf(data.new.diff)
pacf(data.new.diff)

####model####
y = ts(data = data.new)
order = c(2, 1, 3)
library(forecast)
fit <- Arima(y = y, order = order)
summary(fit)
####forecast####
plot(forecast(fit, h = 20))






