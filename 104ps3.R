##Libraries
library(leaps) ##For Regsubsets
library(quantmod) ##For reading in data
library(stargazer) ##For producing nice latex plots
library(qcc)
library(strucchange) ##For MaxChow test
library(lmtest) ##For DW Test
library(normtest) ##For Jarque Bera
##Reading in the data
ecommerce = new.env()
getSymbols('ECOMNSA', src='FRED',env = ecommerce)
ecomnsa = ecommerce$ECOMNSA
ecomnsa = data.frame(ecomnsa)

##Dylan's Data Set
data2 = read.csv("C:/Users/Ben/Desktop/UPenn/UPenn Junior Year/Spring Semester/Econ 104/Problem Set 3/ps3data.csv")

##Building up dataframe
##Taking the log of the money data
ecomnsa = cbind(ecomnsa, log(ecomnsa$ECOMNSA))
colnames(ecomnsa) = c("Ecomnsa", "log(Ecomnsa)")
##Quarterly Seasonal Dummies
ecomnsa = cbind(ecomnsa, c(1:length(ecomnsa$Ecomnsa)), rep(0, length(ecomnsa$Ecomnsa)),
                rep(0, length(ecomnsa$Ecomnsa)), rep(0, length(ecomnsa$Ecomnsa)), rep(0, length(ecomnsa$Ecomnsa)))
colnames(ecomnsa)[3:7] = c("Time", "Q1", "Q2", "Q3", "Q4")
##Populating Quarters
for (i in 1:73) {
  if (i %% 4 ==  1) {
    ecomnsa$Q4[i] = 1
  } 
  if (i %% 4 == 2) {
    ecomnsa$Q1[i] = 1
  }
  if (i %% 4 == 3) {
    ecomnsa$Q2[i] = 1
  }
  if (i %% 4 == 0) {
    ecomnsa$Q3[i] = 1
  }
}
##Filling out Lagged Data Columns
for (i  in 1:10) {
  holder = rep(NA, length(ecomnsa$Ecomnsa))
  new = 73 - i
  new2 = 1 + i
  holder[c(new2:73)] = ecomnsa$`log(Ecomnsa)`[c(1:new)]
  ecomnsa = cbind(ecomnsa, holder)
  colnames(ecomnsa)[i + 7] = paste("Log(ecomnsa)_", i, "lag", sep = "")
}
#########################################################################
##Data Analysis
##Why we take logs/Basic Plot of Data
plot(y = ecomnsa$Ecomnsa, x = ecomnsa$Time, xlab = "Number of Quarters Since Q4 1999", ylab = "E-Commerce Sales in Millions of Dollars",
      main = "E-Commerce Sales by Quarter")
lines(y = ecomnsa$Ecomnsa, x = ecomnsa$Time, lwd = 2)

plot(y = ecomnsa$`log(Ecomnsa)`, x = ecomnsa$Time, xlab = "Number of Quarters Since Q4 1999", ylab = "ln(E-Commerce Sales)",
     main = "ln(E-Commerce Sales) by Quarter")
lines(y = ecomnsa$`log(Ecomnsa)`, x = ecomnsa$Time, lwd = 2) ##Clearly a much more linear trend

##Analysis of just linear trend
lintrend = lm(`log(Ecomnsa)`~ Time, data = ecomnsa)
summary(lintrend)
abline(lintrend$coefficients[1], lintrend$coefficients[2], col = "red", lwd = 2)
##stargazer(lintrend)
##Residual plot
plot(y = lintrend$residuals, x = ecomnsa$Time, main = "Residual Plot for Fitted Linear Trend", xlab = "Number of Quarters since Q4 1999", 
    ylab = "Residual Value")
abline(0, 0, col = "red", lwd = 2) ##There is clearly some higher order stuff going on, so we move to a non-linear trend.

##Analysis with a Quadratic Trend
ecomnsa = cbind(ecomnsa, ecomnsa$Time * ecomnsa$Time)
colnames(ecomnsa)[18] = c("Time2")

quadTrend = lm(`log(Ecomnsa)`~ Time + Time2, data = ecomnsa)
summary(quadTrend)
##Stargazer(quadTrend)
plot(y = ecomnsa$`log(Ecomnsa)`, x = ecomnsa$Time, xlab = "Number of Quarters Since Q4 1999", ylab = "ln(E-Commerce Sales)",
     main = "ln(E-Commerce Sales) by Quarter with Quadratic Model")
lines(y = ecomnsa$`log(Ecomnsa)`, x = ecomnsa$Time, lwd = 2)
lines(y = quadTrend$fitted.values, x = ecomnsa$Time, lwd = 2, col = "red")

##Residual plot
plot(y = quadTrend$residuals, x = ecomnsa$Time, main = "Residual Plot for Fitted Quadratic Trend", xlab = "Number of Quarters since Q4 1999", 
     ylab = "Residual Value")
abline(0, 0, col = "red", lwd = 2) ##Strong Evidence of structural change around the financial crisis,
                                  ##but first let us deal with clear issues of seasonality
##Analysis with Seasonality
seasonalQuad = lm(`log(Ecomnsa)`~ 0 + Time + Time2 + Q1 + Q2 + Q3 + Q4, data = ecomnsa)
summary(seasonalQuad)
##Stargazer(seasonalQuad)
plot(y = ecomnsa$`log(Ecomnsa)`, x = ecomnsa$Time, xlab = "Number of Quarters Since Q4 1999", ylab = "ln(E-Commerce Sales)",
     main = "ln(E-Commerce Sales) by Quarter with Quadratic Trend and Seasonality")
lines(y = ecomnsa$`log(Ecomnsa)`, x = ecomnsa$Time, lwd = 2)
lines(y = seasonalQuad$fitted.values, x = ecomnsa$Time, lwd = 2, col = "red")

##Residual Plot
plot(y = seasonalQuad$residuals, x = ecomnsa$Time, main = "Residual Plot for Quadratic Trend and Seasonailty",
     xlab = "Number of Quarters since Q4 1999", ylab = "Residual Value")
abline(0, 0, col = "red", lwd = 2) ##Very large evidence of a structural break in the residual plot around the 2008 financial crisis
##As defined by the Federal Government, this recession lasted from Q4 2007 to Q2 2009. On our chart this is 33-39

##Evidence of a structural break
##Recursive Residuals Test
recurseResidual = matrix(1, 73)
recurseResidual[1:10] = 0
##We start after 10 quarters of realizations.
for (i in 11:72) {
  tempFrame = ecomnsa[c(1:i),]
  tempModel = lm(`log(Ecomnsa)`~ 0 + Time + Time2 + Q1 + Q2 + Q3 + Q4, data = tempFrame)
  prediction = predict(object = tempModel, newdata = ecomnsa[i+1,])
  print(prediction)
  residual = prediction - ecomnsa$`log(Ecomnsa)`[i + 1]
  recurseResidual[i] = residual
}
plot(y = recurseResidual, x = ecomnsa$Time, main = "Recursive Residual Plot-Starts from Quarter #11",
     xlab = "Number of Quarters since Q4 1999", ylab = "Residual Value")
abline(0, 0, col = "red", lwd = 2)
abline(v = 33, col = "red", lwd = 2) ## Right at the first quarter of the financial crisis, and a positive residual!

##Max Chow Test
fs = Fstats(seasonalQuad, from = 11, to = 63, data = ecomnsa)
which.max(fs$Fstats) + 10 ##Quarter number 36
sctest(seasonalQuad, type = "expF", from = 10, to = 63, data = ecomnsa) ##Statistically significant Highest chow test value is q36,
                                                                        ##which is right when suspicious things begin in our recurssive
                                                                        ##Residual plot, and we have subject matter knowledge of the recession
##Splitting the data at Quarter #36:
dummy36 = ifelse(ecomnsa$Time > 36, yes = 1, no = 0)
for (i in c(3:7, 18)) {
ecomnsa = cbind(ecomnsa, ecomnsa[i] * dummy36)
}
colnames(ecomnsa)[19:24] = c("Timep2", "Q1p2", "Q2p2", "Q3p2", "Q4p2", "Time2p2")

##Fitting the new, structural-change-accounted-for model
structChange = lm(`log(Ecomnsa)`~ 0 + Time + Time2 + Q1 + Q2 + Q3 + Q4 + Timep2 +
                    Q1p2 + Q2p2 + Q3p2 + Q4p2 + Time2p2, data = ecomnsa)
summary(structChange)
##Stargazer(structChange)
plot(y = ecomnsa$`log(Ecomnsa)`, x = ecomnsa$Time, xlab = "Number of Quarters Since Q4 1999", ylab = "ln(E-Commerce Sales)",
     main = "Quarterly ln(E-Commerce Sales) with Structural Change at Quarter 36")
lines(y = ecomnsa$`log(Ecomnsa)`, x = ecomnsa$Time, lwd = 2)
lines(y = structChange$fitted.values, x = ecomnsa$Time, lwd = 2, col = "red")

plot(y = structChange$residuals, x = ecomnsa$Time, main = "Residual Plot for Structural Change Model",
     xlab = "Number of Quarters since Q4 1999", ylab = "Residual Value")
abline(0, 0, col = "red", lwd = 2) ##Evidence here of serial correlation, long swoops of residuals above and below a White Noise Mean of 0 

##Evidence of serial correlation and tests
##Durbin Watson
##Lots of 1st order serial correlation
dwtest(formula = `log(Ecomnsa)`~ 0 + Time + Time2 + Q1 + Q2 + Q3 + Q4 + Timep2 +
         Q1p2 + Q2p2 + Q3p2 + Q4p2 + Time2p2, data = ecomnsa, alternative = "two.sided")
##P-value of 8.394 * 10 ^-9
##Correllegram
acf(structChange$residuals, main = "Auto-Correlation Plot") ## No evidence of a greater than AR(2) process
pacf(structChange$residuals, main = "Partial Auto-Correlation Plot")

##Testing Ar(2) and AR(3) Serial correlation with Breusch-Godfrey
bgtest(order = 2, formula = `log(Ecomnsa)`~ 0 + Time + Time2 + Q1 + Q2 + Q3 + Q4 + Timep2 +
         Q1p2 + Q2p2 + Q3p2 + Q4p2 + Time2p2, data = ecomnsa, type = "Chisq")
##P-Value of 0.0002222
bgtest(order = 3, formula = `log(Ecomnsa)`~ 0 + Time + Time2 + Q1 + Q2 + Q3 + Q4 + Timep2 +
         Q1p2 + Q2p2 + Q3p2 + Q4p2 + Time2p2, data = ecomnsa, type = "Chisq")
##p-value of 0.000102

##Now running with three lags
finalModel = lm(`log(Ecomnsa)`~ 0 + Time + Time2 + Q1 + Q2 + Q3 + Q4 + Timep2 +
                  Q1p2 + Q2p2 + Q3p2 + Q4p2 + Time2p2 + `Log(ecomnsa)_1lag` + 
                  `Log(ecomnsa)_2lag` + `Log(ecomnsa)_3lag`, data = ecomnsa[3:73,])
summary(finalModel)
##StarGazer(finalModel)

plot(y = ecomnsa$`log(Ecomnsa)`[4:73], x = ecomnsa$Time[4:73], xlab = "Number of Quarters Since Q4 1999", ylab = "ln(E-Commerce Sales)",
     main = "Quarterly ln(E-Commerce Sales) with AR(3)--Full Model")
lines(y = ecomnsa$`log(Ecomnsa)`[4:73], x = ecomnsa$Time[4:73], lwd = 2)
lines(y = finalModel$fitted.values, x = ecomnsa$Time[4:73], lwd = 2, col = "red")

plot(y = finalModel$residuals, x = ecomnsa$Time[4:73], main = "Residual Plot for Full Model with AR(3)",
     xlab = "Number of Quarters since Q4 1999", ylab = "Residual Value")
abline(0, 0, col = "red", lwd = 2) ##A truly gorgeous residual plot

##Normality of residuals in final model
hist(finalModel$residuals, main = "Histrogram of Residuals in Final Model", xlab = "Residual Value", ylab = "Frequency")
jb.norm.test(finalModel$residuals, nrepl = 2000) ##All set! P-value approx. 0.885

