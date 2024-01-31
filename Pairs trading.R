# Test Pairs Trading(21)
setwd('/Users/danilavtonoskin/Desktop/Study/3 Semester/Empirical finance/HW/Codes')
getwd()
# Loading libraries
library(tidyverse)
library(xts)
# load data
pairs <- read.csv('EMP_Finance_21_Avtonoshkin.csv',header = TRUE, sep = ';', dec = ',')
# convert data type into the right one
pairs$Date = as.Date(pairs$Date, format = '%Y-%m-%d')
pairs_xts <- xts(pairs[,2:3], order.by = pairs$Date)
class(pairs$Date)
# convert to a different date format
#pairs$Date <- format(pairs$Date, format = '%B %d %Y')
#head(pairs)
# building the graph
ggplot(pairs, aes(x = Date)) + geom_line(aes(y = SHEL, color = 'SHEL'))+ 
  geom_line(aes(y = BP, color = 'BP')) + labs(title = 'SHELL vs BP stock prices')+
  ylab('price in $') + theme(plot.title = element_text(hjust = 0.5)) 
# regress price of BP and SHELL on each other
ggplot(pairs, aes(x = BP, y = SHEL)) + geom_point(alpha = 0.5,col = 'black') + geom_smooth(method = lm, se = FALSE, col = 'darkred')
# Grenger Causality test
install.packages('lmtest')
library(lmtest)
names(pairs)
#ensure that series are stationary
#BP <- diff(pairs$BP)
#SHELL <- diff(pairs$SHEL)
# perform a granger causality test
grangertest(SHEL ~ BP, order = 3, data = pairs)
grangertest(BP ~ SHEL, order = 3, data = pairs)
# we can't reject a null hypothesis that neither Shell nor BP history prices predict future prices of each other
# now we will perform a Johansen test
install.packages('urca')
library(urca)
jotest <- ca.jo(pairs[,2:3], type = 'trace', K = 2, ecdet = 'none', spec = 'longrun')
summary(jotest)
# values or r<= 1 < 1pct -> no cointegration exists
# let's try Augmented Dickey-Fuller Test
install.packages('tseries')
library(tseries)
s <- 1.00000 * pairs$SHEL + 12.68817 * pairs$BP
#s
plot(s, type = 'l')
adf.test(s)
# p values is higher rhan 0.05 -> no signs of a cointegration
# linear regression with the lag of 1 year
a <- lm(pairs$SHEL ~ pairs$BP)
summary(a)
# alpha = 17.84926, beta = 1.16278, p value = 2.2e-16
# so formula: Shell prices - 17.84926 - 1.16278 * BP prices
# running window regression 1 day
head(pairs)
alphas <- c()
betas <- c()
i = 2
while(i <= length(pairs$Date)){
  reg <- lm(pairs$SHEL[1:i] ~ pairs$BP[1:i])
  alpha <- reg$coefficients[1]
  beta <- reg$coefficients[2]
  alphas <- rbind(alphas,alpha)
  betas <- rbind(betas, beta)
  i = i + 1
}
df <- cbind(alphas, betas)
df <- as.data.frame(df)
names(df) <- c('Alpha', 'Beta')
rownames(df) <- NULL
head(df)
# spread = Shell p(t1) - alpha(t0) - (BP p (t1) * b(t0)
nrow(pairs)
# drop the first row of a stock prices df to join both later
pairs <- pairs[-1,]
nrow(pairs) == nrow(df)
head(df)
pairs <- cbind(pairs, df$Alpha)
pairs <- cbind(pairs, df$Beta)
names(pairs)[4:5]<- c('Alpha', 'Beta')
# add a spread column
pairs <- pairs %>%
  mutate(Spread = lead(SHEL,1) - Alpha - (Beta * (lead(BP,1))))
head(pairs)
# trading graph
pairs %>%
  ggplot(aes(Date, Spread)) + geom_line(col = 'darkblue') +
  geom_abline(slope = 0, intercept = 1, color = 'red', size = 1.05) + 
  geom_abline(slope = 0, intercept = -1, color = 'green', size = 1.05) +
  geom_abline(slope = 0, intercept = 0, color = 'pink', linetype = 'dashed') +
  labs(title = 'Spread')+
  ylab('') + theme(plot.title = element_text(hjust = 0.5)) 
# crossing upwards the red line - time to sell
# crossing downwards the green light 









  





