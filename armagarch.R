library(forecast)
library(aTSA)
library(fGarch)
library(urca)

options(warn=-1)


# Data ----------------------------------------------------------------------------------------

stock = 'BRK-B'
csv_in = paste(c('data/',stock,'.csv'), collapse = "")
csv_out = paste(c('data/',tolower(stock),'_ag.csv'), collapse = "")

df = read.csv(csv_in, header=TRUE)
df = subset(df, subset=Close != "null")
df$Close = as.numeric(df$Close)

logreturns = diff(log(df$Close), lag=1)
df$logreturns <- 0
df$logreturns <- c(0, logreturns)

test_size = 253 # one year
train_size = nrow(df) - test_size
train = df[1:train_size,]
lr_train = train$logreturns
test = df[(train_size+1): nrow(df),]


# Tests ------------------------------------------------------------------------------------

# Plot
ts.plot(lr_train, xlab = "Day", ylab = "Closing price")
abline(a = 0, 0, col = "red")

# Stationarity
adf_auto = ur.df(lr_train, selectlags = "AIC") # 1 lag
adf.test(lr_train, nlag=1) # => stationary

# Heteroskedasticity
auto_arima = auto.arima(lr_train, ic = "bic", seasonal = FALSE)
arimaorder(auto_arima)
arima = arima(lr_train, c(2, 0, 0)) #  inserting into model (needed for arch.test)

arch.test(arima, output = TRUE) #  => heteroskedasticity

# Fitting ARMA-GARCH --------------------------------------------------------------------------

fit_ics = function(p, q, x, y) {
  model = garchFit(substitute(~ arma(p, q) + garch(x, y),
                              list(
                                p = p,
                                q = q,
                                x = x,
                                y = y
                              )),
                   data = lr_train,
                   trace = FALSE)
  print(model@fit$ics)
}

fit_ics(0,0,1,1)
fit_ics(0,1,1,1)
fit_ics(1,0,1,1)
fit_ics(1,1,1,1)
fit_ics(2,0,1,1)

# Best model, from above
arma_garch = garchFit(
  formula = ~ arma(1, 1) + garch(1, 1),
  data = lr_train,
  trace = FALSE
)
summary(arma_garch)

qqnorm(arma_garch@residuals)
qqline(arma_garch@residuals)
plot(density(arma_garch@residuals))

#* Ljung-Box test show independent residuals
#* LM ARCH test show no signs of heteroskedasticity left
#* However, shapiro wilks, qqnorm and density plot doesn't suggest normality in residuals
#* Modelling the cond.dist with t-distribution doesn't help either. 
#* Ignoring normality for now, just note that model may not be at its most optimal



# Cross validating ----------------------------------------------------------------------------------

last_train = tail(train,1)

date = c(last_train$Date)
close = c(last_train$Close)
forecast = c(last_train$Close)

i = 1
while (i <= test_size) {
  train_size_cross = train_size + i - 1
  train_cross = df[1:train_size_cross, ]
  test_cross = df[(train_size_cross+1) : nrow(df), ]
  
  lr_train_cross = train_cross$logreturns
  
  arma_garch = garchFit(
    formula = ~ arma(1, 1) + garch(1, 1),
    data = lr_train_cross,
    trace = FALSE
  )
  
  arma_pred = predict(arma_garch, n.ahead = 1, plot=TRUE, confint=0.7)
  last_train_cross = tail(train_cross, 1)$Close
  forecasted_price = exp(cumsum(arma_pred$meanForecast) + log(last_train_cross))

  date = c(date, head(test_cross, 1)$Date)
  close = c(close, head(test_cross,1)$Close)
  forecast = c(forecast, forecasted_price)
  
  print(paste(i, "of", test_size))
  
  i = i+1
}

df_cross = data.frame(date, close, forecast)
head(df_cross)

write.csv(df_cross, csv_out, row.names=FALSE)
