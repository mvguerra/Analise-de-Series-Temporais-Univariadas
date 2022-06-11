rm(list=ls())
graphics.off()

library(urca)
library(forecast)
library(astsa)
library(tseries)
library(TSstudio)

# Importação da Série Original
df <- read.csv("C:/Users/mvrgu/OneDrive/Documentos/Scanned Documents/Mestrado/Estatística Econômica Aplicada/data_MarcusGuerra.csv", header = T,sep = ",", dec = ".")

# Gráfico da Série Original
plot(df[,1], df[,2], type = "l", col = "black", lty=1, main="Série Original", xlab="Observações", ylab="Valores", cex.main=1, cex.lab=1, font.main=1, font.lab=1)

# Correlogramas da Série Original
acf2(df[,2], max.lag=60, main="Correlogramas da Série Original", cex.main=1, cex.lab=1, font.main=1, font.lab=1)

# ANÁLISE: A ACF da Série Original sugere uma série não estacionária com sazonalidade de 12 períodos.

# Decomposição da Série Original
df1 <- ts(df, frequency=12)
dfcomponents1 <- stl(df1[,2], t.window=13, s.window="periodic", robust=TRUE)
plot(dfcomponents1, main="Decomposição da Série Original", cex.main=1, cex.lab=1, font.main=1, font.lab=1)

# ANÁLISE: A decomposição da Série Original (com frequência de 12 períodos) sugere presença de tendência.

# DECISÃO: Tirar a Primeira Diferença da Série Original.

# Primeira Diferença da Série Original
df.first_diff <- diff(df[,2])

# Gráfico da Primeira diferença da Série Original
plot(df.first_diff, type = "l", col = "black", lty=1, main="Primeira Diferença da Série Original", xlab="Observações", ylab="Valores", cex.main=1, cex.lab=1, font.main=1, font.lab=1)

# Correlogramas da Primeira Diferença da Série Original
acf2(df.first_diff, max.lag=60, main="Correlogramas da Primeira Diferença da Série Original", cex.main=1, cex.lab=1, font.main=1, font.lab=1)

# ANÁLISE: A ACF da Primeira Diferença da Série Original não alterou muito a situação.
# Continua sugerindo uma série não estacionária com sazonalidade de 12 períodos.

# Decomposição da Primeira Diferença da Série Original
df1.first_diff <- diff(df1[,2])
dfcomponents2 <- stl(df1.first_diff, t.window=13, s.window="periodic", robust=TRUE)
plot(dfcomponents2, main="Decomposição da Primeira Diferença da Série Original", cex.main=1, cex.lab=1, font.main=1, font.lab=1)

# ANÁLISE: A decomposição da Primeira Diferença da Série Original (com frequência de 12 períodos) não alterou muito a situação.
# Continua sugerindo a presença de tendência.

# DECISÃO: Descartar a Primeira Diferença da Série Original.
#Tirar a Primeira Diferença Sazonal de Ordem 12 da Série Original.

# Primeira Diferença Sazonal de Ordem 12 da Série Original
df.first_sdiff12 <- diff(df[,2],12)

# Gráfico da Primeira Diferença Sazonal de Ordem 12 da Série Original
plot(df.first_sdiff12, type = "l", col = "black", lty=1, main="Primeira Diferença Sazonal de Ordem 12 da Série Original", xlab="Observações", ylab="Valores", cex.main=1, cex.lab=1, font.main=1, font.lab=1)

# Correlogramas da Primeira Diferença Sazonal de Ordem 12 da Série Original
acf2(df.first_sdiff12, max.lag=60, main="Correlogramas da Primeira Diferença Sazonal de Ordem 12 da Série Original", cex.main=1, cex.lab=1, font.main=1, font.lab=1) 

# ANÁLISE: A ACF da Primeira Diferença Sazonal de Ordem 12 da Série Original sugere uma série estacionária com sazonalidade de 12 períodos.

# Decomposição da Primeira Diferença Sazonal de Ordem 12 da Série Original
df1.first_sdiff12 <- diff(df1[,2],12)
dfcomponents3 <- stl(df1.first_sdiff12, t.window=13, s.window="periodic", robust=TRUE)
plot(dfcomponents3, main="Decomposição da Primeira Diferença Sazonal de Ordem 12 da Série Original", cex.main=1, cex.lab=1, font.main=1, font.lab=1)

# ANÁLISE: A decomposição da Primeira Diferença Sazonal de Ordem 12 da Série Original (com frequência de 12 períodos) sugere ausença de tendência.

# Teste ADF de raiz unitária - da Primeira Diferença Sazonal de Ordem 12 da Série Original
test.adf <- ur.df(df1.first_sdiff12, lags = 60, type = "drift", selectlags = "BIC")
summary(test.adf)

# ANÁLISE: # O valor da estatistíca -13.7803 é menor que o nível crítico (tau2) -2.87 ao nível de significância de 5%.
# P-valor é menor que o nível de significância de 5% (0.05).
# Pode-se concluir pela rejeição da hipótese nula (que a série possui raiz unitária).
# Portanto, segundo o teste ADF, rejeita-se a hipótese que a série não é estacionária.

# Teste KPSS de raiz unitária - da Primeira Diferença Sazonal de Ordem 12 da Série Original
test.kpss <- ur.kpss(df1.first_sdiff12, type = "mu", use.lag=NULL)
summary(test.kpss)
kpss.test(df1.first_sdiff12)

# ANÁLISE: # O valor da estatistíca 0.3776 é menor que o nível crítico 0.463 ao nível de significância de 5%.
# P-valor é maior que o nível de significância de 5% (0.05).
# Pode-se concluir pela não rejeição da hipótese nula (que a série não possui raiz unitária).
# Portanto, segundo o teste KPSS, não se rejeita a hipótese que a série é estacionária.

# DECISÃO: Utilizar a Primeira Diferença Sazonal de Ordem 12 da Série Original para identificar a ordem mais ampla do modelo SARIMA.

# Identificação da ordem mais ampla do modelo
# A FAC (da Primeira Diferença Sazonal de Ordem 12 da Série Original) sugere - conservadoramente - MA(0) para a parte regular e MA(4)para a parte sazonal (q=0 e Q=4).
# A FACP (da Primeira Diferença Sazonal de Ordem 12 da Série Original) sugere - conservadoramente - AR(0) para a parte regular e AR(4) para a parte sazonal (p=0 e P=4).
# Não foi aplicada a primeira diferença (d=0).
# Foi aplicada a primeira diferença sazonal de ordem 12 (D=1).

# DECISÃO: Sugere-se a ordem mais ampla do modelo como SARIMA(0,0,0)(4,1,4)12.

(fit1 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(4, 1, 4))) #BIC=4674.81
(fit2 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(4, 1, 3))) #BIC=4667.04
(fit3 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(4, 1, 2))) #BIC=4661.34
(fit4 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(4, 1, 1))) #BIC=4662.28
(fit5 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(4, 1, 0))) #BIC=4660.78
(fit6 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(3, 1, 4))) #BIC=4670.27
(fit7 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(3, 1, 3))) #BIC=4665.05
(fit8 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(3, 1, 2))) #BIC=4657.67
(fit9 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(3, 1, 1))) #BIC=4660.91
(fit10 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(3, 1, 0))) #BIC=4654.80
(fit11 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(2, 1, 4))) #BIC=4662.12
(fit12 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(2, 1, 3))) #BIC=4657.93
(fit13 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(2, 1, 2))) #BIC=4652.76
(fit14 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(2, 1, 1))) #BIC=4656.39
(fit15 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(2, 1, 0))) #BIC=4652.78
(fit16 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(1, 1, 4))) #BIC=4661.00
(fit17 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(1, 1, 3))) #BIC=4656.10
(fit18 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(1, 1, 2))) #BIC=4653.50
(fit19 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(1, 1, 1))) #BIC=4657.74
(fit20 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(1, 1, 0))) #BIC=4665.31
(fit21 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(0, 1, 4))) #BIC=4654.83
(fit22 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(0, 1, 3))) #BIC=4652.87
(fit23 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(0, 1, 2))) #BIC=4658.92
(fit24 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(0, 1, 1))) #BIC=4668.65
(fit25 <- Arima(df1[,2], order = c(0, 0, 0), seasonal = c(0, 1, 0))) #BIC=4671.14

# ANÁLISE: Pela função Arima(), O menor BIC (4652.76) é o SARIMA(0,0,0)(2,1,2)12. BIC=4652.76

fit26 <- auto.arima(df1[,2], p=0, d=0, max.q=0, max.P=4, D=1, max.Q=4, stepwise = FALSE, max.order = 8, approximation = FALSE, test = "adf", ic = "bic", trace = TRUE)
summary(fit26)

# ANÁLISE: Pela função auto.arima(), o menor BIC também é SARIMA(0,0,0)(2,1,2)12. BIC=4652.76

# Verificação das condições de estacionariedade e invertibilidade
autoplot(fit13)

# ANÁLISE: No caso do SARIMA(0,0,0)(2,1,2)12, as raízes inversas das equações características dos polinômios (autorregressivo sazonal e de médias móveis sazonal) estão dentro dos círculos unitários.
# Logo, as condições de estacionariedade e invertibilidade estão satisfeitas.

# DECISÃO: Sugere-se a ordem mais adequada do modelo como SARIMA(0,0,0)(2,1,2)12.

# Correlogramas dos resíduos do modelo selecionado
acf2(resid(fit13), max.lag=60,  cex.main=1, cex.lab=1, font.main=1, font.lab=1)

# ANÁLISE: Quase todas as autocorrelações dos resíduos estão dentro dos limites - indicando que os resíduos se comportam como um ruído branco.

# Teste Ljung-Box dos resíduos do modelo selecionado
checkresiduals(fit13, test="LB")

# ANÁLISE: P-valor é maior que o nível de significância de 5% (0.05).
# Pode-se concluir pela não rejeição da hipótese nula (que os resíduos são indepedentementes distribuídos).
# Portanto, segundo o teste Ljung-Box, não se rejeita a hipótese que os resíduos se comportam como um ruído branco.

# Teste Jarque-Bera dos resíduos do modelo selecionado
jarque.bera.test(residuals(fit13))

# ANÁLISE: P-valor é maior que o nível de significância de 5% (0.05).
# Pode-se concluir pela não rejeição da hipótese nula (que os resíduos são normalmente distribuídos).
# Portanto, segundo o teste Jarque-Bera, não se rejeita a hipótese que os resíduos se comportam como um ruído branco.

# DECISÃO: A avaliação dos resíduos do modelo selecionado indica que os resíduos se comportam como um ruído branco. Segue-se para o forecast.

# Splits / Série de treinamento (476 observações) / Série de teste (24 observações / frequência = 12 x maior AR/MA da ordem selecionada mais adequada do modelo SARIMA = 2)
df_split <- ts_split(df1[,2], sample.out = 24)
df_train <- df_split$train
df_test <- df_split$test

# Modelos para forecast

# Critérios para avaliação do melhor modelo:
# Menor BIC (Bayesian Information Criterion) ou "Critério de Informação Bayesiano".
# Menor RMSE (Root Mean Squared Error) ou "Raiz Quadrada do Erro Médio".
# Menor MAPE (Mean Absolute Percentage Error) ou "Média Percentual Absoluta do Erro".
# Menor Theil's U ou "U de Theil".

# 1) SARIMA(0,0,0)(2,1,2)12 - ordem (selecionada) mais adequada do modelo SARIMA.

m1 <- (fit_train <- Arima(df_train, order = c(0, 0, 0), seasonal = c(2, 1, 2)))
summary(m1) 
BIC(m1) #BIC=4421.2
forecast.m1 <- forecast(m1, h=24)
accuracy(forecast(m1, h=24), df_test) #Theil's U=0.05631508 / RMSE 26.98768 / MAPE 4.360334 
checkresiduals(m1, test="LB") #P-valor=0.671
acf2(resid(m1), max.lag=60) #ACF OK
jarque.bera.test(residuals(m1)) #P-valor=0.1638

# ANÁLISE: Quase todas as autocorrelações dos resíduos estão dentro dos limites - indicando que os resíduos se comportam como um ruído branco.
# P-valor do teste Ljung-Box é maior que o nível de significância de 5% (0.05).
# Pode-se concluir pela não rejeição da hipótese nula (que os resíduos são indepedentementes distribuídos).
# Portanto, segundo o teste ljung-Box, não se rejeita a hipótese que os resíduos se comportam como um ruído branco.
# P-valor do teste Jarque-Bera é maior que o nível de significância de 5% (0.05).
# Pode-se concluir pela não rejeição da hipótese nula (que os resíduos são normalmente distribuídos).
# Portanto, segundo o teste Jarque-Bera, não se rejeita a hipótese que os resíduos se comportam como um ruído branco.

# 2) ETS (suavização exponencial com tendência e sazonalidade)

m2 <- ets(df_train)
summary(m2) 
BIC(m2) #BIC=6211.15
forecast.m2 <- forecast(m2, h=24)
accuracy(forecast(m2, h=24), df_test) #Theil's U=0.05650729 / RMSE 28.34537 / MAPE 4.6632
checkresiduals(m2, test="LB") #P-valor=0.0000496
acf2(resid(m2), max.lag=60) #ACF Não

# ANÁLISE: As autocorrelações dos resíduos mostram decaimento exponencial sazonal - indicando que os resíduos não se comportam como um ruído branco.
# P-valor do teste Ljung-Box é menor que o nível de significância de 5% (0.05).
# Pode-se concluir pela rejeição da hipótese nula (que os resíduos são indepedentementes distribuídos).
# Portanto, segundo o teste Ljung-Box, rejeita-se a hipótese que os resíduos se comportam como um ruído branco.

# 3) TSLM (linear com tendência e sazonalidade)

m3 <- tslm(df_train ~ season + trend) 
summary(m3) 
BIC(m3) #BIC=5445.32
forecast.m3 <- forecast(m3, h=24)
accuracy(forecast(m3, h=24), df_test) #Theil's U=0.1503092 / RMSE=67.37515 / MAPE=11.87971    
checkresiduals(m3, test="LB") #P-valor=2.2e-16
acf2(resid(m3), max.lag=60) #ACF Não

# ANÁLISE: As autocorrelações dos resíduos mostram decaimento exponencial sazonal - indicando que os resíduos não se comportam como um ruído branco.
# P-valor do teste Ljung-Box é menor que o nível de significância de 5% (0.05).
# Pode-se concluir pela rejeição da hipótese nula (que os resíduos são indepedentementes distribuídos).
# Portanto, segundo o teste Ljung-Box, rejeita-se a hipótese que os resíduos se comportam como um ruído branco.

# 4) Holt-Winters (suavização exponencial tripla)

m4 <- holt(df_train, h = 24)
summary(m4) #BIC=8004.204
forecast.m4 <- forecast(m4, h=24)
accuracy(forecast(m4, h=24), df_test) #Theil's U=0.3800267 / RMSE=198.8771 / MAPE=40.1806 
checkresiduals(m4, test="LB") #P-valor=2.2e-16
acf2(resid(m4), max.lag=60) #ACF Não

# ANÁLISE: As autocorrelações dos resíduos mostram decaimento exponencial sazonal - indicando que os resíduos não se comportam como um ruído branco.
# P-valor do teste Ljung-Box é menor que o nível de significância de 5% (0.05).
# Pode-se concluir pela rejeição da hipótese nula (que os resíduos são indepedentementes distribuídos).
# Portanto, segundo o teste Ljung-Box, rejeita-se a hipótese que os resíduos se comportam como um ruído branco.

# 5) Holt-Winters com damped (suavização exponencial tripla - amortecida)

m5 <- holt(df_train, damped = TRUE, h = 24)
summary(m5) #BIC=8001.306
forecast.m5 <- forecast(m5, h=24)
accuracy(forecast(m5, h=24), df_test) #Theil's U=0.375986 / RMSE=196.9927 / MAPE=40.11021  
checkresiduals(m5, test="LB") #P-valor=2.2e-16
acf2(resid(m5), max.lag=60) #ACF Não

# ANÁLISE: As autocorrelações dos resíduos mostram decaimento exponencial sazonal - indicando que os resíduos não se comportam como um ruído branco.
# P-valor do teste Ljung-Box é menor que o nível de significância de 5% (0.05).
# Pode-se concluir pela rejeição da hipótese nula (que os resíduos são indepedentementes distribuídos).
# Portanto, segundo o teste Ljung-Box, rejeita-se a hipótese que os resíduos se comportam como um ruído branco.

# 6) SES (suavização exponencial simples)

m6 <- ses(df_train, h = 24)
summary(m6) #BIC=7987.527
forecast.m6 <- forecast(m6, h=24)
accuracy(forecast(m6, h=24), df_test) #Theil's U=.3693487 / RMSE=197.9712 / MAPE=40.46585 
checkresiduals(m6, test="LB") #P-valor=2.2e-16
acf2(resid(m6), max.lag=60) #ACF Não

# ANÁLISE: As autocorrelações dos resíduos mostram decaimento exponencial sazonal - indicando que os resíduos não se comportam como um ruído branco.
# P-valor do teste Ljung-Box é menor que o nível de significância de 5% (0.05).
# Pode-se concluir pela rejeição da hipótese nula (que os resíduos são indepedentementes distribuídos).
# Portanto, segundo o teste Ljung-Box, rejeita-se a hipótese que os resíduos se comportam como um ruído branco.

# 7) NAIVE (modelo "ingênuo" / passeio aleatório com tendência)

m7 <- naive(df_train, h = 24)
summary(m7) 
forecast.m7 <- forecast(m7, h=24)
accuracy(forecast(m7, h=24), df_test) #Theil's U=0.359072 / RMSE=306.4320 / MAPE=62.66369 
checkresiduals(m7, test="LB") #P-valor=2.2e-16
acf2(resid(m7), max.lag=60) #ACF Não

# ANÁLISE: As autocorrelações dos resíduos mostram decaimento exponencial sazonal - indicando que os resíduos não se comportam como um ruído branco.
# P-valor do teste Ljung-Box é menor que o nível de significância de 5% (0.05).
# Pode-se concluir pela rejeição da hipótese nula (que os resíduos são indepedentementes distribuídos).
# Portanto, segundo o teste Ljung-Box, rejeita-se a hipótese que os resíduos se comportam como um ruído branco.

# SARIMA(0,0,0)(2,1,2)12 teve melhor performance / menor BIC, RMSE, MAPE e Theil's U.
# ETS teve a segunda melhor performance / terceiro menor BIC e segundo menor RMSE, MAPE e Theil's U.
# TSLM teve a terceira melhor performance / segundo menor BIC e terceiro menor RMSE, MAPE e Theil's U.

# Forecast de SARIMA(0,0,0)(2,1,2)12

autoplot(forecast(m1, h = 24), include = 500) +
  autolayer(df_test)

autoplot(forecast(m1, h = 24), include = 50) +
  autolayer(df_test)

# Forecast de ETS - que tem parecidos RMSE, MAPE e Theil's U, mas não passou no teste Ljung-Box de distribuição independente dos resíduos.

autoplot(forecast(m2, h = 24), include = 500) +
  autolayer(df_test) 

autoplot(forecast(m2, h = 24), include = 50) +
  autolayer(df_test) 

# Forecast de TSLM - que tem o segundo menor BIC, mas não passou no teste Ljung-Box de distribuição independente dos resíduos.

autoplot(forecast(m3, h = 24), include = 500) +
  autolayer(df_test) 

autoplot(forecast(m3, h = 24), include = 50) +
  autolayer(df_test) 
