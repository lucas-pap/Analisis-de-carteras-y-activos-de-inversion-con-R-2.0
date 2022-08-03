####################################################################################################
##                      Análisis de carteras y activos de inversión con R                         ##
##                                                                                                ##
####################################################################################################


rm(list = ls())

install.packages(c('tseries',"tidyquant","fimport","fPortfolio","gtools",'ggplot2','tidyr',
                   'purrr','dygraphs',"PerformanceAnalytics",'quantmod','tidyverse',
                   'vars','corrplot','scales'))

library(quantmod)
library(tidyquant)
library(tidyverse)
library(tseries)
library(gtools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fPortfolio)
library(purrr)
library(dygraphs)
library(PerformanceAnalytics)
library(fGarch)
library(aTSA)
library(FinTS)
library(rugarch)
library(vars)
library(corrplot)
library(scales)


options(scipen = 999) #Para sacar la notacion cientifica


### Importamos los datos de las acciones que decidimos analizar.
### Por cuestiones de comodidad, creamos 3 Data frames distintos con los mismos datos:

tickers = c('AAL','GE','CCL','AAPL','F','AMD','INTC','MELI',
            'DIS','FB','ORCL','MSFT','JPM','V','NKE','WST')

tickers = tickers[c(order(tickers))]

precios = tq_get(tickers,get = 'stock.prices',from = "2019-01-01",to = "2020-01-01")
precios = precios[,c('symbol','date','adjusted')]

precios2 = spread(precios,symbol,adjusted)


## Serie de tiempo:
precios_ts = xts(precios2[,2:ncol(precios2)],precios2$date)


## Rendimiento de mercado:
GSPC = getSymbols('^GSPC',from = '2018-01-01', to = '2020-01-01',auto.assign = FALSE)
GSPC = GSPC$GSPC.Adjusted

retornos_sp500 = na.omit(ROC(GSPC))

rm = mean(na.omit(retornos_sp500))
varm = var(na.omit(retornos_sp500))


### Graficamos la evolucion de los precios de cada acción

precios %>%
  ggplot(aes(date,adjusted,color = symbol)) +
  geom_line(show.legend = F) +
  facet_wrap(~symbol,scales = 'free_y') +
  labs(x='Fecha',y = 'Precio Ajustado',title = 'Grafico 1: Camino de precios observados',
       caption = 'Fuente: elaboración propia.') +
  scale_x_date(date_breaks = '3 months',
               date_labels = '%b\n%y') +
  theme(plot.caption = element_text(size=8, hjust=0, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5))


### Calculamos los retornos logaritmicos diarios, mensuales y anuales

retornos_d = na.omit(ROC(precios_ts))
colnames(retornos_d) = tickers

retornos_m = na.omit(ROC(precios_ts, n = 22))
colnames(retornos_m) = tickers

retornos_dts = as.timeSeries(retornos_d)
retornos_mts = as.timeSeries(retornos_m)


### Graficos de los retornos

plot_retm = gather(cbind(as.data.frame(retornos_m),date = index(retornos_m)),symbol,retornos,-date)
plot_retd = gather(cbind(as.data.frame(retornos_d),date = index(retornos_d)),symbol,retornos,-date)

# Diarios
plot_retd %>%
  ggplot(aes(date,retornos,color = symbol)) +
  geom_line(show.legend = F) +
  geom_hline(yintercept = 0) +
  facet_wrap(~symbol,scales = 'free_y') +
  labs(x = 'Fecha', y = 'Retornos diarios', title = 'Gráfico 2: Retornos Diarios',
       caption = 'Fuente: Elaboración propia.') +
  scale_x_date(date_breaks = '3 months', date_labels = '%b\n%y') +
  theme(plot.caption = element_text(size=8, hjust=0, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5))

plot_retd %>%
  ggplot(aes(retornos,y = ..density..,color = symbol)) +
  geom_histogram(show.legend = F,bins = 15) +
  facet_wrap(~symbol,scales = 'free') +
  labs(x = 'Retornos', y = 'Retornos diarios (Densidad)', title = 'Gráfico 3: Histograma de Retornos Diarios',
       caption = 'Fuente: Elaboración propia.') +
  theme(plot.caption = element_text(size=8, hjust=0, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 8,angle = -90,vjust = 0.5,hjust = 0.5))

# Mensuales
plot_retm %>%
  ggplot(aes(date,retornos,color = symbol)) +
  geom_line(show.legend = F) +
  geom_hline(yintercept = 0) +
  facet_wrap(~symbol,scales = 'free_y') +
  labs(x = 'Fecha', y = 'Retornos mensuales', title = 'Retornos Mensuales',
       caption = 'Fuente: Elaboración propia.') +
  scale_x_date(date_breaks = '3 months', date_labels = '%b\n%y') +
  theme(plot.caption = element_text(size=8, hjust=0, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5))

plot_retm %>%
  ggplot(aes(retornos,y = ..density..,color = symbol)) +
  geom_histogram(show.legend = F,bins = 10) +
  facet_wrap(~symbol,scales = 'free') +
  labs(x = 'Retornos', y = 'Retornos mensuales (Densidad)', title = 'Histograma de Retornos Mensuales',
       caption = 'Fuente: Elaboración propia.') +
  theme(plot.caption = element_text(size=8, hjust=0, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 8,angle = -90,vjust = 0.5,hjust = 0.5))


##Grafico dinamico de retornos

dygraph(retornos_d, main = 'Grafico 4: Dygraph Retornos Diarios.') 


### Gráfico de las correlaciones

corrplot(cor(retornos_d),method = 'color', cl.lim= c(0,1),title = 'Correlaciones entre las acciones.',
         tl.col = 'black',tl.offset = 0.3,mar=c(2,0,4,0))



### Matriz de Var y Covar:

matcov = covEstimator(retornos_dts)

### Tabla de retornos esperados y varianza:
ret.var = data.frame("Retornos Esperados" = matcov$mu, "Varianza" = diag(matcov$Sigma))
ret.var = ret.var %>% mutate(Betas = t(CAPM.beta(retornos_d,retornos_sp500)),
                             t(CAPM.alpha(retornos_d,retornos_sp500)))
row.names(ret.var) = tickers

ret.var



##########################################################
##                                                      ##
##     FRONTERA EFICIENTE Y ELECCION DEL PORTAFOLIO     ##
##                                                      ##
##########################################################


frontera = portfolioFrontier(retornos_dts, constraints = 'LongOnly')

#Frontera eficiente
tailoredFrontierPlot(frontera, sharpeRatio = FALSE, title = TRUE, risk = c('Sigma')) ## Grafico 5

weights_frontera = getWeights(frontera) ###Indica las ponderaciones que se hace en cada punto
colnames(weights_frontera) = tickers   ##de la frontera eficiente

rr_frontera = frontierPoints(frontera)## rr = risk return --> Son las combinaciones riesgo-rendimiento


### Armado del portfolio----

objetivo = portfolioSpec()
setTargetReturn(objetivo) = 0.042
print(objetivo)

portafolio = efficientPortfolio(data=retornos_mts,spec = objetivo,constraints="Longonly")
portafolio  #Portafiolio selecto



#----
activos_p = getWeights(portafolio)    # Composicion del portafolio
activos_p = activos_p[activos_p != 0]

tickers_p = names(activos_p)

retornos_p = retornos_d[,c(tickers_p)] # Retornos de los activos que componen el portafolio
n = nrow(retornos_p)


### Pie-Chart de la composicion del portafolio

df = data.frame(Activos = names(activos_p),Porcentaje = c(activos_p))

ggplot(df,aes(x="",y=Porcentaje, fill=Activos))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(Porcentaje/1)),
            position=position_stack(vjust=0.4),color="black",size=5)+
  coord_polar(theta = "y")+
  scale_fill_brewer(palette = "Set1")+
  theme_void()+
  labs(title="Grafico 6: Composición del Portafolio", caption = 'Fuente: elaboración propia.') +
  theme(plot.caption = element_text(size=10, hjust=0.05, vjust = 4, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5))

### Gráfico de las correlaciones entre activos del portafolio:

corrplot(cor(retornos_p),method = 'color', cl.lim= c(0,1),title = 'Grafico 7: Correlaciones entre las acciones del portafolio.',
         tl.col = 'black',tl.offset = 0.3,mar=c(0,0,7,0))




#######################################
##                                   ##
##     SIMULACION DE MONTE CARLO     ##
##                                   ##
#######################################

# Definimos los parametros para el modelo de simulación

datos_p = tail(retornos_p,n)

mu.sigma_p = cbind(mu = colMeans(datos_p),sigma = colStdevs(datos_p)) # Tabla de Retornos esperados y desvios
correlaciones_p = cor(retornos_p)
lastprecio_p = tail(precios_ts[,c(tickers_p)],1) # Ultimo precio de cada accion


# La simulacion de Monte Carlo la corremos en base al rendimiento del portafolio ya armado,
# es por eso que calculamos los retornos historicos del portafolio armado

valores.port = as.matrix(retornos_p) %*% as.matrix(activos_p) 

media.port = mean(valores.port)
sd.port = sd(valores.port)

P0_p = as.matrix(lastprecio_p) %*% as.matrix(activos_p) # Precio inicial de donde parte la simulación


###Parámetros del modelo----

m = 1000 ##Cantidad de simulaciones
P0 = P0_p[1,1] #Precio inicial del activo
mu = media.port #Media del rendimiento
sigma = sd.port #Desvío del rendimiento
Te = 44 #Período de simulación
n = 44 #Cantidad de time steps
dt = Te/n   #Tamaño de cada time step


###Simulación----

sim = matrix(data = rep(P0,m), nrow = m, ncol = n + 1, byrow = F) #Creamos la matriz donde se guardan los caminos de precios

for(i in 1:m){ #hace el bucle para cada simulacion
  for(j in 2:(n+1)){ #hace un shock aleatorio para cada período
    sim[i,j]= sim[i,j-1]*exp((mu - 0.5*(sigma^2))*dt + sigma*sqrt(dt)*rnorm(1))
  }
}



### Gráfico ----
### Colores según media----

dt_dias = round(dt,0)

dt_seq = seq(from = 0, to = dt_dias*n, length = ncol(sim))

medias_avg = c(NULL)

for(i in 1:m){medias_avg = c(medias_avg,mean(sim[i,]))}

medias_avg = matrix(data = sort(medias_avg), ncol = 1)

Asigna_Color = function(i){
  media =  mean(sim[i,])
  if(media > quantile(medias_avg, 0.9)){return('firebrick4')}
  if(media > quantile(medias_avg, 0.7)){return('firebrick1')}
  if(media > quantile(medias_avg, 0.3)){return('gold1')}
  if(media > quantile(medias_avg, 0.1)){return('cyan')}
  return('blue')
}


##Percentiles----

sim_percentiles = matrix(nrow = 3, ncol = n+1)
for(i in 1:(n+1)){
  sim_percentiles[1,i] = quantile(sim[,i],0.95)
  sim_percentiles[2,i] = mean(sim[,i])
  sim_percentiles[3,i] = quantile(sim[,i],0.05)
}


##Armado de Gráficos----

plot(x = dt_seq, y = sim[1,], type = "l", ylim = c(min(sim), max(sim)),
     main = "Gráfico 8: Simulación del Valor del Portafolio", sub = "Fuente: elaboración propia.",
     xlab = "Tiempo", ylab = "Precio", col = Asigna_Color(1))

#Grafica los m-1 restantes caminos:
for(i in 2:m){lines(x = dt_seq, y = sim[i,], col=Asigna_Color(i))}

lines(x = dt_seq, y = sim_percentiles[1,], col = 'azure4', lwd = 2)
lines(x = dt_seq, y = sim_percentiles[2,], col = 'black', lwd = 2.5)
lines(x = dt_seq, y = sim_percentiles[3,], col = 'azure4', lwd = 2)
legend('topleft', title = 'Precio Promedio',
       legend = c('>90%', '70%-90%', '30%-70%','10%-30%','<10%'),
       col = c('firebrick4','firebrick1','gold1','dodgerblue','mediumblue'),
       lty=1, lwd = 2, y.intersp = 1, cex = 0.75, bty = "n", title.adj = c(0.3,-2))


## Graficamos el histograma del valor final de la cartera simulada:

ultimas.sim = as.data.frame(sim[,Te+1])
colnames(ultimas.sim) = 'Sim'

ultimas.sim %>%
  ggplot() +
  geom_histogram(aes(x = Sim,y = ..density.., color = 'red'),show.legend = F,bins = 10) +
  labs(x = 'Valor', y = 'Densidad', title = 'Gráfico 9: Histograma de Valores Simulados',
       caption = 'Fuente: Elaboración propia.') +
  theme(plot.caption = element_text(size=8, hjust=0, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 8,angle = -90,vjust = 0.5,hjust = 0.5)) +
  geom_vline(xintercept = quantile(ultimas.sim$Sim,0.05), linetype = 'dotted')



### Graficamos el camino de los precios con la simulacion:
t.sim = c(seq(from = nrow(retornos_p)+1, to = nrow(retornos_p)+44))

preciosMC = as.data.frame(t(sim))[-1,]
preciosMC['t'] = t.sim


totalMC = as.matrix(precios_ts[,tickers_p]) %*% as.matrix(activos_p)
rownames(totalMC) = c(seq(1,nrow(precios_ts)))
totalMC = as.data.frame(totalMC)
totalMC['t'] = as.numeric(rownames(totalMC))
colnames(totalMC) = c('Valor','t')
t = totalMC$t
totalMC = cbind(as.data.frame(matrix(totalMC$Valor,nrow = nrow(retornos_p)+1, ncol = m)),t)


preciosMC = rbind(totalMC,preciosMC)
preciosMC2 = filter(preciosMC, t >= 252)

plot(x = preciosMC$t, y = preciosMC[,1], type = "l",
     ylim = c(min(preciosMC[,-ncol(preciosMC)]), max(preciosMC[,-ncol(preciosMC)])),
     main = "Gráfico 12: Valores reales del portafolio + Simulación", sub = "Fuente: elaboración propia.",
     xlab = "Tiempo", ylab = "Precio", col = Asigna_Color(1))

#Grafica los m-1 restantes caminos:
for(i in 2:m){lines(x = preciosMC2$t, y = preciosMC2[,i], col=Asigna_Color(i))}

lines(x = t.sim, y = sim_percentiles[1,][-1], col = 'azure4', lwd = 2)
lines(x = t.sim, y = sim_percentiles[2,][-1], col = 'black', lwd = 2.5)
lines(x = t.sim, y = sim_percentiles[3,][-1], col = 'azure4', lwd = 2)
legend('bottom',title = 'Precio Promedio',
       legend = c('>90%', '70%-90%', '30%-70%','10%-30%','<10%'),
       col = c('firebrick4','firebrick1','gold1','dodgerblue','mediumblue'),
       lty=1, lwd = 2, y.intersp = 1, cex = 0.75, bty = "n", title.adj = c(0.3,-2))



#######################################
##                                   ##
##     Modelo de Predicción: VAR     ##
##                                   ##
#######################################



### Tests de DF y PP
DFtests = list()
for (i in 1:ncol(retornos_p)){
  DFtests[[paste('DF',names(retornos_p[,i]))]] = adf.test(retornos_p[,i])
}


PPtests = list()

for (i in 1:ncol(retornos_p)){
  PPtests[[paste('PP',names(retornos_p[,i]))]] = pp.test(retornos_p[,i])
}

par(mfrow = c(4,4))

for (i in 1:ncol(retornos_p)){
  acf(retornos_p[,i])
  pacf(retornos_p[,i])
}

par(mfrow = c(1,1))


### Definimos el modelo VAR

VARselect(retornos_p,lag.max = 25) #VAR(23)

modeloVAR = VAR(retornos_p,p =23)
plot(modeloVAR)


### Corremos los test de autocorrelación y de raíces unitarias

residuos = residuals(modeloVAR)

JBtests = list()

for(i in 1:ncol(retornos_p)){
  JBtests[[paste('JB',names(retornos_p[,i]))]] = jarque.bera.test(residuos[,i])
}


LBtests = list()

for(i in 1:ncol(retornos_p)){
  LBtests[[paste('LB',names(retornos_p[,i]))]] = Box.test(residuos[,i])
}


### Forecast del modelo y graficos:

p = 44
modeloVAR.pred = predict(modeloVAR,n.ahead = p) ## Forecast a 2 meses

plot(modeloVAR.pred)

modeloVAR.graph = as.data.frame(modeloVAR.pred$fcst$AAPL)

for(i in 2:length(tickers_p)){
  modeloVAR.graph = rbind(modeloVAR.graph,as.data.frame(modeloVAR.pred$fcst[[tickers_p[i]]]))
}

Symbol = c(rep('AAPL',p))

for (i in 2:length(tickers_p)){
  Symbol = c(Symbol,rep(tickers_p[[i]],p))
}

modeloVAR.graph = cbind(modeloVAR.graph,Symbol)

### Graficos de los retornos proyectados con intervalos de confianza:
modeloVAR.graph %>% ggplot(aes(index(modeloVAR.graph),fcst,color = Symbol)) +  
  geom_line(show.legend = F) +
  geom_hline(yintercept = 0) +
  geom_line(aes(y = lower),linetype = 'dotted',show.legend = F) +
  geom_line(aes(y = upper),linetype = 'dotted',show.legend = F) +
  facet_wrap(~Symbol,scales = 'free') +
  labs(x = 'Tiempo', y = 'Retornos diarios', title = 'Grafico 10: Predicción de Retornos',
       caption = 'Fuente: elaboración propia') +
  theme(plot.caption = element_text(size=8, hjust=0, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 8,vjust = 0.5,hjust = 0.5))
  


### Introducimos los últimos precios reales para calcular los precios proyectados:
lastprecio_p = tail(precios_ts[,tickers_p],1)
nas = matrix(rep(NA,ncol(retornos_p)*p),nrow = p)
colnames(nas) = colnames(lastprecio_p)

prediccion_p = rbind(as.data.frame(lastprecio_p),nas)
rownames(prediccion_p) = c(seq(1:nrow(prediccion_p)))

for (i in 2:p){
  for (j in 1:ncol(prediccion_p)){
    prediccion_p[i,j] = prediccion_p[i-1,j] * exp(as.data.frame(modeloVAR.pred$fcst[[j]])[i,1])
  }
}

prediccion_upper = rbind(as.data.frame(lastprecio_p),nas)
rownames(prediccion_upper) = c(seq(1:nrow(prediccion_upper)))

for (i in 2:p){
  for (j in 1:ncol(prediccion_upper)){
    prediccion_upper[i,j] = prediccion_upper[i-1,j] * exp(as.data.frame(modeloVAR.pred$fcst[[j]])[i,3])
  }
}

prediccion_lower = rbind(as.data.frame(lastprecio_p),nas)
rownames(prediccion_lower) = c(seq(1:nrow(prediccion_lower)))

for (i in 2:p){
  for (j in 1:ncol(prediccion_lower)){
    prediccion_lower[i,j] = prediccion_lower[i-1,j] * exp(as.data.frame(modeloVAR.pred$fcst[[j]])[i,2])
  }
}


graph.prediccion_lower = na.omit(gather(prediccion_lower,Symbol,Lower))
graph.prediccion_upper = na.omit(gather(prediccion_upper,Symbol,Upper))
graph.pred.precios = na.omit(gather(prediccion_p,Symbol,Prediccion))

graph.pred.precios = cbind(graph.pred.precios,graph.prediccion_lower['Lower'],graph.prediccion_upper['Upper'])


graph.pred.precios %>% ggplot(aes(index(graph.pred.precios),Prediccion,color = Symbol)) +
  geom_line(show.legend = F) +
  facet_wrap(~Symbol,scales = 'free') +
  labs(x = 'Tiempo', y = 'Precios diarios', title = 'Prediccion de Precios',
       caption = 'Fuente: elaboración propia') +
  theme(plot.caption = element_text(size=8, hjust=0, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 8,vjust = 0.5,hjust = 0.5))


### Graficos de precios con intervalos de confianza:

graph.pred.precios %>% ggplot(aes(index(graph.pred.precios),Prediccion,color = Symbol)) +
  geom_line(show.legend = F) +
  geom_line(aes(y = Upper), linetype = 'dotted', show.legend = F) +
  geom_line(aes(y = Lower), linetype = 'dotted', show.legend = F) +
  facet_wrap(~Symbol,scales = 'free') +
  labs(x = 't', y = 'Precios diarios', title = 'Predicción de Precios (con IC)',
       caption = 'Fuente: elaboración propia') +
  theme(plot.caption = element_text(size=8, hjust=0, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 8,vjust = 0.5,hjust = 0.5))



## Agregamos el camino de precios reales para contrastar con la predicción:

total = na.omit(rbind(as.data.frame(precios_ts[,c(tickers_p)]),prediccion_p))
total.upper = na.omit(rbind(as.data.frame(precios_ts[,c(tickers_p)]),prediccion_upper))
total.lower = na.omit(rbind(as.data.frame(precios_ts[,c(tickers_p)]),prediccion_lower))

graph.total = na.omit(gather(total,Symbol,Total))
graph.total.upper = na.omit(gather(total.upper,Symbol,Total))
graph.total.lower = na.omit(gather(total.lower,Symbol,Total))

## Camino de precios reales posteriores a las fechas que tomamos:
precios_reales = tq_get(tickers_p,get = 'stock.prices',from = "2019-01-01", to = '2020-03-06')
precios_reales = precios_reales[,c('symbol','date','adjusted')]


graph.total = cbind(graph.total,precios_reales$adjusted)
colnames(graph.total) = c('Symbol','Total','Real')
graph.total[,'Date'] = precios_reales$date

graph.total.upper = cbind(graph.total.upper,precios_reales$adjusted)
colnames(graph.total.upper) = c('Symbol','Upper','Real')
graph.total.upper[,'Date'] = precios_reales$date

graph.total.lower = cbind(graph.total.lower,precios_reales$adjusted)
colnames(graph.total.lower) = c('Symbol','Lower','Real')
graph.total.lower[,'Date'] = precios_reales$date


graph.total %>% ggplot(aes(x = Date)) +
  geom_line(aes(y = Total), color = 'black') +
  geom_line(aes(y = Real, color = Symbol),show.legend = F) +
  facet_wrap(~Symbol,scales = 'free') +
  labs(x = 'Tiempo', y = 'Precios diarios', title = 'Gráfico 11: Camino Real de Precios y Predicción',
       caption = 'Fuente: elaboración propia') +
  theme(plot.caption = element_text(size=8, hjust=0, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 8, angle = -45,vjust = 0.5,hjust = 0.5))


portafolio.rend = spread(precios_reales,symbol,adjusted)
portafolio.rend[,'Rend real'] = as.matrix(portafolio.rend[2:8]) %*% as.matrix(activos_p)
portafolio.rend[,'Rend pred'] = as.matrix(total) %*% as.matrix(activos_p)

portafolio.rend.graph = portafolio.rend[,c('date','Rend real','Rend pred')]
portafolio.rend.graph = portafolio.rend.graph %>% gather(Rend,Total,-date)


portafolio.rend.graph %>% ggplot(aes(date,Total,color = Rend)) +
  geom_line() +
  labs(x = 'Tiempo', y = 'Precios diarios portafolio', title = 'Grafico 13: Evolucion del Portafolio + Predicción',
       caption = 'Fuente: elaboración propia') +
  theme(plot.caption = element_text(size=8, hjust=0, face="italic", color="black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 8,vjust = 0.5,hjust = 0.5))


### Calculamos la suma de las diferencias entre los precios reales y la prediccion al cuadrado:

Diff_cuadrada = portafolio.rend %>% mutate('Dif cuadrada' = (`Rend real`- `Rend pred`)^2)
Diff_cuadrada = Diff_cuadrada$`Dif cuadrada`
sum.diff_cuad = sum(Diff_cuadrada)
sum.diff_cuad
