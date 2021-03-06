#Funcion f0301 segmentacion AQP
f0301_sliceAQP <- function(spc){
  spc1 <-aqp::slice(spc, 0:199 ~ organic_carbon)
  return(spc1)
}

# funcion f0302 calculo de agregacion AQP
f0302_agregacionAQP <- function(spc){
  # Parte 1 Analisis de agregacion. devuelve un dataframe con los calculos por segmento
  aggAQP <- aqp::slab(spc, fm= ~ organic_carbon)
  return(aggAQP)
}
  
# funcion f0303 ploteo de la gráfica de agregación.
f0303_plotearAgregacion <- function(aggAQP){
  #  Se puede renombrar el título (es el valor de una columna) que sale en el encabezado del grafico
  #aggAQP$variable <- "Variabilidad  vertical  del  Carbono  organico usando AQP" 
  plotAgg <- xyplot(top ~ p.q50 | variable, data=aggAQP, ylab='Profundidad',
                    xlab='mediana limitada por los percentiles 25 y 75',
                    lower=aggAQP$p.q25, upper=aggAQP$p.q75, ylim=c(200,-2),
                    panel=panel.depth_function,
                    alpha=0.25, sync.colors=TRUE,
                    par.settings=list(superpose.line=list(col='RoyalBlue', lwd=2)),
                    prepanel=prepanel.depth_function,
                    cf=aggAQP$contributing_fraction, cf.col='black', cf.interval=20,
                    layout=c(1,1), strip=strip.custom(bg=grey(0.8)),
                    scales=list(x=list(tick.number=4, alternating=3, relation='free')))
  #print(plotAggAQP)
  plot(plotAgg)
}
