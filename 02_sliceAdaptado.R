# Funciones de segmentación adaptada, estos procesos se realizan a partir de un SPC de entrada
f0201_sliceAdaptado <- function(spc){
  # Se hace una copia de spc para trabajar sobre estey no alterar el original
  spc2 <- spc
  
  #Primer paso: extraer y depurar los horizontes, luego ya depurados se aplica el spline 
  layers <- as.data.frame(spc2@horizons[,c("profile_id", "top","bottom","organic_carbon")])
  layers <- layers[!is.na(layers$organic_carbon),]
  layers <- layers[!layers$profile_id %in% layers$profile_id[ave(layers$profile_id, layers$profile_id, FUN = length) == 1],]  
  layersNames <- unique(layers$profile_id) 
  
  layersSpline <- ea_spline(layers, var.name = "organic_carbon",
                            d = t(c(0, 5, 15, 30, 60, 100, 200)), lam = 0.1, vlow = 0, vhigh = 200,
                            show.progress = TRUE)

  layersSplineValues <- as.data.frame(layersSpline$var.1cm)
  names(layersSplineValues) <-  layersNames
  
  # Se segmenta en nuevo spc (spc2).
  spc2 <- aqp::slice(spc2, 0:199 ~ organic_carbon)
  
  # Se remplazan los valores de carbono del spc por los valores calculados con el spline
  for (i in 1: NCOL(layersSplineValues)) {
    idPerfil <- colnames(layersSplineValues[i])
    
    for (j in 1:200) {
      valorCOS <- layersSplineValues[j,i]
      spc2$organic_carbon[spc2$profile_id == idPerfil & spc2$bottom == j] <- valorCOS
    }
  }
  # Se devuelve spc2 con los valores de COS ajustados
  return(spc2)
}