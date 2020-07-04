# Funcion f0401, Interpolacion IDW con los datoas segmentados, validacion y RMSE
f0401_interpolacionIDW <- function(spc, aoi){
  #Extraer del SPC de entrada los promedios de COS por rangos: 0-5, 5-15, 15-30, 0-30.
  sitios <- as.data.frame(cbind(spc@site[, c(1,2)], spc@sp@coords))
  sitios[c("p0a5", "p5a15", "p15a30", "p0a30")] <- NA
  sitios[5:8] <- lapply(sitios[5:8], as.numeric)
  for (i in 1:NROW(sitios)){
    idPerfil <- sitios$profile_id[i]
    
    sitios$p0a5[sitios$profile_id == idPerfil] <- mean(spc$organic_carbon[spc$profile_id == sitios$profile_id[i]][1:5], na.rm = TRUE)
    sitios$p5a15[sitios$profile_id == idPerfil] <- mean(spc$organic_carbon[spc$profile_id == sitios$profile_id[i]][6:15], na.rm = TRUE)
    sitios$p15a30[sitios$profile_id == idPerfil] <- mean(spc$organic_carbon[spc$profile_id == sitios$profile_id[i]][15:30], na.rm = TRUE)
    sitios$p0a30[sitios$profile_id == idPerfil] <- mean(spc$organic_carbon[spc$profile_id == sitios$profile_id[i]][1:30], na.rm = TRUE)
  }
  
  # Ahora se tiene un dataframe con los valores de COS y las coordenadas (WGS84)
  # Para IDW se necesita un objeto del tipo SpatialPointsDataframe, este se crea a continuacion.
  # aoi viene en UTM, su CRS se usa para transformar los puntos a ese sistema
  sitiosSF<- st_as_sf(sitios, coords = c("X", "Y"), crs = 4326)
  sitiosMagna <- st_transform(sitiosSF, proj4string(aoi))
  sitiosCOS <- as(sitiosMagna, 'Spatial')
  sitiosCOS@bbox <- aoi@bbox
  
  # Se debe calcular con base en el bbox de aio el número de celdas para una escala aprox. de 250m
  nCels <- as.integer(((aoi@bbox[1,2]-aoi@bbox[1,1])*(aoi@bbox[2,2] - aoi@bbox[2,1]))/62500)
  
  # Create an empty grid where n is the total number of cells
  grd <- as.data.frame(spsample(sitiosCOS, "regular", n=nCels))
  # You need to figure out what is the expected size of the output grd
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(sitiosCOS)
  
  # Tantas interpolaciones como profundidades establecidas.
  # En esta lista se almacenarán el valor del RMSE para cada iteracion de la siguiente funcion
  idwRMSE <- list()
  for (i in 1:(length(sitiosCOS@data)-2)) {
    
    valDepth <- list('0 a 5 cm', '5 a 15 cm','15 a 30 cm','0 a 30 cm')
    #Los valores estan a partir de la tercer columna
    j <- i+2
    nomDepth <- colnames(sitiosCOS@data[j])

    # Como hay rangos de profundidad con valores NA, se deben excluir para no causar error en la interpolacion
    # Como se eliminan algunos puntos, se debe ajustar el bbox en cada iteracion
    dat <- sitiosCOS[!is.na(sitiosCOS@data[,j]), ][,j]
    dat@bbox <- aoi@bbox
    
    # Interpolacion en grd, usando valor de potencia 2 (idp=2.0)
    P.idw <- gstat::idw(dat@data[,1] ~ 1, dat, newdata=grd, idp=2.0)
    
    # Se convierte en un objeto raster y se recorta al área de estudio
    rNew     <- mask(raster(P.idw), aoi)
    names(rNew) <- nomDepth
    
    # Mapa de la interpolacion
    titleMap <- paste("Porcentaje de \nCarbono Orgánico \npara una profundidad \n de ", valDepth[1])
    
    tm_shape(rNew) + tm_raster(palette = "YlOrBr", title= titleMap, n = 10) + 
      tm_legend(outside = TRUE) +  tm_layout(frame = TRUE, inner.margins = .1) + 
      tm_scale_bar(position=c("left", "bottom"), breaks = c(0, 10, 20, 30), text.size = .7) + 
      tm_grid(labels.inside.frame = FALSE, n.x = 3, n.y = 4, col='gray71') +
      tm_shape(aoi) +  tm_borders()
    
    # Validacion valor observado y valor estimado
    # Renombramos la variable, para que lea siempre el mismo nombre de columna.
    names(dat@data) <- ('varDepth')
    IDW.out <- vector(length = length(dat))
    for (k in 1:length(dat)) {
      IDW.out[k] <- idw(varDepth ~ 1, dat[-k,], dat[k,], idp=2.0)$var1.pred
    }
    
    # RMSE
    valorRMSE <- sqrt( sum((IDW.out - dat$varDepth)^2) / length(dat))
    idwRMSE <- c(idwRMSE, valorRMSE) # Tercer objeto de la lista
    
    # Grafica de diferencias
    titleGraph <- paste("Gráfica de dispersión \n Profundidad: ", valDepth[i], "\nRMSE: ", round(valorRMSE,3))
    OP <- par(pty="s", mar=c(4,2,4,1))
    plot(IDW.out ~ dat$varDepth,  main = titleGraph, col.main="blue", asp=1, 
         xlab="Valores observados", ylab="Valores estimados", pch=16, col=rgb(0,0,0,0.5))
    abline(lm(IDW.out ~ dat$varDepth), col="red", lw=2,lty=2)
    abline(0,1)
    par(OP)
    
  } 
  return(idwRMSE)
}

