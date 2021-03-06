# Funciones para la validacion y depuracion de errores e inconsistencias
# y creacion de objeto SoilProfileCollection 

# f0101, Validar errores en los perfiles
f0101_validarErrores <- function(json){
  errores <- data.frame(id = numeric(), profileId = character(), 
                       status = character(), errorCode = numeric())
  
  #Validacion 01 perfiles duplicados espacialmente
  json1 <- json
  json1 <- unique(json1)
  coord <- json1$properties.identifier[duplicated(json1$geometry.coordinates) | 
                                        duplicated(json1$geometry.coordinates,fromLast=TRUE)]
  for (i in 1: length(coord)){
    id <- json1$id[json1$properties.identifier == coord[i]]
    profileId <- coord[i]
    errores <- rbind(errores, cbind(id, profileId, 3, 1))
  }
  
  #Validacion 02, horizontes duplicados
  for (i in 1: NROW(json)) {
    profileId <- json$properties.identifier[i]
    jsonProfile <- as.data.frame(json$properties.layers[i])
    id <- unique(jsonProfile$profile_id)
    
    duplicatedAttributes <- duplicated(jsonProfile[,-(1:3)])
    duplicatedLayers <- duplicated(jsonProfile[,(1:3)])
    
    for (j in 1: NROW(jsonProfile)) {
      if (duplicatedAttributes[j] != duplicatedLayers[j]) {
        errores <- rbind(errores, cbind(id, profileId, 3, 2))
      }
    }
  }
  
  #Validacion 03, horizontes sin valores top y bottom
  for (i in 1: NROW(json)) {
    profileId <- json$properties.identifier[i]
    jsonProfile <- as.data.frame(json$properties.layers[i])
    id <- unique(jsonProfile$profile_id)
    
    for (j in 1: (NROW(jsonProfile))) {
      if ((is.na(jsonProfile$bottom[j])) & (is.na(jsonProfile$top[j]))) {
        errores <- rbind(errores, cbind(id, profileId, 3, 3))
      }
    }
  }
  
  #Validacion 04, horizontes superpuestos
  for (i in 1: NROW(json)) {
    profileId <- json$properties.identifier[i]
    jsonProfile <- as.data.frame(json$properties.layers[i])
    id <- unique(jsonProfile$profile_id)
    
    if (NROW(jsonProfile) > 1) {
      for (j in 2: NROW(jsonProfile)) {
        if ((jsonProfile$top[j] < jsonProfile$bottom[j-1]) &
            ((jsonProfile$top[j] != jsonProfile$top[j-1]) & (jsonProfile$bottom[j] != jsonProfile$bottom[j-1])) &
            (!is.na(jsonProfile$top[j]) & !is.na(jsonProfile$bottom[j-1]))){
          errores <- rbind(errores, cbind(id, profileId, 3, 4))
        }
      }
    }
  }
  
  errors <- unique(errores)
  names(errores) <- c('id','profileId', 'status', 'errorCode')
  return(errores)
}

# f0102 - Remover perfiles con error.
f0102_removerErrores <- function(json, errores){
  json <- json[!json$id %in% errores$id,]
  return(json)
}

# f0103 - Validar perfiles con inconsistencias.
f0103_validarInconsistencias <- function(json){
  inconsistencias <- data.frame(id = numeric(), profileId = character(), status = character(),
                       errorCode = numeric())
  
  #Validacion 05, horizontes organico
  for (i in 1: NROW(json)) {
    profileId <- json$properties.identifier[i]
    jsonProfile <- as.data.frame(json$properties.layers[i])
    id <- unique(jsonProfile$profile_id)
    if (!is.na(jsonProfile$top[1]) & !is.na(jsonProfile$bottom[1]) ){
      if (((jsonProfile$top[1] > 0) & jsonProfile$bottom[1] == 0) &
          ((jsonProfile$top[2] == 0) & jsonProfile$bottom[2] > 0)) {
        inconsistencias <- rbind(inconsistencias, cbind(id, profileId, 2, 5))
      }
    }
  }
  
  #Validacion 06, horizontes invertido
  for (i in 1: NROW(json)) {
    profileId <- json$properties.identifier[i]
    jsonProfile <- as.data.frame(json$properties.layers[i])
    id <- unique(jsonProfile$profile_id)
    maxLayer <- NROW(jsonProfile)
    if (maxLayer == 1) {
      if (jsonProfile$top[1] > jsonProfile$bottom[1]) {
        inconsistencias <- rbind(inconsistencias, cbind(id, profileId, 2, 6))
      }
    } else {
      for (j in 1: (NROW(jsonProfile))) {
        
        if (j == 1) {
          if ((jsonProfile$top[1] > jsonProfile$bottom[1]) &
              (jsonProfile$bottom[1] == 0) & (jsonProfile$top[2] != 0) &
              ((!is.na(jsonProfile$top[1])) & (!is.na(jsonProfile$bottom[1])))) {
            inconsistencias <- rbind(inconsistencias, cbind(id, profileId, 2, 6))
          }
        } else {
          if ((jsonProfile$top[j] > jsonProfile$bottom[j]) &
              (jsonProfile$bottom[j] >= jsonProfile$bottom[j-1]) &
              ((!is.na(jsonProfile$top[j])) & (!is.na(jsonProfile$bottom[j])))) {
            inconsistencias <- rbind(inconsistencias, cbind(id, profileId, 2, 6))
          }
        }
      }
    }
  }
  
  #Validacion 07, horizontes continuo
  for (i in 1: NROW(json)) {
    profileId <- json$properties.identifier[i]
    jsonProfile <- as.data.frame(json$properties.layers[i])
    id <- unique(jsonProfile$profile_id)
    bottomMax <- (NROW(jsonProfile))
    
    if (is.na(jsonProfile$bottom[bottomMax]) & !is.na((jsonProfile$top[bottomMax]))) {
      inconsistencias <- rbind(inconsistencias, cbind(id, profileId, 2, 7))
    }
  }
  
  #Validacion 08, horizontes duplicados identicos
  for (i in 1: NROW(json)) {
    profileId <- json$properties.identifier[i]
    jsonProfile <- as.data.frame(json$properties.layers[i])
    id <- unique(jsonProfile$profile_id)
    
    if (NROW(jsonProfile[duplicated(jsonProfile),]) != 0) {
      inconsistencias <- rbind(inconsistencias, cbind(id, profileId, 2, 8))
    }
  }
  
  #Validacion 09, valor de top nulo
  for (i in 1: NROW(json)) {
    profileId <- json$properties.identifier[i]
    jsonProfile <- as.data.frame(json$properties.layers[i])
    id <- unique(jsonProfile$profile_id)
    
    for (j in 1: (NROW(jsonProfile))) {
      if (j == 1) {
        if ((is.na(jsonProfile$top[j])) & (!is.na(jsonProfile$bottom[j]))) {
          inconsistencias <- rbind(inconsistencias, cbind(id, profileId, 2, 9))
        }
      }
      if (j > 1) {
        if ((is.na(jsonProfile$top[j])) & (!is.na(jsonProfile$bottom[j])) &
            (!is.na(jsonProfile$bottom[j-1]))) {
          inconsistencias <- rbind(inconsistencias, cbind(id, profileId, 2, 9))
        } 
      }
    }
  }
  
  #Validacion 10, valor del bottom nulo
  for (i in 1: NROW(json)) {
    profileId <- json$properties.identifier[i]
    jsonProfile <- as.data.frame(json$properties.layers[i])
    id <- unique(jsonProfile$profile_id)
    layer <- NROW(jsonProfile)-1
    
    if (layer > 0) { 
      for (j in 1: layer) {
        if (!is.na(jsonProfile$top[j]) 
            & is.na(jsonProfile$bottom[j])
            & !is.na(jsonProfile$top[j+1])) {
          inconsistencias <- rbind(inconsistencias, cbind(id, profileId, 2, 10))
        } 
      }
    }
  }
  
  #Validacion 11, valores de COS atipicos
  jsonf11 <-  do.call(rbind, json$properties.layers)
  jsonf11$cos100 <- log10((as.numeric(jsonf11$organic_carbon))*100)
  atipicValue <- (quantile(jsonf11$cos100, prob=c(0.75), na.rm=TRUE)) + (1.5*IQR(jsonf11$cos100, na.rm = TRUE))
  
  for (i in 1: NROW(jsonf11)){
    id <- jsonf11$profile_id[i]
    profileId <- jsonf11$layer_identifier[i]
    value <- jsonf11$cos100[i]
    if (!is.na(value)){
      if (value > atipicValue) {
        inconsistencias <- rbind(inconsistencias, cbind(id, profileId, 2, 11))
      }
    }
  }
  
  inconsistencias <- unique(inconsistencias)
  names(inconsistencias) <- c('id', 'profileId', 'status', 'errorCode')
  return(inconsistencias)
  
}

#f0104, corregir inconsistencias
f0104_corregirInconsistencias <-  function(spc, errors) {
  # 01 corregir validacion 08, registros duplicados
  spc <- unique(spc)
  
  # 02 corregir validacion 06, horizontes invertidos
  for (i in 1: NROW(spc)) {
    if (spc$profile_id[i] %in% errors$id[errors$errorCode == 2]) {
      if ((!is.na(spc$top[i]) & !is.na(spc$bottom[i])) & (spc$top[i] > spc$bottom[i]) &
          (spc$top[i+1]!= 0)) {
        x <- spc$top[i]
        spc$top[i] <- spc$bottom[i]
        spc$bottom[i] <- x
      }
    }
  }
  
  # 03 corregir validacion 07, horizonte final continuo
  for (i in 1: NROW(spc)) {
    if (spc$profile_id[i] %in% errors$id[errors$errorCode == 3]) {
      if (is.na(spc$bottom[i]) & (spc$profile_id[i] != spc$profile_id[i+1])) {
        spc$bottom[i] <- spc$top[i] + 10
      }
    }
  }
  
  # 04 corregir validacion 09, top nulo
  for (i in 1: NROW(spc)) {
    if (spc$profile_id[i] %in% errors$id[errors$errorCode == 5]) {
      if (is.na(spc$top[i]) & !is.na(spc$bottom[i-1]) & (spc$profile_id[i] == spc$profile_id[i-1])) {
        spc$top[i] <- spc$bottom[i-1]
      }
      if (is.na(spc$top[i]) & (spc$profile_id[i] != spc$profile_id[i-1])) {
        spc$top[i] <- 0
      }
    }
  }
  
  # 05 corregir validacion 10, bottom nulo
  for (i in 1: NROW(spc)) {
    if (spc$profile_id[i] %in% errors$id[errors$errorCode == 6]) {
      if (is.na(spc$bottom[i]) & !is.na(spc$top[i+1]) & (spc$profile_id[i] == spc$profile_id[i+1])) {
        spc$bottom[i] <- spc$top[i+1]
      }
    }
  }
  
  # 06 corregir validacion 11, valores atipicos en COS
  for (i in 1: NROW(spc)) {
    if (spc$layer_identifier[i] %in% errors$profileId[errors$errorCode == 11]) {
      spc$organic_carbon[i] <- NA
    }
  }
  
  # 07 corregir validacion 05, horizonte organico.
  for(profile in errors$id[errors$errorCode == 1]){
    desfase <- spc[spc$profile_id == profile, 'top'][1]
    nlayers <- length(spc[spc$profile_id == profile, 'top'])
    spc[spc$profile_id == profile, 'bottom'][1] <- spc[spc$profile_id == profile, 'top'][1]
    spc[spc$profile_id == profile, 'top'][1] <- 0
    spc[spc$profile_id == profile, 'bottom'][2:nlayers] <- spc[spc$profile_id == profile, 'bottom'][2:nlayers]  + desfase
    spc[spc$profile_id == profile, 'top'][2:nlayers] <- spc[spc$profile_id == profile, 'top'][2:nlayers]  + desfase
  }
  return(spc)
}

# crear objeto SPC con los perfiles validados y corregidos
f0105_crearSPC <- function(json, inconsistencias){
  # Carga de atributos de propiedades del sitio en una matriz y convertirla a dataframe
  sites <- cbind(json$properties.id,
                 json$properties.identifier,
                 do.call(rbind, json$geometry.coordinates),
                 json$properties.country_code)
  sites <- as.data.frame(sites)
  
  # Renombrar las columnas de sites y convertimos los factores en numericos
  names(sites) <- c('profile_id', 'identifier', 'X', 'Y', 'country_code')
  sites$X <-as.numeric(levels(sites$X))[sites$X]
  sites$Y <-as.numeric(levels(sites$Y))[sites$Y]
  
  # Extraccion de los horizontes de los perfiles en otro datadfame.
  spc <- do.call(rbind, json$properties.layers)
  
  # Correccion de inconsistencias
  spc <- f0104_corregirInconsistencias(spc, inconsistencias)
  
  # Verificacion de columnas y conversion de tipos
  if(is.data.frame(spc)) {
    # COnvertir las variables cargadas que estan como texto en valores numericos.
    spc[5:18] <- lapply(spc[5:18], as.numeric)
    
    # Convertir el dataframe en objeto SPC
    depths(spc) <- profile_id ~ top + bottom
    
    # Agregar propiedades del sitio a spc
    site(spc) <- sites
    
    # Agregar coordenadas
    coordinates(spc) <- ~ X+Y
    
    return(spc)
  } else {
    stop('Soil profiles are empty!')
  }
}
