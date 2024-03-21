library(jsonlite)
library(purrr)
library(tidyr)
library(tidyverse)
library(jsonlite)

# Directorio que contiene los archivos JSON
directorio <- "salida"
carpeta_salida <- "csv"

# Función para leer un archivo JSON y devolver un dataframe
leer_json <- function(archivo) {
  data <- fromJSON(archivo)
  out <- data.frame(t(data$value))
  colnames(out) <- data$feature
  return(out)
}

col_parse <- function(df){
  nombres_modificados <- colnames(df)
  diccionario <- list()
  idx <- 1
  for(name in colnames(df)){
    if(!(name %in% names(diccionario))){
      diccionario[[name]] <- 1
    }else{
      nombres_modificados[[idx]] <- gsub("\\s+", "", paste0(name, diccionario[[name]]))
      diccionario[[name]] <- 1 + diccionario[[name]]
    }
    idx <- idx + 1
  }
  colnames(df) <- nombres_modificados
  return(df)
}

fichas_tecnicas_fun <- function(df){
  df_salida<- df[[1]]
  df_salida <- col_parse(df_salida)
  for (df in dataframes[-1]){
    df <- col_parse(df)
    columnas_no_comunes_df1 <- setdiff(names(df_salida), names(df))
    columnas_no_comunes_df2 <-setdiff(colnames(df), colnames(df_salida))
    for(col in columnas_no_comunes_df1){
      df[[col]] <- '-'
    }
    for(col in columnas_no_comunes_df2){
      df_salida[[col]] <- '-'
    }
    df1_copy  <- df_salida
    df2_copy  <- df
    df_salida <- rbind(df1_copy , df2_copy)
  }
  return(df_salida)
}

buscar_feature <- function(df, feature){
  coincidencias <- data.frame(columnas = integer(), repeticiones = integer())
  for(i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (grepl(feature, df[i, j], ignore.case = TRUE)) {
        existe <- any(coincidencias$columnas == j)
        if(existe){
          coincidencias$repeticiones[coincidencias$columnas == j] <- coincidencias$repeticiones[coincidencias$columnas == j] + 1
        }else{
          coincidencias <- rbind(coincidencias, data.frame(columnas = j, repeticiones = 1))
        }
      }
    }
  }
  return(coincidencias[order(coincidencias$repeticiones, decreasing = TRUE), ])
}

anadir_feature <- function(df, feature) {
  coincidencias <-
    data.frame(columnas = integer(), repeticiones = integer())
  for (i in 1:nrow(df)) {
    encontrado <- FALSE
    j <- 1
    while(j <= ncol(df) && !encontrado){
      if (grepl(feature, df[i, j], ignore.case = TRUE)) {
        df[i, feature] <- 1
        encontrado <- TRUE
      } else{
        df[i, feature] <- 0
      }
      j <- j+1
    }
  }
  return(df)
}

# Crearemos una función para obtener el número de columnas de un dataframe
num_columnas <- function(df) {
  ncol(df)
}

# Obtener la lista de archivos JSON en el directorio
archivos <- list.files(path = directorio, pattern = "\\.json$", full.names = TRUE)

# Leer cada archivo JSON y almacenar los datos en diferentes dataframes
dataframes <- map(archivos, leer_json)

fichas_tecnicas <- fichas_tecnicas_fun(dataframes)

ft_pro <- separate(fichas_tecnicas, Car, into = c("Brand", "Model"), sep = " ", remove = TRUE)

ft_pro$`Potencia máxima` <- gsub("[^0-9]", "", ft_pro$`Potencia máxima`)
colnames(ft_pro)[colnames(ft_pro) == 'Potencia máxima'] <- 'Potencia_Maxima_CV'

ft_pro$`Potencia máxima eléctrica` <- gsub("[^0-9]", "", ft_pro$`Potencia máxima eléctrica`)
colnames(ft_pro)[colnames(ft_pro) == '`Potencia máxima eléctrica`'] <- '`Potencia_máxima_eléctrica_cv`'


ft_pro$`Volumen del maletero` <- gsub("[^0-9]", "", fichas_tecnicas$`Volumen del maletero`)
colnames(ft_pro)[colnames(ft_pro) == 'Volumen del maletero'] <- 'Vol_Maletero_Litros'


ft_pro$`Aceleración (0-100km/h)` <- gsub("[^0-9.]", "", fichas_tecnicas$`Aceleración (0-100km/h)`)
colnames(ft_pro)[colnames(ft_pro) == 'Aceleración (0-100km/h)'] <- 'Aceleracion_(0_100km/h)_s'


ft_pro$`Velocidad Máxima` <- gsub("[^0-9]", "", fichas_tecnicas$`Velocidad Máxima`)
colnames(ft_pro)[colnames(ft_pro) == 'Velocidad Máxima'] <- 'Velocidad_Máxima_km/h'

ft_pro$`Largo x ancho x alto` <- gsub("[^0-9 ]", "", ft_pro$`Largo x ancho x alto`)
ft_pro <- separate(ft_pro, `Largo x ancho x alto`, 
                   into = c("Largo_mm", "Ancho_mm", "Alto_mm", sep = " "), remove = TRUE)

ft_pro$` ` <- NULL


ft_pro$Peso <- gsub("[^0-9]", "", fichas_tecnicas$Peso)
colnames(ft_pro)[colnames(ft_pro) == 'Peso'] <- 'Peso_con_conductor_kg'

identical(ft_pro$Combustible, ft_pro$Combustible1)
identical(ft_pro$`Caja de cambios`, ft_pro$Cajadecambios1)

ft_pro$Combustible1 <- NULL
ft_pro$Cajadecambios1 <- NULL
ft_pro$Potenciamáxima1 <- NULL
ft_pro$Tracción1 <- NULL
ft_pro$Volumendelmaletero1 <- NULL
ft_pro$`Aceleración(0-100km/h)1` <- NULL
ft_pro$VelocidadMáxima1 <- NULL
ft_pro$Consumomedio1 <- NULL

ft_pro$`Autonomía eléctrica WLTP` <- gsub("[^0-9 ]", "", fichas_tecnicas$`Autonomía eléctrica WLTP`)
colnames(ft_pro)[colnames(ft_pro) == 'Autonomía eléctrica WLTP'] <- 'Autonomía_eléctrica_WLTP'

ft_pro$`Consumo medio eléctrico WLTP` <- gsub(".*?([0-9.]+).*", "\\1", fichas_tecnicas$`Consumo medio eléctrico WLTP`)
colnames(ft_pro)[colnames(ft_pro) == 'Consumo medio eléctrico WLTP'] <- 'Consumo_medio_eléctrico_WLTP'


ft_pro$`Capacidad de la batería` <- gsub("[^0-9.]", "", fichas_tecnicas$`Capacidad de la batería`)
colnames(ft_pro)[colnames(ft_pro) == 'Capacidad de la batería'] <- 'Capacidad_batería_kWh'


ft_pro$`Wattios motor` <- gsub("[^0-9 ]", "", fichas_tecnicas$`Wattios motor`)
colnames(ft_pro)[colnames(ft_pro) == 'Wattios motor'] <- 'Wattios_motor'

ft_pro$`Tiempo de carga` <- gsub(".*?([0-9]+).*", "\\1", fichas_tecnicas$`Tiempo de carga`)
colnames(ft_pro)[colnames(ft_pro) == 'Tiempo de carga'] <- 'Carga_min'

ft_pro$`Wattios motor` <- gsub("[^0-9 ]", "", fichas_tecnicas$`Wattios motor`)
colnames(ft_pro)[colnames(ft_pro) == 'Wattios motor'] <- 'Wattios_motor'

ft_pro$`Capacidad del depósito` <- gsub("[^0-9 ]", "", fichas_tecnicas$`Capacidad del depósito`)
colnames(ft_pro)[colnames(ft_pro) == 'Capacidad del depósito'] <- 'Depósito_litros'

ft_pro$`Consumo medio` <- gsub(".*?([0-9.]+).*", "\\1", fichas_tecnicas$`Consumo medio`)
colnames(ft_pro)[colnames(ft_pro) == 'Consumo medio'] <- 'Consumo_medio_L/100km'

ft_pro$`Consumo urbano` <- gsub(".*?([0-9.]+).*", "\\1", fichas_tecnicas$`Consumo urbano`)
colnames(ft_pro)[colnames(ft_pro) == 'Consumo urbano'] <- 'Consumo_urbano_L/100km'

ft_pro$`Consumo extraurbano`<- gsub(".*?([0-9.]+).*", "\\1", fichas_tecnicas$`Consumo extraurbano`)
colnames(ft_pro)[colnames(ft_pro) == 'Consumo extraurbano'] <- 'Consumo_extraurbano_L/100km'

ft_pro <- ft_pro %>%
  mutate(id = seq_along(Brand))

aux <- anadir_feature(ft_pro, "bluetooth")
aux_2 <- anadir_feature(ft_pro, "navegador")
aux$navegador = aux_2$navegador

#La funcion buscar feature devuelve las columnas donde aparece la palabra introducida
existe_radio <- buscar_feature(ft_pro, "radio")
existe_consumo <- buscar_feature(ft_pro, "consumo medio")
existe_consumo
existe_emisiones <- buscar_feature(ft_pro, "emisiones")
existe_emisiones
existe_radio <- buscar_feature(ft_pro, "radio")
existe_radio

existe_bluetooth<- buscar_feature(ft_pro, "bluetooth")
existe_bluetooth

existe_android<- buscar_feature(ft_pro, "android")
existe_android

existe_auto<- buscar_feature(ft_pro, "auto")
existe_auto

nombres <- paste0(ft_pro$Brand, " ", ft_pro$Model, " ", ft_pro$Versión)
nombres <- data.frame(nombres)

versiones <- csv$title
versiones <- data.frame(versiones)
versiones$year <- csv$year

sum <- 0
for (i in 1:nrow(versiones)) {
  valor_actual <- versiones[i, 1]
  #print(valor_actual)
  posicion <- match(valor_actual, nombres[, 1])
  #print(posicion)
  if (length(posicion) > 0 && !is.na(posicion)) {
    #print(posicion)
    print(paste0(valor_actual,"---", versiones[i, 2],"---",nombres[posicion, 1 ]))
    sum <- sum +1
  }
}
sum 


fichas_tecnicas_final <- aux[, c("Brand", "Model", "Precio", "Caja de cambios", "Combustible", "Potencia_Maxima_CV",
                                    "Tracción", "Vol_Maletero_Litros", "Aceleracion_(0_100km/h)_s",
                                    "Velocidad_Máxima_km/h","Versión", "Nº de puertas",
                                    "Largo_mm", "Ancho_mm", "Alto_mm",
                                    "Nº de plazas","Peso_con_conductor_kg", "Potencia máxima eléctrica",
                                    "Wattios_motor", "Consumo_medio_eléctrico_WLTP", "Autonomía_eléctrica_WLTP",
                                    "Carga_min", "Depósito_litros", "Consumo_medio_L/100km",
                                    "Consumo_urbano_L/100km", "Consumo_extraurbano_L/100km",
                                    "bluetooth", "navegador")]


csv <- read.csv("car_data_format.csv", sep = ";")

versiones <- as.data.frame(unique(csv$title))

csv_ <- csv %>%
  group_by(title) %>%
  slice(1) %>%
  ungroup()

csv_ <- csv_ %>%
    filter(brand == "RENAULT" | brand == "SEAT"| brand == "TOYOTA"
         | brand == "KIA"| brand == "VOLKSWAGEN"| brand == "HYUNDAI"
         | brand == "PEUGEOT" | brand == "DACIA" | brand == "CITROEN"
         | brand == "MERCEDES-BENZ")


csv_ <- as.data.frame(csv_$url_true)
write.csv(csv_, "urls.csv", row.names = FALSE)
write.csv(aux, "fichas_tecnicas26.csv", row.names = FALSE)



#------------------------------------------------------------------------------

adJsons <- list.files(path = "API Requests/adJSONs")

archivos_json <- adJsons[grepl("\\.json$", adJsons)]

recorrer_json <- function(json, prefijo = "") {
  propiedades <- character()  # Crear un vector para almacenar las propiedades
  
  for (propiedad in names(json)) {
    clave_completa <- ifelse(nchar(prefijo) > 0, paste(prefijo, propiedad, sep = "$"), propiedad)
    # Verificar si la propiedad es un objeto JSON
    if (is.list(json[[propiedad]])) {
      # Si es un objeto JSON, llamar recursivamente a la función con ese objeto y combinar los vectores resultantes
      propiedades <- c(propiedades, recorrer_json(json[[propiedad]], prefijo = clave_completa))
    } else {
      propiedades <- c(propiedades, clave_completa)  # Agregar la propiedad al vector solo si no es un objeto JSON
    }
  }
  
  return(propiedades)  # Devolver el vector de propiedades
}


fichas_tecnicas_prueba <-  data.frame(matrix(nrow = 0, ncol = 1000))
fichas_tecnicas_features <- c()
primer_archivo <- TRUE
for (archivo_json in archivos_json) {
  ruta_json <- file.path("API Requests/adJSONs", archivo_json)
  datos_json <- fromJSON(ruta_json)
  if(primer_archivo){
    fichas_tecnicas_features <- recorrer_json(datos_json,"")
    primer_archivo <- FALSE
  }else{
    nuevas_columnas <- recorrer_json(datos_json,"")
    no_pertecen <- setdiff(nuevas_columnas, fichas_tecnicas_features)
    fichas_tecnicas_features <- c(fichas_tecnicas_features, no_pertecen)
  }
}

colnames(fichas_tecnicas_prueba) <- fichas_tecnicas_features
indices <- grep("NA", colnames(fichas_tecnicas_prueba), fixed = TRUE)
fichas_tecnicas_prueba <- fichas_tecnicas_prueba[, -indices]

ficha_json <- function(json, prefijo = "") {
  propiedades <- list()  # Crear una lista para almacenar las propiedades
  
  for (propiedad in names(json)) {
    clave_completa <- ifelse(nchar(prefijo) > 0, paste(prefijo, propiedad, sep = "$"), propiedad)
    
    # Verificar si la propiedad es un objeto JSON
    if (is.list(json[[propiedad]])) {
      # Si es un objeto JSON, llamar recursivamente a la función con ese objeto y guardar el resultado en la lista de propiedades
      sub_propiedades <- ficha_json(json[[propiedad]], prefijo = clave_completa)
      propiedades[[clave_completa]] <- sub_propiedades
    } else {
      # Si no es un objeto JSON, guardar el valor en la lista de propiedades
      propiedades[[clave_completa]] <- json[[propiedad]]
    }
  }
  return(propiedades)  # Devolver la lista de propiedades
}

ficha_json <- function(json, prefijo = "") {
  propiedades <- list()  # Crear una lista para almacenar las propiedades
  
  for (propiedad in names(json)) {
    clave_completa <- ifelse(nchar(prefijo) > 0, paste(prefijo, propiedad, sep = "$"), propiedad)
    
    # Verificar si la propiedad es un objeto JSON
    if (is.list(json[[propiedad]])) {
      # Si es un objeto JSON, llamar recursivamente a la función con ese objeto y guardar el resultado en la lista de propiedades
      sub_propiedades <- ficha_json(json[[propiedad]], prefijo = clave_completa)
      propiedades <- c(propiedades, sub_propiedades)
    } else {
      # Si no es un objeto JSON, guardar el par clave-valor en la lista de propiedades
      propiedades[[clave_completa]] <- json[[propiedad]]
    }
  }
  return(propiedades)  # Devolver la lista de propiedades
}

is_empty <- function(x) {
  is.null(x) || length(x) == 0
}

primer_archivo <- TRUE
for (archivo_json in archivos_json) {
  print(archivo_json)
  ruta_json <- file.path("API Requests/adJSONs", archivo_json)
  datos_json <- fromJSON(ruta_json)
  
  if (primer_archivo) {
    primer_archivo <- FALSE
    dataCar <- data.frame(
      id = ifelse(!is_empty(datos_json$ad$id), datos_json$ad$id, " - "),
      title = ifelse(!is.null(datos_json$ad$title), datos_json$ad$title, " - "),
      photos = ifelse(
        !is_empty(datos_json$ad$photos),
        datos_json$ad$photos[1],
        " - "
      ),
      url = ifelse(!is.null(datos_json$ad$url), datos_json$ad$url, " - "),
      price = ifelse(!is_empty(datos_json$ad$price), datos_json$ad$price[[1]], " - "),
      vehicleyear = ifelse(
        !is.null(datos_json$ad$vehicle$year),
        datos_json$ad$vehicle$year,
        " - "
      ),
      vehiclekm = ifelse(
        !is.null(datos_json$ad$vehicle$km),
        datos_json$ad$vehicle$km,
        " - "
      ),
      vehiclecolor = ifelse(
        !is.null(datos_json$ad$vehicle$color),
        datos_json$ad$vehicle$color,
        " - "
      ),
      vehiclecolorId = ifelse(
        !is.null(datos_json$ad$vehicle$colorId),
        datos_json$ad$vehicle$colorId,
        " - "
      ),
      vehicleisCertified = ifelse(
        !is.null(datos_json$vehicle$isCertified),
        datos_json$vehicle$isCertified,
        " - "
      ),
      certificatedProgramId = ifelse(
        !is.null(datos_json$ad$vehicle$certificatedProgramId),
        datos_json$ad$vehicle$certificatedProgramId,
        " - "
      ),
      hasVehicleHistory =
        ifelse(
          !is.null(datos_json$ad$vehicle$hasVehicleHistory),
          datos_json$ad$vehicle$hasVehicleHistory,
          " - "
        ),
      makeId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$makeId),
          datos_json$ad$vehicle$specs$makeId,
          " - "
        ),
      make =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$make),
          datos_json$ad$vehicle$specs$make,
          " - "
        ),
      modelId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$modelId),
          datos_json$ad$vehicle$specs$modelId,
          " - "
        ),
      model =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$model),
          datos_json$ad$vehicle$specs$model,
          " - "
        ),
      versionId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$versionId),
          datos_json$ad$vehicle$specs$versionId,
          " - "
        ),
      version =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$version),
          datos_json$ad$vehicle$specs$version,
          " - "
        ),
      uniqueVehicleId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$uniqueVehicleId),
          datos_json$ad$vehicle$specs$uniqueVehicleId,
          " - "
        ),
      bodyTypeId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$bodyTypeId),
          datos_json$ad$vehicle$specs$bodyTypeId,
          " - "
        ),
      doors =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$doors),
          datos_json$ad$vehicle$specs$doors,
          " - "
        ),
      power =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$power),
          datos_json$ad$vehicle$specs$power,
          " - "
        ),
      transmissionTypeId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$transmissionTypeId),
          datos_json$ad$vehicle$specs$transmissionTypeId,
          " - "
        ),
      fuelTypeId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$fuelTypeId),
          datos_json$ad$vehicle$specs$fuelTypeId,
          " - "
        ),
      seatingCapacity =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$seatingCapacity),
          datos_json$ad$vehicle$specs$seatingCapacity,
          " - "
        ),
      professionalSellerId =
        ifelse(
          !is.null(datos_json$ad$professionalSeller$id),
          datos_json$ad$professionalSeller$id,
          " - "
        ),
      externalAdId =
        ifelse(
          !is.null(datos_json$ad$externalAdId),
          datos_json$ad$externalAdId,
          " - "
        ),
      isTradable =
        ifelse(
          !is.null(datos_json$ad$isTradable),
          datos_json$ad$isTradable,
          " - "
        ),
      province =
        ifelse(
          !is.null(datos_json$professionalSeller$location$province),
          datos_json$professionalSeller$location$province,
          " - "
        ),
      provinceId =
        ifelse(
          !is.null(datos_json$professionalSeller$location$provinceId),
          datos_json$professionalSeller$location$provinceId,
          " - "
        ),
      year =
        ifelse(
          !is.null(datos_json$vehicleSpecs$year),
          datos_json$vehicleSpecs$year,
          " - "
        ),
      numberOfDoors =
        ifelse(
          !is.null(datos_json$vehicleSpecs$numberOfDoors),
          datos_json$vehicleSpecs$numberOfDoors,
          " - "
        ),
      numberOfSeats =
        ifelse(
          !is.null(datos_json$vehicleSpecs$numberOfSeats),
          datos_json$vehicleSpecs$numberOfSeats,
          " - "
        ),
      horsePower =
        ifelse(
          !is.null(datos_json$vehicleSpecs$horsePower),
          datos_json$vehicleSpecs$horsePower,
          " - "
        ),
      start =
        ifelse(
          !is.null(datos_json$vehicleSpecs$start),
          datos_json$vehicleSpecs$start,
          " - "
        ),
      end =
        ifelse(
          !is.null(datos_json$vehicleSpecs$end),
          datos_json$vehicleSpecs$end,
          " - "
        ),
      dimensionsInMillimeterswidth =
        ifelse(
          !is.null(datos_json$vehicleSpecs$dimensionsInMillimeters$width),
          datos_json$vehicleSpecs$dimensionsInMillimeters$width,
          " - "
        ),
      dimensionsInMillimetersheight =
        ifelse(
          !is.null(datos_json$vehicleSpecs$dimensionsInMillimeters$height),
          datos_json$vehicleSpecs$dimensionsInMillimeters$height,
          " - "
        ),
      dimensionsInMillimeterslength =
        ifelse(
          !is.null(datos_json$vehicleSpecs$dimensionsInMillimeters$length),
          datos_json$vehicleSpecs$dimensionsInMillimeters$length,
          " - "
        ),
      trunkCapacityInLiters =
        ifelse(
          !is.null(datos_json$vehicleSpecs$trunkCapacityInLiters),
          datos_json$vehicleSpecs$trunkCapacityInLiters,
          " - "
        ),
      weight =
        ifelse(
          !is.null(datos_json$vehicleSpecs$weight),
          datos_json$vehicleSpecs$weight,
          " - "
        ),
      tankCapacityInLiters =
        ifelse(
          !is.null(datos_json$vehicleSpecs$tankCapacityInLiters),
          datos_json$vehicleSpecs$tankCapacityInLiters,
          " - "
        ),
      consumptionurban =
        ifelse(
          !is.null(datos_json$vehicleSpecs$consumption$urban),
          datos_json$vehicleSpecs$consumption$urban,
          " - "
        ),
      consumptionmixed =
        ifelse(
          !is.null(datos_json$vehicleSpecs$consumption$mixed),
          datos_json$vehicleSpecs$consumption$mixed,
          " - "
        ),
      consumptionextraUrban =
        ifelse(
          !is.null(datos_json$vehicleSpecs$consumption$extraUrban),
          datos_json$vehicleSpecs$consumption$extraUrban,
          " - "
        ),
      maxSpeed =
        ifelse(
          !is.null(datos_json$vehicleSpecs$maxSpeed),
          datos_json$vehicleSpecs$maxSpeed,
          " - "
        ),
      acceleration =
        ifelse(
          !is.null(datos_json$vehicleSpecs$acceleration),
          datos_json$vehicleSpecs$acceleration,
          " - "
        ),
      manufacturerPrice =
        ifelse(
          !is.null(datos_json$vehicleSpecs$manufacturerPrice),
          datos_json$vehicleSpecs$manufacturerPrice,
          " - "
        ),
      co2EmissionsGramsPerKm =
        ifelse(
          !is.null(datos_json$vehicleSpecs$co2EmissionsGramsPerKm),
          datos_json$vehicleSpecs$co2EmissionsGramsPerKm,
          " - "
        ),
      batteryVoltage =
        ifelse(
          !is.null(
            datos_json$vehicleSpecs$electricFeatures$powerSource$batteryVoltage$value
          ),
          datos_json$vehicleSpecs$electricFeatures$powerSource$batteryVoltage$value,
          "-"
        ),
      batteryKWH =
        ifelse(
          !is.null(
            datos_json$vehicleSpecs$electricFeatures$powerSource$maximumBatteryKWH$value
          ),
          datos_json$vehicleSpecs$electricFeatures$powerSource$maximumBatteryKWH$value,
          "-"
        ),
      chargingTime =
        ifelse(
          !is_empty(
            datos_json$vehicleSpecs$electricFeatures$chargingInformation
          ),
          datos_json$vehicleSpecs$electricFeatures$chargingInformation$standardMode$duration$value,
          "-"
        ),
      chargingTimeFast =
        ifelse(
          !is_empty(
            datos_json$vehicleSpecs$electricFeatures$chargingInformation
          ),
          datos_json$vehicleSpecs$electricFeatures$chargingInformation$fastMode$duration$value,
          "-"
        )
    )
    
  }else {
    coche2 <- data.frame(
      id = ifelse(!is_empty(datos_json$ad$id), datos_json$ad$id, " - "),
      title = ifelse(!is.null(datos_json$ad$title), datos_json$ad$title, " - "),
      photos = ifelse(
        !is_empty(datos_json$ad$photos),
        datos_json$ad$photos[1],
        " - "
      ),
      url = ifelse(!is.null(datos_json$ad$url), datos_json$ad$url, " - "),
      price = ifelse(!is_empty(datos_json$ad$price), datos_json$ad$price[[1]], " - "),
      vehicleyear = ifelse(
        !is.null(datos_json$ad$vehicle$year),
        datos_json$ad$vehicle$year,
        " - "
      ),
      vehiclekm = ifelse(
        !is.null(datos_json$ad$vehicle$km),
        datos_json$ad$vehicle$km,
        " - "
      ),
      vehiclecolor = ifelse(
        !is.null(datos_json$ad$vehicle$color),
        datos_json$ad$vehicle$color,
        " - "
      ),
      vehiclecolorId = ifelse(
        !is.null(datos_json$ad$vehicle$colorId),
        datos_json$ad$vehicle$colorId,
        " - "
      ),
      vehicleisCertified = ifelse(
        !is.null(datos_json$vehicle$isCertified),
        datos_json$vehicle$isCertified,
        " - "
      ),
      certificatedProgramId = ifelse(
        !is.null(datos_json$ad$vehicle$certificatedProgramId),
        datos_json$ad$vehicle$certificatedProgramId,
        " - "
      ),
      hasVehicleHistory =
        ifelse(
          !is.null(datos_json$ad$vehicle$hasVehicleHistory),
          datos_json$ad$vehicle$hasVehicleHistory,
          " - "
        ),
      makeId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$makeId),
          datos_json$ad$vehicle$specs$makeId,
          " - "
        ),
      make =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$make),
          datos_json$ad$vehicle$specs$make,
          " - "
        ),
      modelId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$modelId),
          datos_json$ad$vehicle$specs$modelId,
          " - "
        ),
      model =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$model),
          datos_json$ad$vehicle$specs$model,
          " - "
        ),
      versionId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$versionId),
          datos_json$ad$vehicle$specs$versionId,
          " - "
        ),
      version =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$version),
          datos_json$ad$vehicle$specs$version,
          " - "
        ),
      uniqueVehicleId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$uniqueVehicleId),
          datos_json$ad$vehicle$specs$uniqueVehicleId,
          " - "
        ),
      bodyTypeId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$bodyTypeId),
          datos_json$ad$vehicle$specs$bodyTypeId,
          " - "
        ),
      doors =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$doors),
          datos_json$ad$vehicle$specs$doors,
          " - "
        ),
      power =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$power),
          datos_json$ad$vehicle$specs$power,
          " - "
        ),
      transmissionTypeId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$transmissionTypeId),
          datos_json$ad$vehicle$specs$transmissionTypeId,
          " - "
        ),
      fuelTypeId =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$fuelTypeId),
          datos_json$ad$vehicle$specs$fuelTypeId,
          " - "
        ),
      seatingCapacity =
        ifelse(
          !is.null(datos_json$ad$vehicle$specs$seatingCapacity),
          datos_json$ad$vehicle$specs$seatingCapacity,
          " - "
        ),
      professionalSellerId =
        ifelse(
          !is.null(datos_json$ad$professionalSeller$id),
          datos_json$ad$professionalSeller$id,
          " - "
        ),
      externalAdId =
        ifelse(
          !is.null(datos_json$ad$externalAdId),
          datos_json$ad$externalAdId,
          " - "
        ),
      isTradable =
        ifelse(
          !is.null(datos_json$ad$isTradable),
          datos_json$ad$isTradable,
          " - "
        ),
      province =
        ifelse(
          !is.null(datos_json$professionalSeller$location$province),
          datos_json$professionalSeller$location$province,
          " - "
        ),
      provinceId =
        ifelse(
          !is.null(datos_json$professionalSeller$location$provinceId),
          datos_json$professionalSeller$location$provinceId,
          " - "
        ),
      year =
        ifelse(
          !is.null(datos_json$vehicleSpecs$year),
          datos_json$vehicleSpecs$year,
          " - "
        ),
      numberOfDoors =
        ifelse(
          !is.null(datos_json$vehicleSpecs$numberOfDoors),
          datos_json$vehicleSpecs$numberOfDoors,
          " - "
        ),
      numberOfSeats =
        ifelse(
          !is.null(datos_json$vehicleSpecs$numberOfSeats),
          datos_json$vehicleSpecs$numberOfSeats,
          " - "
        ),
      horsePower =
        ifelse(
          !is.null(datos_json$vehicleSpecs$horsePower),
          datos_json$vehicleSpecs$horsePower,
          " - "
        ),
      start =
        ifelse(
          !is.null(datos_json$vehicleSpecs$start),
          datos_json$vehicleSpecs$start,
          " - "
        ),
      end =
        ifelse(
          !is.null(datos_json$vehicleSpecs$end),
          datos_json$vehicleSpecs$end,
          " - "
        ),
      dimensionsInMillimeterswidth =
        ifelse(
          !is.null(datos_json$vehicleSpecs$dimensionsInMillimeters$width),
          datos_json$vehicleSpecs$dimensionsInMillimeters$width,
          " - "
        ),
      dimensionsInMillimetersheight =
        ifelse(
          !is.null(datos_json$vehicleSpecs$dimensionsInMillimeters$height),
          datos_json$vehicleSpecs$dimensionsInMillimeters$height,
          " - "
        ),
      dimensionsInMillimeterslength =
        ifelse(
          !is.null(datos_json$vehicleSpecs$dimensionsInMillimeters$length),
          datos_json$vehicleSpecs$dimensionsInMillimeters$length,
          " - "
        ),
      trunkCapacityInLiters =
        ifelse(
          !is.null(datos_json$vehicleSpecs$trunkCapacityInLiters),
          datos_json$vehicleSpecs$trunkCapacityInLiters,
          " - "
        ),
      weight =
        ifelse(
          !is.null(datos_json$vehicleSpecs$weight),
          datos_json$vehicleSpecs$weight,
          " - "
        ),
      tankCapacityInLiters =
        ifelse(
          !is.null(datos_json$vehicleSpecs$tankCapacityInLiters),
          datos_json$vehicleSpecs$tankCapacityInLiters,
          " - "
        ),
      consumptionurban =
        ifelse(
          !is.null(datos_json$vehicleSpecs$consumption$urban),
          datos_json$vehicleSpecs$consumption$urban,
          " - "
        ),
      consumptionmixed =
        ifelse(
          !is.null(datos_json$vehicleSpecs$consumption$mixed),
          datos_json$vehicleSpecs$consumption$mixed,
          " - "
        ),
      consumptionextraUrban =
        ifelse(
          !is.null(datos_json$vehicleSpecs$consumption$extraUrban),
          datos_json$vehicleSpecs$consumption$extraUrban,
          " - "
        ),
      maxSpeed =
        ifelse(
          !is.null(datos_json$vehicleSpecs$maxSpeed),
          datos_json$vehicleSpecs$maxSpeed,
          " - "
        ),
      acceleration =
        ifelse(
          !is.null(datos_json$vehicleSpecs$acceleration),
          datos_json$vehicleSpecs$acceleration,
          " - "
        ),
      manufacturerPrice =
        ifelse(
          !is.null(datos_json$vehicleSpecs$manufacturerPrice),
          datos_json$vehicleSpecs$manufacturerPrice,
          " - "
        ),
      co2EmissionsGramsPerKm =
        ifelse(
          !is.null(datos_json$vehicleSpecs$co2EmissionsGramsPerKm),
          datos_json$vehicleSpecs$co2EmissionsGramsPerKm,
          " - "
        ),
      batteryVoltage =
        ifelse(
          !is.null(
            datos_json$vehicleSpecs$electricFeatures$powerSource$batteryVoltage$value
          ),
          datos_json$vehicleSpecs$electricFeatures$powerSource$batteryVoltage$value,
          "-"
        ),
      batteryKWH =
        ifelse(
          !is.null(
            datos_json$vehicleSpecs$electricFeatures$powerSource$maximumBatteryKWH$value
          ),
          datos_json$vehicleSpecs$electricFeatures$powerSource$maximumBatteryKWH$value,
          "-"
        ),
      chargingTime =
        ifelse(
          !is.null(
            datos_json$vehicleSpecs$electricFeatures$chargingInformation$standardMode$duration$value
          ),
          datos_json$vehicleSpecs$electricFeatures$chargingInformation$standardMode$duration$value,
          "-"
        ),
      chargingTimeFast =
        ifelse(
          !is.null(
            datos_json$vehicleSpecs$electricFeatures$chargingInformation$fastMode$duration$value
          ),
          datos_json$vehicleSpecs$electricFeatures$chargingInformation$fastMode$duration$value,
          "-"
        )
    )
    dataCar = rbind(dataCar, coche2)
    
  }
  print("-----")
  #
  
}

write.csv(dataCar, "carData_Final.csv", row.names = TRUE)


coincidencias <- merge(csv, dataCar, by.x = "title", by.y = "title")

# Contar las coincidencias
numero_de_coincidencias <- sum(coincidencias$title == coincidencias$title)










































































































































































