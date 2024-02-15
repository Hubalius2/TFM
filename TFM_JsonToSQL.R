library(jsonlite)
library(purrr)
library(tidyr)
library(tidyverse)

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




fichas_tecnicas_final <- ft_pro[, c("Brand", "Model", "Precio", "Caja de cambios", "Combustible", "Potencia_Maxima_CV",
                                    "Tracción", "Vol_Maletero_Litros", "Aceleracion_(0_100km/h)_s",
                                    "Velocidad_Máxima_km/h","Versión", "Nº de puertas",
                                    "Largo_mm", "Ancho_mm", "Alto_mm",
                                    "Nº de plazas","Peso_con_conductor_kg", "Potencia máxima eléctrica",
                                    "Wattios_motor", "Consumo_medio_eléctrico_WLTP", "Autonomía_eléctrica_WLTP",
                                    "Carga_min", "Depósito_litros", "Consumo_medio_L/100km",
                                    "Consumo_urbano_L/100km", "Consumo_extraurbano_L/100km")]
























































































































































































































