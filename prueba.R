


#Cargar librerias.
library(jsonlite)


# Cargar el direcorio y los archivos json.
jsonDir <- "salida"
archivos <- list.files(path = jsonDir, pattern = "*.json", full.names = TRUE)
archivos

#Cargar los datos en la lista data_json
fichas_tecnicas <- data.frame()
primero <- TRUE
for (archivo in archivos){
  print(archivo)
  datos_json <- list()
  data <- fromJSON(archivo)
  datos_json <- c(datos_json, data)
  if (primero){
    fichas_tecnicas <- data.frame(t(datos_json$value))
    colnames(fichas_tecnicas) <- datos_json$feature
    primero <- FALSE
  }else{
    fichas_tecnicas <- rbind(fichas_tecnicas, datos_json$value)
  }
}

#Combinamos todos los datos en un solo Dataframe.
fichas_tecnicas2 <- do.call(rbind, lapply(datos_json, as.data.frame))
unique(fichas_tecnicas$Precio)

#--------------------------------------------------------------------------

library(jsonlite)
library(purrr)

# Directorio que contiene los archivos JSON
directorio <- "ruta/a/la/carpeta"
# Carpeta donde guardar los archivos CSV
carpeta_salida <- "ruta/a/la/carpeta_de_salida"

# Función para leer un archivo JSON y devolver un dataframe
leer_json <- function(archivo) {
  data <- fromJSON(archivo)
  return(data.frame(feature = names(data), value = unlist(data)))
}

# Crear la carpeta de salida si no existe
if (!file.exists(carpeta_salida)) {
  dir.create(carpeta_salida)
}

# Obtener la lista de archivos JSON en el directorio
archivos <- list.files(path = directorio, pattern = "\\.json$", full.names = TRUE)

# Leer cada archivo JSON y almacenar los datos en diferentes dataframes
dataframes <- map(archivos, leer_json)

# Dividir los dataframes según la cantidad de pares feature/value
dataframes_por_pares <- split(dataframes, map_int(dataframes, nrow))

# Mostrar la cantidad de dataframes para cada cantidad de pares
print(table(names(dataframes_por_pares)))

# Guardar cada dataframe en un archivo separado en la carpeta de salida
walk2(names(dataframes_por_pares), dataframes_por_pares, function(nombre, dataframe) {
  write.csv(dataframe, file.path(carpeta_salida, paste0(nombre, ".csv")), row.names = FALSE)
})


#####################################################
for (lista in dataframes_por_columnas){
  df1 <- lista[[1]]
  nombre <- ncol(df1)
  nombres_columnas <- colnames(lista[[1]])
  coincide <- TRUE
  for(df in lista){
    coincide <- identical(nombres_columnas, colnames(df))
  }
  
  df_columnas <- data.frame()
  
  if(coincide){
    for(df in lista){
      nombres_columnas_df2 <- colnames(df)
      columnas_faltantes_df2 <- setdiff(nombres_columnas, nombres_columnas_df2)
      columnas_faltantes_df1 <- setdiff(nombres_columnas_df2, nombres_columnas)
      print(columnas_faltantes_df2)
      print(columnas_faltantes_df1)
      for (col in columnas_faltantes_df2) {
        df[[col]] <- NA
      }
      
      # Agregar las columnas faltantes a df1 con valores NA
      for (col in columnas_faltantes_df1) {
        df1[[col]] <- NA
      }
      a <- identical(colnames(df1), colnames(df))
      print(a)
      df1 <- rbind(df1, df)
      write.csv(df_columnas, file.path(carpeta_salida, paste0(nombre, ".csv")), row.names = FALSE)
    }
  }
}


###############################################################################
# Dividir los dataframes según la cantidad de pares feature/value
num_columnas_por_df <- sapply(dataframes, num_columnas)
dataframes_por_columnas <- split(dataframes, num_columnas_por_df)

dataframes_finales <- list()

for (lista in dataframes_por_columnas){
  df1 <- lista[[1]]  # Tomar el primer dataframe como base
  nombres_columnas_df1 <- names(df1)
  nombre <-ncol(df1)
  print('______________________')
  print(nombre)
  lista_aux <- lista[-1]
  for(df2 in lista_aux){
    nombres_columnas_df2 <- names(df2)
    columnas_no_comunes_df1 <- setdiff(names(df1), names(df2))
    columnas_no_comunes_df2 <-setdiff(names(df2), names(df1))
    for(col in columnas_no_comunes_df1){
      df2[[col]] <- '-'
    }
    for(col in columnas_no_comunes_df2){
      df1[[col]] <- '-'
    }
    columnas_comunes <- intersect(names(df1), names(df2))
    df1_copy  <- df1[columnas_comunes]
    df2_copy  <- df2[columnas_comunes]
    df1 <- rbind(df1_copy , df2_copy)
  }
  print(ncol(df1))
  print('______________________')
  write.csv(df1, file.path(carpeta_salida, paste0(nombre, ".csv")), row.names = FALSE)
}

##############################
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

##############################
car_SQL <- data.frame()
for (i in 1:nrow(dataCar)) {
  print(i)
  if (toupper(dataCar[i, "make"]) %in% marcas_unicas$make) {
    #if (dataCar[i, "model"] %in% modelos_unicas$model) {
    #if (dataCar[i, "version"] %in% versiones_unicas$version) {
    car_SQL <- rbind(car_SQL, dataCar[i, ])
    #print(dataCar[i, "title"])
    #}
    #}
  }
}

##############################
for (i in 1:nrow(csv)){
  title_match <- dataCar$id[dataCar$title == csv[i, "title"]]
  print(title_match)
  if(csv[i, "title"] %in% dataCar$title){
    #print(csv[i, "title"])
    csv[i, "ficha_id"] <-  dataCar$id[dataCar$title == csv[i, "title"]]
  }else{
    csv[i, "ficha_id"] <-  "0"
  }
}

total <- 0
for (i in 1:nrow(csv)){
  if(csv[i, "ficha_id"] > 0){
    #print(csv[i, "title"])
    total <- total + 1
  }
}


# Instala y carga el paquete FNN si aún no lo has hecho
install.packages("FNN")
library(FNN)

# Crea un dataframe con los datos de los clientes
clientes <- data.frame(
  Cliente = 1:10,
  Edad = c(30, 35, 25, 22, 28, 32, 40, 38, 45, 50),
  MontoCompra = c(100, 80, 60, 40, 90, 70, 120, 110, 130, 150)
)

# Normaliza los datos para evitar sesgos por diferentes escalas
clientes_norm <- scale(clientes[, -1])  # excluye la columna Cliente para la normalización

# Define un nuevo cliente con el que queremos encontrar vecinos cercanos
nuevo_cliente <- c(35, 85)  # Edad = 35, Monto de Compra = 85

# Encuentra los vecinos más cercanos usando el algoritmo de vecinos más cercanos (KNN)
k <- 3  # Número de vecinos más cercanos a buscar
vecinos <- get.knnx(clientes_norm, matrix(nuevo_cliente, ncol = 2), k = k)

# Imprime los vecinos más cercanos
print(vecinos)



# Definir las dos listas
lista_ventas <- c("creationdate", "brand", "model", "version", "title", "url", "km", "year", "cubiccapacity", "mainprovince", "makeid", "modelid", "fueltypeid", "fueltype", "bodytypeid", "environmentallabel", "drivenwheelsid", "transmissiontypeid", "price_amount", "warranty_months", "offertype_id", "offertype_literal", "pack_legacyid", "pack_type", "url_true", "ficha_id")
lista_referencia <- c("creationdate", "url", "km", "provinceid", "environmentallabel", "price_amount", "url_true", "ficha_id")

# Encontrar los elementos de lista_ventas que no están en lista_referencia
elementos_faltantes_ventas <- setdiff(lista_ventas, lista_referencia)

# Mostrar los elementos faltantes
print(elementos_faltantes_ventas)



#---------------------------------------------------


# Definir la tabla de reemplazo para quitar acentos
tabla_reemplazo <- "áéíóúüñÁÉÍÓÚÜÑ"
sin_acentos <- "aeiouunAEIOUUN"

# Función para quitar acentos de una cadena

# Encontrar los elementos que están en toWrite$mainprovince pero no en provincias_unicas$province
elementos_toWrite <- toupper(unique(toWrite$mainprovince))
elementos_provincias_unicas <- toupper(unique(provincias_unicas$province))
elementos_no_coincidentes <- setdiff(elementos_toWrite, elementos_provincias_unicas)

# Mostrar los elementos que no coinciden
print(elementos_no_coincidentes)

toWrite_clean <- toWrite  
toWrite_clean <- iconv(toWrite, "UTF-8", "ASCII")

ventas_df