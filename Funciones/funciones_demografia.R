# funciones de Demografía

library(stringr)

# Tasas calculadas y multiplicadas por mil

func_dem_TasaBruta_t = function(NumEventos_t,PobMedia_t) {
  return((NumEventos_t/PobMedia_t)*1000)
}

func_dem_TasaGeneral_t = function(NumEventos_t,PobMedia_Referida_t) {
  return((NumEventos_t/PobMedia_Referida_t)*1000)
}

func_dem_TasaEspecifica_t = function(vNumEventos_x_t,vPobMedia_x_t) {
  return((vNumEventos_x_t/vPobMedia_x_t)*1000)
}

func_dem_ISinteticoCoyuntural_t = function(vTasasEspecificas_x_t) {
  return(sum(vTasasEspecificas_x_t,na.rm = T)/1000)
}

func_dem_EdadMedia_t = function(vTasasEspecificas_x_t,Edad.marcas) {
  numer = sum(vTasasEspecificas_x_t * Edad.marcas,na.rm = T)/1000
  denom = func_dem_ISinteticoCoyuntural_t(vTasasEspecificas_x_t)
  return(numer/denom)
}



################
#### OTRAS UTILIDADES
################


func_elimina_codigo_CCAA = function(columna) {
  
  pattern <- "\\d\\d\\s"
  reemplazo = ""
  nombre = as.character(stringr::str_replace_all(columna,pattern,reemplazo))
  nombre = stringr::str_trim(nombre)
  return(nombre)
}



func_extrae_codigo_CCAA = function(columna) {
  pattern <- "\\d\\d"
  codigo = as.character(stringr::str_extract(columna,pattern)) 
  codigo = stringr::str_trim(codigo)
  #codigo[1] = "00"
  return(codigo)
}


func_extrae_notienen_codigos = function(columna,columnacodigos) {
  ind = which(is.na(columnacodigos))
  salida = unique(columna[ind])
  return(salida)
}


func_asigna_codigos_CCAA = function(columna,columna_codigos,etiqueta,nuevocodigo) {
  ind = which(columna==etiqueta)
  columna_codigos[ind] = nuevocodigo
  return(columna_codigos)
}

func_extrae_Num_Edad = function(columna) {
  pattern <- "\\d+\\s"
  codigo = as.character(stringr::str_extract(columna,pattern)) 
  codigo = stringr::str_trim(codigo)
  #codigo[1] = "00"
  return(codigo)
}

