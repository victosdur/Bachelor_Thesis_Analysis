# funciones de mapas para Demografía
# ámbitos: comunidad autónoma, provincias
## LISTADO DE FUNCIONES EN ESTE FICHERO:
# func_mapademografia_prov(dtmapa,Titulo,categorias = 7,colores = NULL,
#                           LeyendaPos = "bottomleft",Leyenda.size=0.5)
# func_extrae_codigo_provincia(vprovincias)
# func_crea_colores_brewer(cuantos,que_paletacolor=3)  
# func_mapademografia_ccaa(dtmapa,Titulo,categorias = 7,colores = NULL,
#                           LeyendaPos = "bottomleft",Leyenda.size=0.5)
# func_extrae_codigo_ccaa(vCCAA,ConvierteCodSIANE=TRUE)  
# func_mapademografia_municipios = function(dtmapa,Titulo,categorias = 7,colores = NULL,
#                            LeyendaPos = "bottomleft",Leyenda.size=0.5) 
# func_obtiene_codigo_prov(vProv)
  
## Con Siane

# <https://github.com/rOpenSpain/Siane/blob/master/README.md>
# <http://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=CAANE#>
devtools::install_github("rOpenSpain/Siane")
install.packages("devtools")
install.packages("Rtools")
library(Siane)
library(raster)
library(maptools)
library(RColorBrewer)
library(classInt)
library(ggplot2)
load("funciones_mapas_SIANE.RData")


####
# Data.frame con dos columnas al menos: Codigo, Valor
# - Código provincial (02, 03, ...) irá en la variable-columna: Codigo
# - Variable que se representará (num): Valor



func_mapademografia_prov = function(dtmapa,Titulo,categorias = 7,colores = NULL,
                                    LeyendaPos = "bottomleft",Leyenda.size=0.5) {
  #browser()
  VerLevel <- "Provincias"
  #canarias <- VerCanarias
  #scale <- "6m" # "3m" also accepted
  #obj = "/Users/calvo/Downloads/SIANE/"
  
  #LeyendaPos = c("bottomright", "bottom", "bottomleft", "left", "topleft", 
  #                "top", "topright", "right", "center")
  
  # shp <- siane_map(obj = dir.SIANE, canarias = VerCanarias, 
  #                  year = VerYear, level = VerLevel, 
  #                  scale = VerScale)

  #shp_prov6m
    
  suppressMessages( 
    shp_merged <- siane_merge(shp = shp_prov6m, df = dtmapa, by = "Codigo", value = "Valor")
  )
  
  pallete_colour <- "BuPu"
  style <- "kmeans"
  
  values_ine <- shp_merged@data[["Valor"]] # Values we want to plot are stored in the shape@data data frame
  
  if (length(categorias)==1) {
    n <- categorias
    if (is.null(colores)) {
      colors <- brewer.pal(n, pallete_colour) # A pallete from RColorBrewer
    } else {
      colors = colores
    }
    brks <- classInt::classIntervals(values_ine, n = n, style = style)
    my_pallete <- brks$brks
  } else {
    n <- length(categorias)-1
    if (is.null(colores)) {
      colors <- brewer.pal(n, pallete_colour) # A pallete from RColorBrewer
    } else {
      colors = colores
    }  
    my_pallete <- categorias
  }
  col <- colors[findInterval(values_ine, my_pallete,
                             all.inside=TRUE)] # Setting the final colors
  
  raster::plot(shp_merged,col = col) # Plot the map
  title_plot <- Titulo
  title(main = title_plot)
  #legend(legend = maptools::leglabs(round(my_pallete)), fill = colors,x = "bottomright",cex=0.7)
  legend(legend = maptools::leglabs(round(my_pallete),under="inferior a",over="superior a"), 
         fill = colors,x = LeyendaPos,cex=Leyenda.size)
  #legend(legend = maptools::leglabs(round(my_pallete)), fill = colors,x = 1,y=1,cex=Leyenda.size)
  
}


func_extrae_codigo_provincia = function(vprovincias) {
  
  resul <- sapply(vprovincias,
                  function(x) strsplit(x = as.character(x), split = " ")[[1]][1])
  return(resul)
}

func_crea_colores_brewer = function(cuantos,que_paletacolor=3) {
  paletas = c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", 
            "OrRd", "PuBu", "PuBuGn","PuRd", "Purples", "RdPu", "Reds",
            "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
  if (is.character(que_paletacolor)) {
    cual = which(que_paletacolor %in% paletas)
    if (cual>0) {
      paletacolor = que_paletacolor
    } else {
       paletacolor = "BuPu"  
    }
  } else {
    paletacolor = paletas[que_paletacolor]
  }
  colors <- brewer.pal(cuantos, paletacolor)
  return(colors)
}


####
# Data.frame con dos columnas al menos: Codigo, Valor
# - Código provincial (02, 03, ...) irá en la variable-columna: Codigo
# - Variable que se representará (num): Valor




func_mapademografia_ccaa = function(dtmapa,Titulo,categorias = 7,colores = NULL,
                                    LeyendaPos = "bottomleft",Leyenda.size=0.5) {
  #browser()
  VerLevel <- "Comunidades"
  #canarias <- VerCanarias
  #scale <- "6m" # "3m" also accepted
  #obj = "/Users/calvo/Downloads/SIANE/"
  
  #LeyendaPos = c("bottomright", "bottom", "bottomleft", "left", "topleft", 
  #                "top", "topright", "right", "center")
  
  # shp <- siane_map(obj = dir.SIANE, canarias = VerCanarias, 
  #                  year = VerYear, level = VerLevel, 
  #                  scale = VerScale)
  
  #shp_comunidades6m
  
  suppressMessages( 
    shp_merged <- siane_merge(shp = shp_comunidades6m, df = dtmapa, level = VerLevel,
                            by = "Codigo", value = "Valor")
  )
  values_ine <- shp_merged@data[["Valor"]] # Values we want to plot are stored in the shape@data data frame
  pallete_colour <- "BuPu"
  style <- "kmeans"
  if (length(categorias)==1) {
    n <- categorias
    if (is.null(colores)) {
      colors <- brewer.pal(n, pallete_colour) # A pallete from RColorBrewer
    } else {
      colors = colores
    }
    brks <- classInt::classIntervals(values_ine, n = n, style = style)
    my_pallete <- brks$brks
  } else {
    n <- length(categorias)-1
    if (is.null(colores)) {
      colors <- brewer.pal(n, pallete_colour) # A pallete from RColorBrewer
    } else {
      colors = colores
    }  
    my_pallete <- categorias
  }
  col <- colors[findInterval(values_ine, my_pallete,
                             all.inside=TRUE)] # Setting the final colors
  
  raster::plot(shp_merged,col = col) # Plot the map
  title_plot <- Titulo
  title(main = title_plot)
  #legend(legend = maptools::leglabs(round(my_pallete)), fill = colors,x = "bottomright",cex=0.7)
  legend(legend = maptools::leglabs(round(my_pallete),under="inferior a",over="superior a"), 
         fill = colors,x = LeyendaPos,cex=Leyenda.size)
  #legend(legend = maptools::leglabs(round(my_pallete)), fill = colors,x = 1,y=1,cex=Leyenda.size)
  
}



func_extrae_codigo_ccaa = function(vCCAA,ConvierteCodSIANE=TRUE) {
  
  resul <- sapply(vCCAA,
                  function(x) strsplit(x = as.character(x), split = " ")[[1]][1])
  #is.character(indenv17ccaa$Codigo)
  if (ConvierteCodSIANE) {
    resul = as.numeric(resul)+60  # En SIANE: ccaa -> 60,61,...,79
  }
  return(resul)
}






func_mapademografia_municipios = function(dtmapa,Titulo,categorias = 7,colores = NULL,
                                          LeyendaPos = "bottomleft",Leyenda.size=0.5) {
  #browser()
  VerLevel <- "Municipios"
  #canarias <- VerCanarias
  #scale <- "6m" # "3m" also accepted
  #obj = "/Users/calvo/Downloads/SIANE/"
  
  #LeyendaPos = c("bottomright", "bottom", "bottomleft", "left", "topleft", 
  #                "top", "topright", "right", "center")
  
  # shp <- siane_map(obj = dir.SIANE, canarias = VerCanarias, 
  #                  year = VerYear, level = VerLevel, 
  #                  scale = VerScale)

  #shp_municipios3m
  
  suppressMessages( 
    shp_merged <- siane_merge(shp = shp_municipios3m, df = dtmapa, level = VerLevel,
                            by = "Codigo", value = "Valor")
  )
  pallete_colour <- "BuPu"
  style <- "kmeans"
  
  values_ine <- shp_merged@data[["Valor"]] # Values we want to plot are stored in the shape@data data frame
  
  if (length(categorias)==1) {
    n <- categorias
    if (is.null(colores)) {
      colors <- brewer.pal(n, pallete_colour) # A pallete from RColorBrewer
    } else {
      colors = colores
    }
    brks <- classInt::classIntervals(values_ine, n = n, style = style)
    my_pallete <- brks$brks
  } else {
    n <- length(categorias)-1
    if (is.null(colores)) {
      colors <- brewer.pal(n, pallete_colour) # A pallete from RColorBrewer
    } else {
      colors = colores
    }  
    my_pallete <- categorias
  }
  col <- colors[findInterval(values_ine, my_pallete,
                             all.inside=TRUE)] # Setting the final colors
  
  raster::plot(shp_merged,col = col) # Plot the map
  title_plot <- Titulo
  title(main = title_plot)
  #legend(legend = maptools::leglabs(round(my_pallete)), fill = colors,x = "bottomright",cex=0.7)
  legend(legend = maptools::leglabs(round(my_pallete),under="inferior a",over="superior a"), 
         fill = colors,x = LeyendaPos,cex=Leyenda.size)
  #legend(legend = maptools::leglabs(round(my_pallete)), fill = colors,x = 1,y=1,cex=Leyenda.size)
  
}


# Para ggplot2 -------

func_shp_mapa = function(dtmapa,nivel="Provincias") {
  
  VerLevel <- "Provincias"
  #canarias <- VerCanarias
  #scale <- "6m" # "3m" also accepted
  #obj = "/Users/calvo/Downloads/SIANE/"
  
  #LeyendaPos = c("bottomright", "bottom", "bottomleft", "left", "topleft", 
  #                "top", "topright", "right", "center")
  
  # shp <- siane_map(obj = dir.SIANE, canarias = VerCanarias, 
  #                  year = VerYear, level = VerLevel, 
  #                  scale = VerScale)
  
  if (nivel=="Municipios") {
    Gshp = shp_municipios3m
    VerLevel <- "Municipios"
  } else {
    if (nivel=="Comunidades") {
      Gshp = shp_comunidades6m
      VerLevel <- "Comunidades"
    } else {
      Gshp = shp_prov6m
      VerLevel <- "Provincias"
    }
  }
  
  suppressMessages( 
    shp_merged <- siane_merge(shp = Gshp, df = dtmapa, level=VerLevel,
                              by = "Codigo", value = "Valor")
  )
  return(shp_merged)
  
}


func_obtiene_codigo_prov = function(vProv) {
  #ProvinciasCodigos = mm2
  codigos = ProvinciasCodigos$id_prov[which(ProvinciasCodigos$rotulo %in% vProv)]
  return(codigos)
}


func_conv_a_df_ggplot2 = function(objshp) {
  
  dataProjected = objshp
  # add to data a new column termed "id" composed of the rownames of data
  # dataProjected@data$id <- rownames(dataProjected@data)
  # 
  # # create a data.frame from our spatial object
  # watershedPoints <- fortify(dataProjected, region = "id")
  # 
  # # merge the "fortified" data with the data from our spatial object
  # watershedDF <- merge(watershedPoints, dataProjected@data, by = "id")
  
  dataProjected@data$id <- rownames(dataProjected@data)
  watershedPoints <- ggplot2::fortify(dataProjected, region = "id")
  watershedDF <- base::merge(watershedPoints, dataProjected@data, by = "id")
  #head(watershedDF)
  return(watershedDF) 
  
  
}




