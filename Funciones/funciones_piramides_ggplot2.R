library(ggplot2)
library(ggthemes)
library(tidyverse)

# func_piramide_ggplot2()
# func_piramides_enfrentadas_ggplot2()
# func_piramide_superpuestas_ggplot2()
# func_piramide_compuestasCateg_ggplot2()
# func_agrupar_variable()
# func_etiquetas_gruposEdad(metodo=1,final=85,labelfinal=NULL) 

###############################################################
######
###############################################################

# Los datos, df o tibble, deben tener tres columnas: 
#   Edad (chr o fct) (simples o grupos edad), 
#   Sexo (chr o fct) ("Hombre" o "Mujer"), 
#   Poblacion (num) (absolutos)
#   Caso (chr o fct) (variable categórica para pirámides compuestasCateg)

func_piramide_ggplot2 = function(datosPiramide,
                                 porcentajes=TRUE,
                                 etiquetas=FALSE,etiquetas.size=4,
                                 UsaCaso=FALSE,
                                 etiq.hombre="Hombre",etiq.mujer="Mujer",
                                 colorear="Sexo",colores=NULL) {
  #colnames(datos)[2] = "Genero"
  #browser()
  df2 = datosPiramide
  df2$Poblacion[df2$Sexo==etiq.hombre] = - df2$Poblacion[df2$Sexo==etiq.hombre]
  Gcadporcen = ""
  if (porcentajes) {
    Gcadporcen = "%"
    
    if (UsaCaso) {  # UsaCaso=TRUE -> ("Caso" %in% colnames(df2) )
      df3 = df2 %>% 
        dplyr::group_by(Caso) %>% 
        dplyr::summarise(
          Subtotal = sum(abs(Poblacion))
        )
      fCaso = function(x) {
        df3$Subtotal[df3$Caso==x]
      }
      
      vfCaso = function(x) {
        sapply(x,fCaso)
      }
      
      df2 = df2 %>% 
        dplyr::mutate(
          Total = vfCaso(Caso),
          Porcentajes = 100*round(Poblacion/Total,4)
        )
    } else {
      df2 = df2 %>% 
        dplyr::mutate(
          Total = sum(abs(Poblacion)),
          Porcentajes = 100*round(Poblacion/Total,4)
        )      
    }
    df2 = df2 %>% 
      dplyr::rename(
        Pob = Poblacion
      )
    df2$Poblacion = df2$Porcentajes
  }  
  
  if (colorear=="Edad") {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, fill = Edad)) +
      geom_bar(data = subset(df2, Sexo == etiq.mujer), stat = "identity") +
      geom_bar(data = subset(df2, Sexo == etiq.hombre), stat = "identity")
  } else if (colorear=="Poblacion") {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, fill = Poblacion)) +
      geom_bar(data = subset(df2, Sexo == etiq.mujer), stat = "identity") +
      geom_bar(data = subset(df2, Sexo == etiq.hombre), stat = "identity")
  } else {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, fill = Sexo)) +
      geom_bar(data = subset(df2, Sexo == etiq.mujer), stat = "identity") +
      geom_bar(data = subset(df2, Sexo == etiq.hombre), stat = "identity")
  }

  if (etiquetas) {
      pyramidGH <- pyramidGH +
      geom_text(data = subset(df2, Sexo == etiq.hombre), 
                aes(y = Poblacion,label = paste0((-1)*Poblacion,Gcadporcen)), 
                size = etiquetas.size, hjust = 1) +
        geom_text(data = subset(df2, Sexo == etiq.mujer), 
                  aes(y = Poblacion,label = paste0(Poblacion,Gcadporcen)), 
                  size = etiquetas.size, hjust = 0)
  }
  
  if (porcentajes) {
    Vmax = round(max(abs(df2$Poblacion)),0) + 1
    pyramidGH <- pyramidGH +
      scale_y_continuous(breaks = c(seq(-Vmax, 0, 1), seq(1, Vmax, 1)),
                         labels = paste0(as.character(c(seq(Vmax, 0, -1),
                                              seq(1, Vmax, 1))), "%"))
      
  } else {
    tmp1 = max(round(range(df2$Poblacion/1000000),0))
    if (tmp1>=2) {
      pyramidGH <- pyramidGH +
        scale_y_continuous(labels = paste0(as.character(c(seq(tmp1, 0, -1), 
                                              seq(1, tmp1, 1))), "m")) 
    } else {
      rango = range(abs(df2$Poblacion))
      ampli = diff(rango)
      vbr = round(c(seq(-rango[2],0,length.out = 3), 
                    seq(1,rango[2],length.out = 3)),0)
      vbr2 = round(c(seq(rango[2],0,length.out = 3), 
                    seq(1,rango[2],length.out = 3)),0)
      pyramidGH <- pyramidGH +
        scale_y_continuous(
          breaks = vbr,
          labels = paste0(as.character(vbr2), "")) 
      
    }
  }
  
  if (!is.null(colores)) {
    pyramidGH = pyramidGH +
      scale_fill_manual(values=colores)
  }
  
  
  if (porcentajes) {
    pyramidGH <- pyramidGH + 
      labs(y = "Porcentajes Población",
           x = "Edades") 
    
  } else {
    pyramidGH <- pyramidGH + 
      labs(y = "Población",
           x = "Edades")
  }
  
  if (colorear!="Sexo") {
    pyramidGH <- pyramidGH +
      guides(fill=FALSE)
  }
  
  pyramidGH <- pyramidGH + 
    coord_flip()
  
  pyramidGH +
    theme_light()
  
  
}


###############################################################
######
###############################################################

func_piramides_enfrentadas_ggplot2 = function(datosPiramide,
                                              porcentajes=TRUE,
                                              etiquetas=FALSE,etiquetas.size=4,
                                              UsaCaso=TRUE,
                                              etiq.hombre="Hombre",etiq.mujer="Mujer",
                                              colorear="Sexo",colores=NULL,
                                              nfilas=NULL,ncols=NULL) {
  #browser()
  p = func_piramide_ggplot2(datosPiramide,porcentajes,
                  etiquetas,etiquetas.size,UsaCaso,
                  etiq.hombre,etiq.mujer,
                  colorear,colores)
  p = p + 
    facet_wrap( ~Caso, nrow = nfilas,ncol = ncols)
  p 
  
}




###############################################################
######
###############################################################


func_piramide_superpuestas_ggplot2 = function(datosPiramide,
                                 porcentajes=TRUE,
                                 etiquetas=FALSE,etiquetas.size=4,
                                 colores = NULL,
                                 transparente=FALSE,
                                 alfa=0.1,bar.size=1,
                                 etiq.hombre="Hombre",etiq.mujer="Mujer") {
  #colnames(datos)[2] = "Genero"
  #browser()
  UsaCaso=TRUE
  df2 = datosPiramide
  df2$Caso = factor(df2$Caso)
  df2$Poblacion[df2$Sexo==etiq.hombre] = - df2$Poblacion[df2$Sexo==etiq.hombre]
  Gcadporcen = ""
  if (porcentajes) {
    Gcadporcen = "%"
    
    if (UsaCaso) {  # UsaCaso=TRUE -> ("Caso" %in% colnames(df2) )
      df3 = df2 %>% 
        dplyr::group_by(Caso) %>% 
        dplyr::summarise(
          Subtotal = sum(abs(Poblacion))
        )
      fCaso = function(x) {
        df3$Subtotal[df3$Caso==x]
      }
      
      vfCaso = function(x) {
        sapply(x,fCaso)
      }
      
      df2 = df2 %>% 
        mutate(
          Total = vfCaso(Caso),
          Porcentajes = 100*round(Poblacion/Total,4)
        )
    } else {
      df2 = df2 %>% 
        mutate(
          Total = sum(abs(Poblacion)),
          Porcentajes = 100*round(Poblacion/Total,4)
        )      
    }
    df2 = df2 %>% 
      dplyr::rename(
        Pob = Poblacion
      )
    df2$Poblacion = df2$Porcentajes
  }  
  
  if (!transparente) {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, 
                                 color = Caso,fill="transparent"))  
  } else {
    pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, 
                                 color = Caso,fill=Caso))  
  }
  
  UniCasos = unique(df2$Caso)  
  for (i in 1:length(UniCasos)) {
    pyramidGH = pyramidGH +
      # geom_bar(data = subset(df2, Caso==UniCasos[i]), 
      #        stat = "identity",fill="transparent")  
      geom_bar(data = subset(df2, Caso==UniCasos[i]),
             stat = "identity",size=bar.size,alpha=alfa)
      
      
  }

  if (etiquetas) {
    pyramidGH <- pyramidGH +
      geom_text(data = subset(df2, Sexo == etiq.hombre), 
                aes(y = Poblacion,label = paste0((-1)*Poblacion,Gcadporcen)), 
                size = etiquetas.size, hjust = 1) +
      geom_text(data = subset(df2, Sexo == etiq.mujer), 
                aes(y = Poblacion,label = paste0(Poblacion,Gcadporcen)), 
                size = etiquetas.size, hjust = 0)
  }
  
  if (porcentajes) {
    Vmax = round(max(abs(df2$Poblacion)),0) + 1
    pyramidGH <- pyramidGH +
      scale_y_continuous(breaks = c(seq(-Vmax, 0, 1), seq(1, Vmax, 1)),
                         labels = paste0(as.character(c(seq(Vmax, 0, -1), 
                                                        seq(1, Vmax, 1))), "%"))
    
  } else {
    tmp1 = max(round(range(df2$Poblacion/1000000),0))
    if (tmp1>=2) {
      pyramidGH <- pyramidGH +
        scale_y_continuous(labels = paste0(as.character(c(seq(tmp1, 0, -1), 
                                                          seq(1, tmp1, 1))), "m")) 
    } else {
      rango = range(abs(df2$Poblacion))
      ampli = diff(rango)
      vbr = round(c(seq(-rango[2],0,length.out = 3), 
                    seq(1,rango[2],length.out = 3)),0)
      vbr2 = round(c(seq(rango[2],0,length.out = 3), 
                     seq(1,rango[2],length.out = 3)),0)
      pyramidGH <- pyramidGH +
        scale_y_continuous(
          breaks = vbr,
          labels = paste0(as.character(vbr2), "")) 
      
    }
  }
  
  if (!is.null(colores)) {
    pyramidGH <- pyramidGH +
      scale_fill_manual(values=colores) +
      scale_color_manual(values=colores)
  }
  
  if (porcentajes) {
    pyramidGH <- pyramidGH + 
      labs(y = "Porcentajes Población",
           x = "Edades") 
    
  } else {
    pyramidGH <- pyramidGH + 
      labs(y = "Población",
           x = "Edades")
  }
  
  pyramidGH <- pyramidGH + 
    coord_flip()
  
  pyramidGH +
    theme_light()
  
  
}



###############################################################
######
###############################################################


func_piramide_compuestasCateg_ggplot2 = function(datosPiramide,
                                              porcentajes=TRUE,
                                              etiquetas=FALSE,etiquetas.size=4,
                                              colores = NULL,ordeninverso=FALSE,
                                              alfa=1,bar.size=1,
                                              etiq.hombre="Hombre",etiq.mujer="Mujer") {
  #colnames(datos)[2] = "Genero"
  #browser()
  UsaCaso=FALSE
  transparente=TRUE
  df2 = datosPiramide
  df2$Caso = factor(df2$Caso)
  df2$Poblacion[df2$Sexo==etiq.hombre] = - df2$Poblacion[df2$Sexo==etiq.hombre]
  Gcadporcen = ""
  if (porcentajes) {
    Gcadporcen = "%"

    df2 = df2 %>% 
      mutate(
        Total = sum(abs(Poblacion)),
        Porcentajes = 100*round(Poblacion/Total,4)
      )      
    
    df2 = df2 %>% 
      dplyr::rename(
        Pob = Poblacion
      )
    df2$Poblacion = df2$Porcentajes
  }  
  
  pyramidGH <- ggplot(df2, aes(x = Edad, y = Poblacion, 
                                 fill=Caso))  

  if (ordeninverso) {
    pyramidGH = pyramidGH +
      geom_bar(stat="identity",color="black",
               position = position_stack(reverse = TRUE),
               size=bar.size,alpha=alfa)
  } else { 
    pyramidGH = pyramidGH +
        geom_bar(stat="identity",color="black",
             size=bar.size,alpha=alfa)
  }
  
  if (etiquetas) {
    pyramidGH <- pyramidGH +
      geom_text(data = subset(df2, Sexo == etiq.hombre), 
                aes(y = Poblacion,label = paste0((-1)*Poblacion,Gcadporcen)), 
                size = etiquetas.size, hjust = 1) +
      geom_text(data = subset(df2, Sexo == etiq.mujer), 
                aes(y = Poblacion,label = paste0(Poblacion,Gcadporcen)), 
                size = etiquetas.size, hjust = 0)
  }
  
  if (porcentajes) {
    Vmax = round(max(abs(df2$Poblacion)),0) + 1
    
    pyramidGH <- pyramidGH +
      scale_y_continuous(breaks = c(seq(-Vmax, 0, 1), seq(1, Vmax, 1)),
                         labels = paste0(as.character(c(seq(Vmax, 0, -1),
                                                        seq(1, Vmax, 1))), "%"))
    
  } else {
    tmp1 = max(round(range(df2$Poblacion/1000000),0))
    if (tmp1>=2) {
      pyramidGH <- pyramidGH +
        scale_y_continuous(labels = paste0(as.character(c(seq(tmp1, 0, -1), 
                                                          seq(1, tmp1, 1))), "m")) 
    } else {
      rango = range(abs(df2$Poblacion))
      ampli = diff(rango)
      vbr = round(c(seq(-rango[2],0,length.out = 3), 
                    seq(1,rango[2],length.out = 3)),0)
      vbr2 = round(c(seq(rango[2],0,length.out = 3), 
                     seq(1,rango[2],length.out = 3)),0)
      pyramidGH <- pyramidGH +
        scale_y_continuous(
          breaks = vbr,
          labels = paste0(as.character(vbr2), "")) 
      
    }
  }
  
  if (!is.null(colores)) {
    pyramidGH <- pyramidGH +
      scale_fill_manual(values=colores) 
  }
  
  if (porcentajes) {
    pyramidGH <- pyramidGH + 
      labs(y = "Porcentajes Población",
           x = "Edades") 
    
  } else {
    pyramidGH <- pyramidGH + 
      labs(y = "Población",
           x = "Edades")
  }
  
  pyramidGH <- pyramidGH + 
    coord_flip()
  
  pyramidGH +
    theme_light()
  
  
}


###############################################################
######
###############################################################


func_agrupar_variable = function(variable,metodo=1,final=85,
                                 vbreaks=NULL,vlabels=NULL,labelfinal=NULL) {
  
  if (is.null(labelfinal)) {
    lbfinal = paste0(final,"+")
  } else {
    lbfinal = labelfinal
  }
  
  if (metodo==1) {
    Agrup = cut(variable,breaks = c(0,1,seq(5,final,by=5),200),include.lowest = T,
                labels = c("0","1-4",paste0(seq(5,final-5,by=5),"-",seq(9,final,by=5)),lbfinal))
    return(Agrup)
  }
  if (metodo==2) {
    Agrup = cut(variable,breaks = c(seq(0,final,by=5),200),include.lowest = T,
                labels = c(paste0(seq(0,final-5,by=5),"-",seq(4,final,by=5)),lbfinal))
    return(Agrup)
  }
  if (!is.null(vbreaks)) {
    if (is.null(vlabels)) {
      Agrup = cut(variable,breaks = vbreaks,include.lowest = T)
    } else {
      Agrup = cut(variable,breaks = vbreaks,include.lowest = T,
                  labels = vlabels )
    }
    return(Agrup)
  }
  
  
}


###############################################################
######
###############################################################


func_etiquetas_gruposEdad = function(metodo=1,final=85,labelfinal=NULL) {
  
  if (is.null(labelfinal)) {
    lbfinal = paste0(final,"+")
  } else {
    lbfinal = labelfinal
  }
  
  if (metodo==1) {
    Agrup = c("0","1-4",paste0(seq(5,final-5,by=5),"-",seq(9,final,by=5)),lbfinal)
    return(Agrup)
  }
  if (metodo==2) {
    Agrup = c(paste0(seq(0,final-5,by=5),"-",seq(4,final,by=5)),lbfinal)
    return(Agrup)
  }

}
