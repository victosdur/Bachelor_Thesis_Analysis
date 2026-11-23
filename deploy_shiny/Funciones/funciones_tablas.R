## Fichero: funciones_tablas.R

# func_salida_tablas(datos1,
#                    salida="latex",
#                    apaisadalatex=FALSE,
#                    variaspaginas=TRUE,
#                    fuentesize=NULL) # 8, 2, 12, ...

# func_salida_excel(df01,
#                    ficexcel,
#                    titulo="Hoja 1",
#                    fijaFC = TRUE,
#                   anchuraAuto = TRUE)

# func_salida_excel_varias(l_df01,
#                            ficexcel,
#                            vtitulo=paste("Hoja",1:length(l_df01)),
#                            fijaFC = TRUE,
#                            anchuraAuto = TRUE) 

# Carga de paquetes necesarios --------------------------------------------

library(readxl)
library(knitr)
library(dplyr)
library(kableExtra)

library(openxlsx)

# funciones tablas con kableExtra: 1 --------------------------------------
# salida puede ser: "latex" o "html"

func_salida_tablas = function(datos1,
                              salida="latex",
                              apaisadalatex=FALSE,
                              variaspaginas=TRUE,
                              fuentesize=NULL) {  # 8, 2, 12, ...
  if (salida=="latex") {
    if (apaisadalatex) {
      if (variaspaginas) {
        if (!is.null(fuentesize)) {
          datos1 %>% 
            kable(salida,longtable=TRUE,booktabs=TRUE) %>% 
            kable_styling(latex_options = c("striped","repeat_header"),
                          font_size = fuentesize,
                          repeat_header_text="(contin\\'ua)") %>% 
            landscape()
          
        } else {  
          datos1 %>% 
            kable(salida,longtable=TRUE,booktabs=TRUE) %>% 
            kable_styling(latex_options = c("striped","repeat_header"),
                          repeat_header_text="(contin\\'ua)") %>% 
            landscape()
        }  
        
      } else {
        datos1 %>% 
          kable(salida,booktabs=TRUE) %>% 
          kable_styling(latex_options = c("striped", "scale_down")) %>% 
          landscape()
      }
    } else {
      if (!is.null(fuentesize)) {
        datos1 %>% 
          kable(salida,longtable=TRUE,booktabs=TRUE) %>% 
          kable_styling(latex_options = c("striped","repeat_header"),
                        font_size = fuentesize,
                        repeat_header_text="(contin\\'ua)")
      } else {
        datos1 %>% 
          kable(salida,longtable=TRUE,booktabs=TRUE) %>% 
          kable_styling(latex_options = c("striped","repeat_header"),
                        repeat_header_text="(contin\\'ua)")
      }
    }
  } else {
    if (!is.null(fuentesize)) {
      datos1 %>% 
        kable(salida) %>% 
        kable_styling(bootstrap_options = c("striped", "hover",
                                            "condensed","responsive"),
                      font_size = fuentesize)
    } else {
      datos1 %>% 
        kable(salida) %>% 
        kable_styling(bootstrap_options = c("striped", "hover",
                                            "condensed","responsive"),
                      full_width = T)
      
    }  
  }
}


# Funciones excel: exportar 1 df ---------------------------------------------

func_salida_excel = function(df01,
                             ficexcel,
                             titulo="Hoja 1",
                             fijaFC = TRUE,
                             anchuraAuto = TRUE) {
  options("openxlsx.numFmt" = "#,##0")
  wb <- createWorkbook()
  addWorksheet(wb, titulo)
  #writeDataTable(wb, 1, df, startRow = 16, startCol = 2, 
  #                 tableStyle = "TableStyleLight2")
  writeDataTable(wb, 1, df01, 
                 tableStyle = "TableStyleLight2")
  if (fijaFC) {
    ## fija la primera fila y columna
    freezePane(wb, sheet = 1, firstRow = TRUE, firstCol = TRUE) 
  }
  if (anchuraAuto) {
    ## Establecer anchos automáticos de columnas
    #setColWidths(wb, sheet = 1, cols = 1:21, widths = "auto")
    setColWidths(wb, sheet = 1, cols = 1:1, widths = "auto")
  }
  # Abrir Excel para ver como queda actualmente
  #openXL(wb)
  # Grabar la hoja en un fichero Excel
  saveWorkbook(wb, ficexcel, overwrite = TRUE)
}


# Funciones excel: exportar varios df ---------------------------------------------

# df01 = cars
# df02 = iris
# df03 = cars
# l_df01 = list(df01,df02,df03)
# vtitulo0 = c("Primera","Segunda","Tercera")
# func_salida_excel_varias(l_df01,ficexcel = "jj.xlsx",vtitulo=vtitulo0)

func_salida_excel_varias = function(l_df01,
                                    ficexcel,
                                    vtitulo=paste("Hoja",1:length(l_df01)),
                                    fijaFC = TRUE,
                                    anchuraAuto = TRUE) {
  options("openxlsx.numFmt" = "#,##0")
  wb <- createWorkbook()
  for (i in 1:length(l_df01)) {
    addWorksheet(wb, vtitulo[i])
    
    writeDataTable(wb, i, l_df01[[i]], 
                   tableStyle = "TableStyleLight2")
    if (fijaFC) {
      ## fija la primera fila y columna
      freezePane(wb, sheet = i, firstRow = TRUE, firstCol = TRUE) 
    }
    if (anchuraAuto) {
      ## Establecer anchos automáticos de columnas
      #setColWidths(wb, sheet = 1, cols = 1:21, widths = "auto")
      setColWidths(wb, sheet = i, cols = 1:1, widths = "auto")
    }
  }
  
  #writeDataTable(wb, 1, df, startRow = 16, startCol = 2, 
  #                 tableStyle = "TableStyleLight2")
  # Abrir Excel para ver como queda actualmente
  #openXL(wb)
  # Grabar la hoja en un fichero Excel
  saveWorkbook(wb, ficexcel, overwrite = TRUE)
}

