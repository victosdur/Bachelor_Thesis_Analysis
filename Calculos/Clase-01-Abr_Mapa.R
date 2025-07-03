library(dplyr)
library(ggplot2)
library(plotly)
library(highcharter)
library(quantmod)
library(survival)
data("citytemp")


library(jsonlite)
mydata <- fromJSON("https://code.highcharts.com/mapdata/countries/es/es-all.geo.json", flatten = TRUE)
mydata$features
"countries/es/es-all"
Myhcmap2 <- fromJSON("data/shapefiles_espana_ccaa_2.geojson", simplifyVector = FALSE)
Myhcmap2$features

ccaacsv <- read_csv2("data/ccaa.csv", locale = locale(encoding="ISO-8859-1"),
                     col_types = cols(Codigo = col_character()))

library(highcharter)

#2009
library(readr)
library(tidyverse) #para realizar las uniones con dplyr
basicos_hogar<- read_csv("Microdatos/datos2009/esudb09d.csv")
basicos_persona<- read_csv("Microdatos/datos2009/esudb09r.csv")
detallados_hogar<- read_csv("Microdatos/datos2009/esudb09h.csv")
detallados_persona<- read_csv("Microdatos/datos2009/esudb09p.csv")
union_hogar = basicos_hogar %>% inner_join(detallados_hogar,c("DB030"="HB030"))
library(magrittr)
union_hogar %<>% select(DB030,DB040,DB090,HY020,HY010,vhRentaa,vhPobreza,HX040,HX240,vhMATDEP) 
library(ineq)
library(reldist)
union_personas_casa=basicos_persona %>% separate(RB030,into = c("idhogar","orden"),sep = -2,convert=TRUE) %>%left_join(union_hogar,by=c("idhogar"="DB030"))

union_personas_casa %<>% group_by(DB040) %>% summarise(Arope=sum(vrEU2020*RB050)/sum(RB050),
                                                       Igini=gini(HY020/HX240,weights = RB050),
)
union_hogar %<>% group_by(DB040) %>% summarise(renta_hogar=sum(DB090*vhRentaa)/sum(DB090),
                                               personas_hogar=sum(HX040*DB090)/sum(DB090),
                                               renta_persona=sum(DB090*vhRentaa)/sum(DB090)/personas_hogar,
                                               hogatpobreza=sum(DB090*vhPobreza)/sum(DB090))


union_hogar$codigo = c(12,3,6,16,15,17,2,13,7,8,11,9,10,4,1,14,18,19,5)
union_hogar %<>% arrange(codigo)
union_hogar %<>% mutate(renta_hogar_ccaa=round(renta_hogar,2),
                        personas_hogar=round(personas_hogar,2),
                        renta_persona_ccaa=round(renta_persona,2),
                        hogatpobreza=round(100*hogatpobreza,2))
union_personas_casa$codigo = c(12,3,6,16,15,17,2,13,7,8,11,9,10,4,1,14,18,19,5)
union_personas_casa %<>% arrange(codigo) %>% mutate(
  Arope=round(100*Arope,2),
  Gini=round(100*Igini,2))

highchart() %>%
  hc_add_series_map(Myhcmap2, union_hogar, value = "renta_hogar_ccaa", joinBy = c("codaut_sin", "codigo"),
                    name="Renta neta media por hogar",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(24000, 38000, by=2000))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_title(text="Renta neta media por hogar por Comunidad Autonoma 2009") %>% hc_caption(text="Fuente:Elaboración propia TFG")

highchart() %>%
  hc_add_series_map(Myhcmap2, union_hogar, value = "personas_hogar", joinBy = c("codaut_sin", "codigo"),
                    name="Personas por hogar",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(2, 4, length.out=5))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 1) %>%
  hc_title(text="Número medio de personas por hogar por Comunidad Autónoma") %>% hc_caption(text="Fuente:Elaboración propia TFG")

highchart() %>%
  hc_add_series_map(Myhcmap2, union_hogar, value = "renta_persona_ccaa", joinBy = c("codaut_sin", "codigo"),
                    name="Renta neta media por persona",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(8000, 15000, by=1000))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_title(text="Renta neta media por persona por Comunidad Autónoma") %>% hc_caption(text="Fuente:Elaboración propia TFG")




highchart() %>%
  hc_add_series_map(Myhcmap2, union_hogar, value = "hogatpobreza", joinBy = c("codaut_sin", "codigo"),
                    name="Tasa de hogares en pobreza",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(5, 40, length.out=10 ))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_title(text="Tasa de hogares en pobreza por Comunidad Autonoma") %>% hc_caption(text="Fuente:Elaboración propia TFG")




highchart() %>%
  hc_add_series_map(Myhcmap2, union_personas_casa, value = "Arope", joinBy = c("codaut_sin", "codigo"),
                    name="Tasa de pobreza Arope",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(10, 40, length.out=8 ))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_title(text="Tasa de pobreza AROPE por Comunidad Autonoma") %>% hc_caption(text="Fuente:Elaboración propia TFG")

highchart() %>%
  hc_add_series_map(Myhcmap2, union_personas_casa, value = "Gini", joinBy = c("codaut_sin", "codigo"),
                    name="C Gini",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(26, 42, length.out=8 ))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_title(text="Coeficiente de Gini por Comunidad Autonoma") %>% hc_caption(text="Fuente:Elaboración propia TFG")


#2020
basicos_hogar<- read_csv("Microdatos/datos_2020/esudb20d.csv")
basicos_persona<- read_csv("Microdatos/datos_2020/esudb20r.csv")
detallados_hogar<- read_csv("Microdatos/datos_2020/esudb20h.csv")
detallados_persona<- read_csv("Microdatos/datos_2020/esudb20p.csv")

union_hogar = basicos_hogar %>% inner_join(detallados_hogar,c("DB030"="HB030"))
union_hogar %<>% select(DB030,DB040,DB090,HY020,HY010,vhRentaa,vhPobreza,HX040,HX240,vhMATDEP) 

union_personas_casa=basicos_persona %>% separate(RB030,into = c("idhogar","orden"),sep = -2,convert=TRUE) %>%left_join(union_hogar,by=c("idhogar"="DB030"))

union_personas_casa %<>% group_by(DB040) %>% summarise(Arope=sum(vrEU2020*RB050)/sum(RB050),
                                                       Igini=gini(HY020/HX240,weights = RB050),
)
union_hogar %<>% group_by(DB040) %>% summarise(renta_hogar=sum(DB090*vhRentaa)/sum(DB090),
                                               personas_hogar=sum(HX040*DB090)/sum(DB090),
                                               renta_persona=sum(DB090*vhRentaa)/sum(DB090)/personas_hogar,
                                               hogatpobreza=sum(DB090*vhPobreza)/sum(DB090))


union_hogar$codigo = c(12,3,6,16,15,17,2,13,7,8,11,9,10,4,1,14,18,19,5)
union_hogar %<>% arrange(codigo)
union_hogar %<>% mutate(renta_hogar_ccaa=round(renta_hogar,2),
                        personas_hogar=round(personas_hogar,2),
                        renta_persona_ccaa=round(renta_persona,2),
                        hogatpobreza=round(100*hogatpobreza,2))
union_personas_casa$codigo = c(12,3,6,16,15,17,2,13,7,8,11,9,10,4,1,14,18,19,5)
union_personas_casa %<>% arrange(codigo) %>% mutate(
  Arope=round(100*Arope,2),
  Gini=round(100*Igini,2))


highchart() %>%
  hc_add_series_map(Myhcmap2, union_hogar, value = "renta_hogar_ccaa", joinBy = c("codaut_sin", "codigo"),
                    name="Renta neta media por hogar",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(22000, 38000, by=2000))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_title(text="Renta neta media por hogar por Comunidad Autónoma") %>% hc_caption(text="Fuente:Elaboración propia TFG")

highchart() %>%
  hc_add_series_map(Myhcmap2, union_hogar, value = "personas_hogar", joinBy = c("codaut_sin", "codigo"),
                    name="Personas por hogar",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(2, 4, length.out=5))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 1) %>%
  hc_title(text="Número medio de personas por hogar por Comunidad Autónoma") %>% hc_caption(text="Fuente:Elaboración propia TFG")

highchart() %>%
  hc_add_series_map(Myhcmap2, union_hogar, value = "renta_persona_ccaa", joinBy = c("codaut_sin", "codigo"),
                    name="Renta neta media por persona",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(9000, 16000, by=1000))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_title(text="Renta neta media por persona por Comunidad Autónoma") %>% hc_caption(text="Fuente:Elaboración propia TFG")




highchart() %>%
  hc_add_series_map(Myhcmap2, union_hogar, value = "hogatpobreza", joinBy = c("codaut_sin", "codigo"),
                    name="Tasa de hogares en pobreza",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(5, 40, length.out=10 ))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_title(text="Tasa de hogares en pobreza por Comunidad Autónoma") %>% hc_caption(text="Fuente:Elaboración propia TFG")




highchart() %>%
  hc_add_series_map(Myhcmap2, union_personas_casa, value = "Arope", joinBy = c("codaut_sin", "codigo"),
                    name="Tasa de pobreza Arope",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(10, 45, length.out=8 ))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_title(text="Tasa de pobreza AROPE por Comunidad Autónoma") %>% hc_caption(text="Fuente:Elaboración propia TFG")

highchart() %>%
  hc_add_series_map(Myhcmap2, union_personas_casa, value = "Gini", joinBy = c("codaut_sin", "codigo"),
                    name="C Gini",
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_colorAxis(dataClasses = color_classes(seq(26, 44, length.out=8 ))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_title(text="Coeficiente de Gini por Comunidad Autónoma") %>% hc_caption(text="Fuente:Elaboración propia TFG")
