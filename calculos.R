library(bslib)
library(shiny)
library(tidyverse)
library(readxl)
library(ggrepel)
library(highcharter)
library(jsonlite)
library(factoextra)
library(gt)

paleta <- c("#087E8B", "#17365D", "#D97735")
tema_tfg <- function() {
  theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", colour = "#17365D"),
          plot.subtitle = element_text(colour = "#526579"),
          panel.grid.minor = element_blank(), legend.position = "bottom",
          plot.caption = element_text(colour = "#64748B"))
}
theme_set(tema_tfg())

regiones <- tibble(DB040 = c("ES11","ES12","ES13","ES21","ES22","ES23","ES24","ES30","ES41","ES42","ES43","ES51","ES52","ES53","ES61","ES62","ES63","ES64","ES70"),
  comunidad = c("Galicia","Asturias","Cantabria","País Vasco","Navarra","La Rioja","Aragón","Madrid","Castilla y León","Castilla-La Mancha","Extremadura","Cataluña","C. Valenciana","Baleares","Andalucía","Murcia","Ceuta","Melilla","Canarias"),
  codigo = c(12,3,6,16,15,17,2,13,7,8,11,9,10,4,1,14,18,19,5))

cargar_ecv <- function(anio) {
  carpeta <- if (anio == 2009) "Microdatos/datos2009" else "Microdatos/datos_2020"
  leer <- function(tipo) read_csv(file.path(carpeta, paste0("esudb", substr(anio, 3, 4), tipo, ".csv")), show_col_types = FALSE)
  hogares <- inner_join(leer("d"), leer("h"), by = c("DB030" = "HB030")) %>% mutate(DB040 = as.character(DB040))
  personas <- leer("r") %>% separate(RB030, into = c("idhogar", "orden"), sep = -2, convert = TRUE, remove = FALSE) %>% left_join(hogares, by = c("idhogar" = "DB030"))
  list(hogares = hogares, personas = personas)
}

indicadores <- function(datos, anio) {
  h <- datos$hogares %>% group_by(DB040) %>% summarise(renta_hogar = sum(DB090 * vhRentaa)/sum(DB090), personas_hogar = sum(HX040 * DB090)/sum(DB090), renta_persona = renta_hogar/personas_hogar, hogatpobreza = sum(DB090 * vhPobreza)/sum(DB090), .groups = "drop")
  personas <- datos$personas
  p <- personas %>% group_by(DB040) %>% summarise(Arope = sum(vrEU2020 * RB050)/sum(RB050), Igini = reldist::gini(HY020/HX240, weights = RB050), .groups = "drop")
  resultado <- left_join(h, p, by = "DB040") %>% left_join(regiones, by = "DB040") %>% mutate(Año = as.character(anio))
  stopifnot(!anyNA(resultado$comunidad), nrow(resultado) == nrow(regiones))
  resultado
}

datos2009 <- cargar_ecv(2009)
datos2020 <- cargar_ecv(2020)
ind2009 <- indicadores(datos2009, 2009)
ind2020 <- indicadores(datos2020, 2020)


RentaNetaMedia <- read_excel("data/RentaNetaMedia.xls",range = "A2:D14",col_names = c("Total","Ambos sexos","Hombres","Mujeres"))
datos1=tibble(Ano=rep(RentaNetaMedia$Total,3),Renta=c(RentaNetaMedia$`Ambos sexos`,RentaNetaMedia$Hombres,RentaNetaMedia$Mujeres),grupo=rep(c("Total","Hombre","Mujer"),each=13))
grafrenta=ggplot(data=datos1,aes(x=Ano,y=Renta,color=grupo,group=grupo))+
  geom_line()+
  geom_point(size=2)+
  labs(title="Renta Neta Media",
       subtitle="                  por Año",
       y="Renta",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  tema_tfg()+
  guides(color=guide_legend(title="Sexo"))
UmbralPobreza <- read_excel("data/UmbralPobreza.xls",range = "A2:B14",col_names = c("Ano","Umbral Pobreza"))
UmbralPobreza <- UmbralPobreza %>% arrange(Ano)
grafumbral=ggplot(data=UmbralPobreza,aes(x=Ano,y=`Umbral Pobreza`,group=1))+
  geom_line(alpha = 1,linetype = "solid", colour="#087E8B",linewidth = 1)+
  geom_point(size=2)+
  labs(title="Umbral Pobreza",
       subtitle="                  por Año",
       y="Umbral Pobreza",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  tema_tfg()

TasaPobrezaArope <- read_excel("data/TasaPobrezaArope.xlsx",range = "A8:B20",col_names = c("Ano","Tpobreza"),col_types = c("numeric","numeric"))
TasaPobrezaArope <- TasaPobrezaArope %>% arrange(Ano)


grafarope=ggplot(data=TasaPobrezaArope,aes(x=Ano,y=Tpobreza,group=1))+
  geom_line()+
  scale_y_continuous(limits = c(20,30))+
  scale_x_continuous(breaks=seq(2008,2020,1))+
  geom_smooth()+
  geom_point(size=4)+
  geom_label(aes(label=Tpobreza,y=Tpobreza+0.25),show.legend = FALSE)+
  labs(title="Tasa de riesgo de pobreza(Tasa AROPE)",
       subtitle="                  por Año",
       y="Tasa de riesgo de pobreza",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  tema_tfg()
s80s20 <- read_excel("data/s80s20.xls", range = "A8:B20",col_names = c("Ano","s80s20"))

grafs80s20=ggplot(data=s80s20,aes(x=Ano,y=`s80s20`,group=1))+
  geom_line()+
  scale_y_continuous(limits = c(3,10))+
  geom_point(size=2)+
  geom_label(aes(label=`s80s20`,y=`s80s20`+0.5),show.legend = FALSE)+
  labs(title="Ratio s80s20 por 100",
       subtitle="                  por Año",
       y="Ratio s80s20",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  tema_tfg()
IGini <- read_excel("data/Gini.xlsx", range = "A8:B20",col_names = c("Ano","Indice Gini"))
grafgini=ggplot(data=IGini,aes(x=Ano,y=`Indice Gini`,group=1))+
  geom_line()+
  scale_y_continuous(limits = c(30,40))+
  geom_point(size=2)+
  geom_label(aes(label=`Indice Gini`,y=`Indice Gini`+1),show.legend = FALSE)+
  labs(title="Índice de Gini",
       subtitle="                  por Año",
       y="Índice Gini",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  tema_tfg()


comparacion <- tibble(Año = 2009:2020,
  Renta = 100 * (diff(sort_by_renta <- RentaNetaMedia %>% arrange(Total) %>% pull(`Ambos sexos`)) / head(sort_by_renta, -1)),
  Umbral = 100 * (diff(sort_by_umbral <- UmbralPobreza %>% arrange(Ano) %>% pull(`Umbral Pobreza`)) / head(sort_by_umbral, -1)))
x <- comparacion %>% pivot_longer(-Año, names_to = "Indicador", values_to = "Variación") %>% pivot_wider(names_from = Año, values_from = Variación) %>% gt() %>% fmt_number(columns = -Indicador, decimals = 2) %>% tab_header(title = "Evolución de la renta y el umbral", subtitle = "Variación interanual (%)") %>% tab_source_note("Elaboración propia TFG")


mapa_espana <- jsonlite::fromJSON("data/shapefiles_espana_ccaa_2.geojson", simplifyVector = FALSE)
mapa_indicador <- function(datos, variable, titulo, cortes) {
  tabla <- datos %>% mutate(renta_persona = round(renta_persona, 2), Arope = round(100 * Arope, 2), Igini = round(100 * Igini, 2))
  highchart() %>% hc_add_series_map(mapa_espana, tabla, value = variable, joinBy = c("codaut_sin", "codigo"), name = titulo, dataLabels = list(enabled = TRUE, format = "{point.properties.name}")) %>% hc_colorAxis(dataClasses = color_classes(cortes)) %>% hc_legend(align = "right", layout = "vertical") %>% hc_title(text = titulo) %>% hc_caption(text = "Fuente: Elaboración propia TFG")
}
graf1 <- mapa_indicador(ind2009, "renta_persona", "Renta neta media por persona", seq(8000,15000,1000))
graf2 <- mapa_indicador(ind2009, "Arope", "Tasa AROPE", seq(10,40,length.out=8))
graf3 <- mapa_indicador(ind2009, "Igini", "Coeficiente de Gini", seq(26,42,length.out=8))
graf4 <- mapa_indicador(ind2020, "renta_persona", "Renta neta media por persona", seq(9000,16000,1000))
graf5 <- mapa_indicador(ind2020, "Arope", "Tasa AROPE", seq(10,45,length.out=8))
graf6 <- mapa_indicador(ind2020, "Igini", "Coeficiente de Gini", seq(26,44,length.out=8))


grafico_cluster <- function(datos) {
  matriz <- datos %>% select(Arope, renta_persona, Igini) %>% as.data.frame()
  rownames(matriz) <- datos$comunidad
  matriz <- scale(matriz)
  set.seed(123)
  modelo <- kmeans(matriz, centers = 4, nstart = 25)
  fviz_cluster(modelo, data = matriz, ellipse.type = "euclid", repel = TRUE, star.plot = TRUE) + tema_tfg()
}
cluster2009 <- grafico_cluster(ind2009)


personas_adultas <- read_csv("Microdatos/datos_2020/esudb20p.csv", show_col_types = FALSE) %>% transmute(RB030 = as.character(PB030))
p_adultas <- datos2020$personas %>% mutate(RB030 = as.character(RB030)) %>% semi_join(personas_adultas, by = "RB030")
ind_cluster2020 <- indicadores(list(hogares = datos2020$hogares, personas = p_adultas), 2020)
cluster2020 <- grafico_cluster(ind_cluster2020)


curva_lorenz <- function(datos, anio) {
  curva <- ineq::Lc(datos$personas$HY020 / datos$personas$HX240, n = datos$personas$RB050)
  tibble(L = curva$L, p = curva$p, Año = as.character(anio))
}
Lorentz <- bind_rows(curva_lorenz(datos2009, 2009), curva_lorenz(datos2020, 2020))
graflorenz <- ggplot(Lorentz, aes(p,L,colour=Año)) + geom_line(linewidth=1) + coord_fixed() + geom_abline(linetype="dashed", colour="grey60") + scale_colour_manual(values=paleta[1:2]) + labs(title="Curva de Lorenz", subtitle="2009 y 2020", x="Población acumulada", y="Renta acumulada", caption="Fuente: Elaboración propia TFG")
evolucion <- bind_rows(ind2009, ind2020) %>% pivot_longer(c(renta_hogar, personas_hogar, renta_persona, hogatpobreza, Arope, Igini), names_to="Indicador", values_to="valor")

newggslopegraph <- function(dataframe, indicador, titulo, formato) {
  datos <- filter(dataframe, Indicador == indicador) %>% mutate(tiempo = match(Año, sort(unique(Año))), etiqueta = formato(valor))
  ggplot(datos, aes(tiempo, valor, group=comunidad)) + geom_line(colour="#087E8B", alpha=.65, linewidth=.65) + geom_point(colour="#17365D", size=2) +
    geom_text_repel(data=filter(datos, tiempo==1), aes(label=paste(comunidad, etiqueta)), direction="y", nudge_x=-.12, hjust=1, size=3, max.overlaps=Inf, seed=123) +
    geom_text_repel(data=filter(datos, tiempo==2), aes(label=paste(etiqueta, comunidad)), direction="y", nudge_x=.12, hjust=0, size=3, max.overlaps=Inf, seed=123) +
    scale_x_continuous(breaks=1:2, labels=c("2009","2020"), limits=c(.15,2.85)) + labs(title=titulo, subtitle="Comparativa territorial", x=NULL,y=NULL,caption="Fuente: Elaboración propia TFG") + tema_tfg() + theme(panel.grid=element_blank(), axis.text.y=element_blank())
}
grafarope2 <- newggslopegraph(evolucion,"Arope","Evolución de la tasa AROPE", scales::label_percent(accuracy=.1))
grafgini2 <- newggslopegraph(evolucion,"Igini","Evolución del índice de Gini", scales::label_number(accuracy=.01))
grafrenta2 <- newggslopegraph(evolucion,"renta_persona","Evolución de la renta por persona", scales::label_number(accuracy=1, suffix=" €"))
