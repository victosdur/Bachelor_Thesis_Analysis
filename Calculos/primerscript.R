library(readxl) #Para poder leer archivos excel
library(tidyverse) 
library(knitr)#para hacer kable
library(ggplot2)#para graficos avanzados
library(ggthemes) 
library(tibble)#tibbles
#esquisse::esquisser() #para los graficos automaticos
library(magrittr)#para poder actualizar el mismo dataframe con tidyverse
install.packages("ggThemeAssist")
library(ggThemeAssist)
ggThemeAssist::ggThemeAssistGadget(graf1)


RentaNetaMedia <- read_excel("RentaNetaMedia.xls",range = "A2:D14",col_names = c("Total","Ambos sexos","Hombres","Mujeres"))






datos1=tibble(Ano=rep(RentaNetaMedia$Total,3),Renta=c(RentaNetaMedia$`Ambos sexos`,RentaNetaMedia$Hombres,RentaNetaMedia$Mujeres),grupo=rep(c("Total","Hombre","Mujer"),each=13))
datos1
ggplot(data=datos1,aes(x=Ano,y=Renta,color=grupo,group=grupo))+
  geom_line()+
  geom_point(size=2)+
  labs(title="Renta Neta Media",
       subtitle="                  por Año",
       y="Renta",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  theme_solarized()+
  guides(color=guide_legend(title="Sexo"))
library(tibble)
datos1=tibble(Ano=rep(RentaNetaMedia$Total,3),Renta=c(RentaNetaMedia$`Ambos sexos`,RentaNetaMedia$Hombres,RentaNetaMedia$Mujeres),grupo=rep(c("Total","Hombre","Mujer"),each=13))
datos1
diff(RentaNetaMedia$`Ambos sexos`)

UmbralPobreza <- read_excel("UmbralPobreza.xls",range = "A2:B14",col_names = c("Ano","Umbral Pobreza"))
UmbralPobreza %<>% arrange(Ano)
ggplot(data=UmbralPobreza,aes(x=Año,y=`UmbralPobreza`,group=1))+
  geom_line(alpha = 1,linetype = "solid", colour="blue",size = 1)+
  geom_point(size=2)+
  labs(title="Umbral Pobreza",
       subtitle="                  por Año",
       y="Umbral Pobreza",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  theme_solarized()




TasaPobrezaArope <- read_excel("TasaPobrezaArope.xlsx",range = "A8:B20",col_names = c("Ano","Tpobreza"),col_types = c("numeric","numeric"))
TasaPobrezaArope %<>% arrange(Ano)


ggplot(data=TasaPobrezaArope,aes(x=Ano,y=Tpobreza,group=1))+
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
  theme_solarized()
library(lubridate)
library(TSA)
library(tseries)
datosst=ts(TasaPobrezaArope$Tpobreza,start=c(2008,1),frequency = 1)
TasaPobrezaArope %<>% mutate(ano2=make_date(year=Ano))
TasaPobrezaArope %>% 
library(forecast)
auto.arima(datosst)
install.packages("modeltime")
install.packages("tidymodels")
library(tidymodels)
library(modeltime)
library(timetk)   
#Vamos a realizar prediccioens de la tasa de riesgo para los dso proximo año con tidymodels
#Visualizamos el dataset 
TasaPobrezaArope %>% plot_time_series(ano2,Tpobreza)
splits=TasaPobrezaArope %>% time_series_split(assess = "2 years",cumulative=TRUE)
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(ano2, Tpobreza, .interactive = FALSE)
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)
model_fit_arima=arima_reg() %>% set_engine("auto_arima") %>% fit(Tpobreza~ano2,training(splits))
model_fit_arima
model_fit_prophet <- prophet_reg() %>%
  set_engine("prophet") %>%
  fit(Tpobreza ~ ano2, training(splits))
model_fit_prophet
model_table <- modeltime_table(
  model_fit_arima, 
  model_fit_prophet
) 
model_table
calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))
calibration_table
calibration_table %>%
  modeltime_forecast(actual_data = TasaPobrezaArope) %>%
  plot_modeltime_forecast(.interactive = FALSE)
prediccion=calibration_table %>%
  modeltime_refit(TasaPobrezaArope) %>%
  modeltime_forecast(h = "2 years", actual_data = TasaPobrezaArope)
prediccion %>% ggplot(aes(x=.index,y=.value))+
  geom_point(data=filter(prediccion,year(.index)<2021))+
  geom_point(data=filter(prediccion,year(.index)>2020),aes(color=.model_desc),size=3)+
  geom_line(data=filter(prediccion,.model_desc != "PROPHET"),colour="lightblue")+
  geom_line(data=filter(prediccion,.model_desc != "UPDATE: ARIMA(1,0,0) WITH NON-ZERO MEAN"),colour="lightcoral")+geom_smooth()+
  geom_label(aes(label=round(.value,1),y=.value+0.25),show.legend = FALSE)+
  labs(title="Tasa de riesgo de pobreza(Tasa AROPE)",
       subtitle="por Año y predicción 2021-2022",
       y="Tasa de riesgo de pobreza",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  guides(color = guide_legend(title = "Modelo Prediccion"))+
  scale_color_hue(labels=c("Profeta","ARIMA(1,0,0)"))+
  scale_x_date(date_breaks="1 year",date_labels = "%Y")+
  theme_solarized()+
  theme(legend.background = element_rect(fill = "yellow",colour = 1))
  

library(TSA)
library(tseries)
library(forecast)
TasaPobrezaArope %<>% arrange(...1)
stemArope=ts(TasaPobrezaArope$Total,start = 2008,frequency=1)
stemArope
m=HoltWinters(stemArope,gamma=FALSE,beta = FALSE)
plot(m)
plot(fitted(m))
predhw=predict(m,n.ahead=2)
predhw

x=ses(stemArope,h=5)
plot(x)
x=ets(stemArope)
y=forecast(x,h=2)
y
plot(y)

tend=lm(TasaPobrezaArope$Total~as.numeric(TasaPobrezaArope$...1))
tend
coef=as.vector(tend$coefficients)
a=coef[1]
a
b=coef[2]
b
timepred=2021:2022
tsecular=a+b*timepred
tsecular

#Realizar un alisado ya que la recta de tendencia no muestra correctamente el patron que se obtiene o un AR(1)
datos2=tibble(Ano=c(TasaPobrezaArope$as.nume...1,"2021","2022"),Total=round(c(TasaPobrezaArope$Total,tsecular),1))
ggplot(data=datos2,aes(x=as.numeric(Ano),y=Total,group=1))+
  geom_line()+
  geom_abline(intercept=a,slope=b,color="blue")+
  scale_y_continuous(limits = c(20,30))+
  scale_x_continuous(breaks=2008:2022)+
  geom_label(aes(label=Total,y=Total+0.25),show.legend = FALSE)+
  labs(title="Prediccion 2021-2022 Tasa de Riesgo y Tnedencia",
       y="Tasa de riesgo de pobreza",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  theme_solarized()
compon=read_csv("Componente.txt",col_names = c("Ano","Feka","RiesgoPobreza(despues_trans_sociales)","Hogares_intensidad_trabajo_baja","Privacion_material_severa"))
datos1=tibble(Ano=rep(compon$Ano,3),Porce=c(compon$`RiesgoPobreza(despues_trans_sociales)`,compon$Hogares_intensidad_trabajo_baja,compon$Privacion_material_severa),grupo=rep(c("RP","IB","PM"),each=8))
datos1
ggplot(datos1, aes(x = Ano, y = Porce, fill = grupo)) + 
  geom_bar(stat="identity", position="dodge")+
  scale_x_continuous(breaks=2013:2020)+
  labs(title="Porcentajes Componentes Tasa de Riesgo",
       subtitle="                  por Año",
       y="Porcentajes",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  theme_solarized()+
  scale_fill_hue(labels = c("Riesgo Pobreza(despues de tranferencias sociales)", "Hogares con intensidad de trabajo baja", "Privacion material severa"))



library(readxl)
s80s20 <- read_excel("s80s20.xls", range = "A8:B20",col_names = c("Ano","s80s20"))

ggplot(data=s80s20,aes(x=Ano,y=`s80s20`,group=1))+
  geom_line()+
  scale_y_continuous(limits = c(3,10))+
  geom_point(size=2)+
  geom_label(aes(label=`s80s20`,y=`s80s20`+0.5),show.legend = FALSE)+
  labs(title="Ratio s80s20 por 100",
       subtitle="                  por Año",
       y="Ratio s80s20",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  theme_solarized()

par # solo sirve para graficos no considerados especiales como los de ggplot2
IGini <- read_excel("Gini.xlsx", range = "A8:B20",col_names = c("Ano","Indice Gini"))
IGini %<>% arrange(Ano)
CGini = IGini %>% mutate(Cgin=Gini/100) %>% select(...1,Cgin)
mat <- matrix(c(1, 2),nrow = 1, ncol = 2,byrow = TRUE)

ggplot(data=IGini,aes(x=Ano,y=`Indice Gini`,group=1))+
  geom_line()+
  scale_y_continuous(limits = c(30,40))+
  geom_point(size=2)+
  geom_label(aes(label=`Indice Gini`,y=`Indice Gini`+1),show.legend = FALSE)+
  labs(title="Índice de Gini",
       subtitle="                  por Año",
       y="Índice Gini",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  theme_solarized()

ggplot(data=CGini,aes(x=...1,y=Cgin,group=1))+
  geom_line()+
  scale_y_continuous(limits = c(0.30,0.40))+
  geom_point(size=2)+
  geom_label(aes(label=round(Cgin,2),y=Cgin+0.01),show.legend = FALSE)+
  labs(title="Coeficiente de Gini",
       subtitle="                  por Año",
       y="Coeficiente Gini",x="Año",
       caption="Fuente:Elaboración Propia TFG") +
  theme_solarized()

evolucion=tibble(Año=IGini$Ano,RentaNetaMedia=RentaNetaMedia$`Renta Neta media persona`/1000,TpobrezaArope=TasaPobrezaArope$Tpobreza,Umbral=UmbralPobreza$`Umbral Pobreza`/1000,Ratios80s20=s80s20$s80s20,IndiceGini=IGini$`Indice Gini`)
library(tidyverse)
evolucion %<>% pivot_longer(-Año,names_to = "Indicador",values_to = "valor") %>% filter(Año == 2009 | Año == 2020)
install.packages("CGPfunctions")
library(CGPfunctions)
library(plotly)
newggslopegraph(dataframe = evolucion,
                Times=Año,
                Measurement=valor,
                Grouping=Indicador,Title = "Evolución de los Indicadores de Pobreza y desigualdad en España",
                SubTitle = "2009-20020. (Umbral y Renta Neta en miles de euros)",
                Caption = "Elaboración propia TFG",
                DataLabelPadding = 0.2,
                DataLabelLineSize = 0.5,
                DataLabelFillColor = "yellow",
                ThemeChoice = "econ")
