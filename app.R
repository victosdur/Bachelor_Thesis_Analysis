library(shiny)
library(bslib)
library(highcharter)
library(gt)
source("calculos.R", local = TRUE)

indicadores_nacionales <- c("Renta neta media por persona", "Umbral de pobreza", "Tasa AROPE", "Ratio s80s20", "Índice Gini")
indicadores_regionales <- c("Renta neta media por persona", "Tasa AROPE", "Índice Gini")

tema <- bs_theme(version = 5, bootswatch = "flatly", primary = "#087E8B", bg = "#F3F6FA", fg = "#17365D")
texto <- function(archivo) card(card_body(includeMarkdown(file.path("www", archivo))))

ui <- page_navbar(
  title = "ECV | España",
  theme = tema,
  header = tags$head(tags$link(rel = "stylesheet", href = "app.css")),
  fillable = FALSE,
  nav_panel("Visión general",
            h2("Encuesta de Condiciones de Vida"),
            p("España · 2008–2020 · Elaboración propia a partir del INE"),
            layout_columns(
              value_box("Renta media · 2020", textOutput("k_renta")),
              value_box("AROPE · 2020", textOutput("k_arope")),
              value_box("Gini · 2020", textOutput("k_gini")),
              value_box("S80/S20 · 2020", textOutput("k_ratio")), col_widths = c(3,3,3,3)
            ),
            card(card_header("Evolución de la renta"), plotOutput("inicio", height = "420px"))
  ),
  nav_panel("Evolución nacional", shiny::fluidRow(
    shiny::column(3, card(card_header("Filtros"), card_body(selectInput("indic", "Indicador", indicadores_nacionales)))),
    shiny::column(9, card(card_header("Serie temporal"), plotOutput("nacional", height = "440px")),
                  card(card_header("Variación interanual"), div(style="overflow-x:auto", gt_output("tabla"))))
  )),
  nav_panel("Comunidades autónomas", shiny::fluidRow(
    shiny::column(3, card(card_header("Filtros"), card_body(selectInput("ano", "Año", c("2009", "2020"), selected="2020"), selectInput("indic2", "Indicador", indicadores_regionales)))),
    shiny::column(9, card(card_header("Mapa territorial"), highchartOutput("mapa", height="620px")))
  )),
  nav_panel("Análisis clúster", layout_columns(
    card(card_header("2009"), plotOutput("cl2009", height="530px")),
    card(card_header("2020"), plotOutput("cl2020", height="530px")), col_widths=c(6,6)
  )),
  nav_panel("Comparativa territorial", shiny::fluidRow(
    shiny::column(3, card(card_header("Filtros"), card_body(selectInput("indic3", "Indicador", c("Curva Lorenz", indicadores_regionales))))),
    shiny::column(9, card(card_header("2009 frente a 2020"), plotOutput("comparativa", height="760px")))
  )),
  nav_menu("Documentación",
           nav_panel("Introducción", texto("introduccion.md")),
           nav_panel("Definiciones", texto("definiciones.md")),
           nav_panel("Indicadores", texto("indicadores.md"))
  ),
  footer = div(class="pie", "Victor Toscano Durán · Trabajo de Fin de Grado · Fuente: INE")
)

server <- function(input, output, session) {
  ultimo <- function(datos, anio, variable) datos[[variable]][which.max(as.numeric(datos[[anio]]))]
  output$k_renta <- renderText(scales::label_number(big.mark=".", decimal.mark=",", suffix=" €", accuracy=1)(ultimo(RentaNetaMedia,"Total","Ambos sexos")))
  output$k_arope <- renderText(paste0(ultimo(TasaPobrezaArope,"Ano","Tpobreza"), " %"))
  output$k_gini <- renderText(ultimo(IGini,"Ano","Indice Gini"))
  output$k_ratio <- renderText(ultimo(s80s20,"Ano","s80s20"))
  output$inicio <- renderPlot(grafrenta)
  output$nacional <- renderPlot({req(input$indic); switch(input$indic, "Renta neta media por persona"=grafrenta, "Umbral de pobreza"=grafumbral, "Tasa AROPE"=grafarope, "Ratio s80s20"=grafs80s20, "Índice Gini"=grafgini)})
  output$tabla <- render_gt(x)
  output$mapa <- renderHighchart({req(input$ano,input$indic2); indice <- match(input$indic2, indicadores_regionales); list(graf1,graf2,graf3,graf4,graf5,graf6)[[indice + if(input$ano=="2020") 3 else 0]]})
  output$cl2009 <- renderPlot(cluster2009)
  output$cl2020 <- renderPlot(cluster2020)
  output$comparativa <- renderPlot({req(input$indic3); switch(input$indic3,"Curva Lorenz"=graflorenz,"Renta neta media por persona"=grafrenta2,"Tasa AROPE"=grafarope2,"Índice Gini"=grafgini2)})
}

shinyApp(ui, server)
