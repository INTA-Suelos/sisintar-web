#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(magrittr)
library(shinydashboard)
library(dashboardthemes)
library(leaflet)
source("apariencia.R")

buscar_perfiles <- function(perfiles,
                            rango_lon = NULL,
                            rango_lat = NULL,
                            rango_fecha = NULL,
                            clase = NULL
) {

    if (!is.null(rango_lon)) {
        keep <- perfiles$lon >= min(rango_lon) & perfiles$lon <= max(rango_lon)
        perfiles <- perfiles[keep, ]
    }

    if (!is.null(rango_lat)) {
        keep <- perfiles$lat >= min(rango_lat) & perfiles$lat <= max(rango_lat)
        perfiles <- perfiles[keep, ]
    }

    if (!is.null(rango_fecha)) {
        rango_fecha <- as.Date(rango_fecha)
        keep <- perfiles$fecha >= min(rango_fecha) & perfiles$fecha <= max(rango_fecha)
        perfiles <- perfiles[keep, ]
    }

    if (!is.null(clase)) {
        keep <- lapply(clase, function(x) grepl(x, perfiles$clase, ignore.case = TRUE))
        keep <- Reduce("|", keep)

        perfiles <- perfiles[keep, ]
    }

    perfiles
}



nombres <- data.table::fread("datos/nombres.csv")

nombres_perfil <- nombres[startsWith(nombres$Nombre_csv, "perfil_"), ]
variables_perfil <- setNames(nombres_perfil$Nombre_csv, nombres_perfil$Nombre_sisinta)


nombres_horizonte <- nombres[!startsWith(nombres$Nombre_csv, "perfil_"), ]
variables_horizonte <- setNames(nombres_horizonte$Nombre_csv, nombres_horizonte$Nombre_sisinta)

ui <- dashboardPage(

    dashboardHeader(title = img(src="logo.png", width = 170)),
    dashboardSidebar(
        sidebarMenu(
            selectInput("formato", "Formato", choices = c("CSV", "EXCEL")),
            downloadButton("exportar", "Exportar")
        )
    ),
    dashboardBody(
        ### changing theme
        sisinta_theme,

        fluidRow(
            box(title = h2("Selección de sitios"), width = 12,
                fluidRow(
                    column(4,
                           fluidRow(
                               column(4, offset = 4, numericInput("norte", "Norte", -20))
                           ),
                           fluidRow(
                               column(4, numericInput("oeste", "Oeste", -75)),
                               column(4, offset = 4, numericInput("este", "Este", -53))
                           ),
                           fluidRow(
                               column(4, offset = 4, numericInput("sur", "Sur", -60))
                           )

                    ),
                    column(8, leafletOutput("mapa"))
                ),
                fluidRow(
                    column(4,
                           dateInput("fecha_inicio", "Fecha inicial", "1900-01-01"),
                           dateInput("fecha_final", "Fecha final"),
                    ),
                    column(8,  plotly::plotlyOutput("serie"))
                )
            )
        ),
        fluidRow(
            box(title = h2("Selección de variables"), width = 12,
                column(6,
                       selectInput("variables_horizonte", "Variables de horizonte",
                                   choices = variables_horizonte,
                                   multiple = TRUE),
                       checkboxInput("todas", "Todas las variables")
                ),
                column(6,
                       selectInput("variables_perfil", "Variables de perfil",
                                   choices = variables_perfil,
                                   multiple = TRUE),
                       checkboxInput("todas", "Todas las variables")
                )
            )
        ),
        fluidRow(
            box(title = h2("Procesamiento"), width = 12,
                column(width = 4,
                       checkboxInput("fix_na", "Imputar máxima profundidad inferior si es NA?"),
                       numericInput("na_profundidad", "Grosor del último horizonte (cm)", value = 5)
                ),
                column(width = 6,
                       selectInput("interpolacion", "Interpolación",
                                   choices = c("Ninguna", "Promedio Ponderado", "Splines")),
                       fluidRow(
                           column(6, numericInput("max", "Máxima profundidad (cm)", 100)),
                           column(6, numericInput("res", "Resolución vertical (cm)", 1))
                       )
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    perfiles_todo <- data.table::fread("datos/perfiles.csv")
    perfiles_todo[, selected := TRUE]

    updateNumericInput(inputId = "oeste", value = min(perfiles_todo$lon))
    updateNumericInput(inputId = "este", value = max(perfiles_todo$lon))
    updateNumericInput(inputId = "sur", value = min(perfiles_todo$lat))
    updateNumericInput(inputId = "norte", value = max(perfiles_todo$lat))

    colors <- c("#A66E2D", "#005579")


    perfiles <- reactive({
        # browser()
        perfiles_todo %>%
            copy() %>%
            .[, selected := lat %between%  c(input$sur, input$norte) &
                  lon %between%  c(input$oeste, input$este) &
                  fecha %between%  c(input$fecha_inicio, input$fecha_final)]
    })

    tiempo <- reactive(perfiles() %>%
                           .[selected == TRUE])


    output$serie <- plotly::renderPlotly(
        tiempo() %>%
            plotly::plot_ly(source = "serie_temporal") %>%
            # plotly::add_bars(y = ~ N, color = I(colors[2])) %>%
            plotly::add_histogram(x = ~ fecha, color = I(colors[2])) %>%
            plotly::layout(yaxis = list(fixedrange = TRUE)) %>%
            plotly::config(displaylogo = FALSE,
                           modeBarButtons = list(list("resetScale2d")))
    )

    observeEvent(plotly::event_data("plotly_relayout", "serie_temporal"), {

        zoom <- plotly::event_data("plotly_relayout", "serie_temporal")
        if (is.null(zoom) || names(zoom[1]) %in% c("xaxis.autorange", "width")) {
            updateDateInput(inputId = "fecha_inicio", value = "1900-01-01")
            updateDateInput(inputId = "fecha_final", value = Sys.Date())
        } else {
            updateDateInput(inputId = "fecha_inicio", value = zoom$`xaxis.range[0]`)
            updateDateInput(inputId =  "fecha_final", value = zoom$`xaxis.range[1]`)


        }

    })



    output$mapa <- renderLeaflet({

        leaflet() %>%
            addTiles() %>%    # Add default OpenStreetMap map tiles
            addMiniMap(zoomLevelOffset = -4,
                       zoomLevelFixed = 2,
                       zoomAnimation = FALSE, toggleDisplay = TRUE,
                       strings = list(hideText = "Minimizar mapita", showText = "Mostrar mapita")) %>%
            addDrawToolbar(targetGroup = "draw", singleFeature = TRUE,
                           rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(weight = 2, opacity = .8, fillOpacity = 0.1, color = colors[2], fillColor = colors[2])),
                           polylineOptions = FALSE,
                           polygonOptions = FALSE,
                           circleOptions = FALSE,
                           markerOptions = FALSE,
                           circleMarkerOptions = FALSE,
                           editOptions = editToolbarOptions(edit = FALSE, allowIntersection = FALSE)
                           ) %>%
            fitBounds(min(perfiles_todo$lon), min(perfiles_todo$lat), max(perfiles_todo$lon),
                      max(perfiles_todo$lat),
                      options = list(animate = TRUE)) %>%
            addLegend("topright", colors = colors, labels = c("No seleccionado", "Seleccionado"))


    })


    observe({
        leafletProxy("mapa") %>%
            clearMarkers() %>%
            addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~ paste0(numero, " ", clase),
                             label = ~ numero, color = ~ colors[selected+1], radius = 2,
                             data = perfiles())

    })



    observeEvent(input$mapa_draw_new_feature, {

        lons <- vapply(input$mapa_draw_new_feature$geometry$coordinates[[1]], "[[", numeric(1), 1)
        lats <- vapply(input$mapa_draw_new_feature$geometry$coordinates[[1]], "[[", numeric(1), 2)


        updateNumericInput(inputId = "oeste", value = min(lons))
        updateNumericInput(inputId = "este", value = max(lons))
        updateNumericInput(inputId = "sur", value = min(lats))
        updateNumericInput(inputId = "norte", value = max(lats))

        leafletProxy("mapa") %>%
            fitBounds(min(lons), min(lats), max(lons), max(lats),
                      options = list(animate = TRUE))
    })

    observeEvent(input$mapa_draw_deleted_features,  {
# browser()
        updateNumericInput(inputId = "oeste", value = min(perfiles_todo$lon))
        updateNumericInput(inputId = "este", value = max(perfiles_todo$lon))
        updateNumericInput(inputId = "sur", value = min(perfiles_todo$lat))
        updateNumericInput(inputId = "norte", value = max(perfiles_todo$lat))

        leafletProxy("mapa") %>%
            fitBounds(min(perfiles_todo$lon), min(perfiles_todo$lat), max(perfiles_todo$lon),
                      max(perfiles_todo$lat),
                      options = list(animate = TRUE))
        # input$mapa_draw_deletestop <- FALSE
    })


    output$tabla <- DT::renderDataTable(DT::datatable(perfiles()))


    datos_todos <- data.table::fread("datos/datos_perfiles.csv")
    perfiles_datos <- reactive({
        datos_todos %>%
            .[perfil_id %in% perfiles()[selected == TRUE]$perfil_id]
    })

    output$exportar <- downloadHandler(
        contentType = if (input$formato == "CSV") "text/csv" else "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        filename = function() {
            if (input$formato == "CSV") {
                "perfiles_sisinta.csv"
            } else {
                "perfiles_sisinta.xlsx"
            }
        },
        content = function(file) {
            if (input$formato == "CSV"){
                data.table::fwrite(perfiles_datos(), file)
            } else if (input$formato == "EXCEL") {
                browser()
                SISINTAR::exportar_excel(perfiles_datos(), file)
            }

        }
    )

}

# Run the application
shinyApp(ui = ui, server = server)
