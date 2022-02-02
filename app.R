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
library(leaflet.extras)
source("apariencia.R")

# debugonce(SISINTAR::interpolar_perfiles)
as_polygon <- function(feature) {

    geometry <- feature$geometry$coordinates[[1]]
    x <- unlist(lapply(geometry, function(x) x[1]))
    y <- unlist(lapply(geometry, function(x) x[2]))
    sf::st_polygon(list(cbind(x, y)))

}


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

    title = "SiSINTA",

    dashboardHeader(title = img(src="logo.png", width = 170)),
    dashboardSidebar(
        tags$link(rel = "stylesheet", type = "text/css", href = "detalles.css"),
        h3("Sistema de información de Suelos del INTA"),
        p("Para seleccionar perfiles a exportar se puede seleccionar un área del mapa, ingresar coordenadas y/o seleccionar un periodo. Luego seleccionar el formato del archivo a exportar"),
        sidebarMenu(
            selectInput("formato", h4("Formato"), choices = c("CSV", "EXCEL")),
            downloadButton("exportar", "Exportar")
        ),
        hr(),
        p(a(icon("envelope"), "Contacto", href = "maito:sisinta@inta.gob.ar¨")),
        p(a(icon("github"), "Código fuente", href = "https://github.com/INTA-Suelos/sisintar-web")),
        p(a(icon("globe"), "SiSINTA Web", href = "http://sisinta.inta.gob.ar/"))

    ),
    dashboardBody(
        ### changing theme
        sisinta_theme,

        fluidRow(
            box(title = h2("Selección de perfiles"), width = 12,
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
                                   choices = c("Todas las variables" = "", variables_horizonte),
                                   multiple = TRUE)
                ),
                column(6,
                       selectInput("variables_perfil", "Variables de perfil",
                                   choices = c("Todas las variables" = "", variables_perfil),
                                   multiple = TRUE)
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
        points <- sf::st_as_sf(perfiles_todo, coords = c("lon", "lat"))
        in_polygon <- sf::st_intersects(points, polygon(), sparse = FALSE)
        in_polygon <- rowMeans(in_polygon) != 0

        perfiles_todo %>%
            copy() %>%
            .[, selected := in_polygon &
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
            addDrawToolbar(targetGroup = "draw", singleFeature = FALSE,
                           rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(weight = 2, opacity = .8, fillOpacity = 0.1, color = colors[2], fillColor = colors[2])),
                           polylineOptions = FALSE,
                           polygonOptions =  drawPolygonOptions(shapeOptions = drawShapeOptions(weight = 2, opacity = .8, fillOpacity = 0.1, color = colors[2], fillColor = colors[2])),
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

    polygon <- reactive({
        if (is.null(input$mapa_draw_all_features)) {
            sf::st_as_sfc(list(
                sf::st_polygon(list(matrix(c(input$oeste, input$sur,
                                             input$oeste, input$norte,
                                             input$este, input$norte,
                                             input$este, input$sur,
                                             input$oeste, input$sur), ncol = 2, byrow = TRUE)))
            ))
        } else {
            sf::st_as_sfc(lapply(input$mapa_draw_all_features$features , as_polygon))
        }
    })

    observeEvent(input$mapa_draw_new_feature, {
        bbox <- sf::st_bbox(polygon())

        lons <- bbox[c(1, 3)]
        lats <- bbox[c(2, 4)]

        updateNumericInput(inputId = "oeste", value = min(lons))
        updateNumericInput(inputId = "este", value = max(lons))
        updateNumericInput(inputId = "sur", value = min(lats))
        updateNumericInput(inputId = "norte", value = max(lats))

        leafletProxy("mapa") %>%
            fitBounds(min(lons), min(lats), max(lons), max(lats),
                      options = list(animate = TRUE))
    })

    observeEvent(input$mapa_draw_deleted_features,  {
        updateNumericInput(inputId = "oeste", value = min(perfiles_todo$lon))
        updateNumericInput(inputId = "este", value = max(perfiles_todo$lon))
        updateNumericInput(inputId = "sur", value = min(perfiles_todo$lat))
        updateNumericInput(inputId = "norte", value = max(perfiles_todo$lat))

        leafletProxy("mapa") %>%
            fitBounds(min(perfiles_todo$lon), min(perfiles_todo$lat), max(perfiles_todo$lon),
                      max(perfiles_todo$lat),
                      options = list(animate = TRUE))
    })

    output$tabla <- DT::renderDataTable(DT::datatable(perfiles()))

    datos_todos <- data.table::fread("datos/datos_perfiles.csv")
    datos_perfiles_selecionados <- reactive({
        datos <- datos_todos %>%
            .[perfil_id %in% perfiles()[selected == TRUE]$perfil_id]

        variables <- c(input$variables_horizonte, input$variables_perfil)

        if (length(variables) != 0) {
            variables <- c("perfil_id", "profundidad_inferior", "profundidad_superior", variables)
            datos[, variables, with = FALSE]
        } else {
            datos
        }
    })

    # perfiles_estandarizados
    observeEvent(input$interpolacion,
                 if (input$interpolacion != "Ninguna") {
                     updateCheckboxInput(inputId = "fix_na", value = TRUE)
                 }
    )

    # perfiles_estandarizados
    observeEvent(input$fix_na,
                 if (input$fix_na == FALSE) {
                     updateSelectInput(inputId = "interpolacion", selected = "Ninguna")
                 }
    )


    perfiles_estandaraizados <- reactive({
        datos <- datos_perfiles_selecionados()
        if (input$fix_na == TRUE) {
            datos <- SISINTAR::imputar_profundidad_inferior(datos, profundidad = input$na_profundidad)
        }

        if (input$interpolacion != "Ninguna") {
            datos <- SISINTAR::separar_perfiles(datos)

            ids <- colnames(datos$horizontes) %in% c("perfil_id", "profundidad_inferior", "profundidad_superior")

            numericos <- vapply(datos$horizontes, is.numeric, logical(1))

            categoricas <- colnames(datos$horizontes)[!(numericos | ids)]
            if (length(categoricas)) {
                malas <- names(variables_horizonte[variables_horizonte %in% categoricas])

                showModal(modalDialog(
                    title = "Variables categóricas seleccionadas",
                    paste0("Sólo se pueden interpolar variables continuas.",
                           " Las siguientes variables serán ignoradas:\n",
                           paste0(malas, collapse = ", "),
                           ".")
                ))
            }

            horizontes_interpolar <- seq(0, input$max, by = input$res)

            metodo_interpolar <- switch(input$interpolacion,
                   "Promedio Ponderado" = SISINTAR::interpolar_promedio_ponderado(),
                   "Splines"  = SISINTAR::interpolar_spline()
                   )

            if (input$interpolacion == "Splines") {
                malos <- data.table::as.data.table(datos$horizontes)[, .SD[is.na(profundidad_inferior + profundidad_superior)], by = perfil_id]
                malos <- unique(malos$perfil_id)

                if (length(malos) > 0) {

                    showModal(modalDialog(
                        title = "Perfiles con profundidades faltantes",
                        paste0("La interpolación con splines no acepta valores faltantes en profundidad inferior o superior.",
                               "Los siguientes perfiles tienen valores faltanes que no pudieron imputarse:",
                               paste0(malos, collapse = ", "),
                               ".")
                    ))
                    datos$horizontes <- datos$horizontes[!(datos$horizontes$perfil_id %in% malos), ]
                    datos$sitios <- datos$sitios[!(datos$sitios$perfil_id %in% malos), ]
                }
            }

            datos$horizontes <- SISINTAR::interpolar_perfiles(datos$horizontes,
                                          variables = colnames(datos$horizontes)[numericos & !ids],
                                          horizontes = horizontes_interpolar,
                                          metodo = metodo_interpolar)
            datos <- merge(datos$sitios, datos$horizontes, by = "perfil_id")
        }

        datos
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
            datos <- perfiles_estandaraizados()
            if (is.null(datos)) {

            } else {
                if (input$formato == "CSV") {
                    data.table::fwrite(datos, file)
                } else if (input$formato == "EXCEL") {
                    SISINTAR::exportar_excel(datos, file)
                }
            }



        }
    )

}

# Run the application
shinyApp(ui = ui, server = server)
