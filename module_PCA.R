# rm(list=ls())
# library(shiny)
#
# library(Momocs)
# oc <- olea %>% opoly(5, nb.pts=60)
# data_pca <- PCA(oc)
#

module_ui_pca <- function(id, data) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    # each column in fac becomes a selectInput
    selectInput(inputId = ns("fac1"),
                label="1st factor",
                choices=colnames(data$fac),
                selected=colnames(data$fac)[1],
                multiple=FALSE,
                selectize=FALSE),

    # fac2
    selectInput(inputId = ns("fac2"),
                label="2nd factor:",
                choices=c("NULL", colnames(data$fac)),
                selected="NULL",
                multiple=FALSE,
                selectize=FALSE),

    # axes selection
    numericInput(inputId = ns("xax"),
                 label = "Axis1",
                 value = 1, 1, ncol(data$x), 1),

    numericInput(inputId = ns("yax"),
                 label = "Axis2",
                 value = 2, 1, ncol(data$x), 1),
    # plot width
    sliderInput(inputId = ns("plot_width"),
                label = "Plot width",
                min=200, max=1600, value=800, step = 100),

    # plot zoom
    sliderInput(inputId=ns("zoom"),
                label="zoom (<=1 to display all points)",
                min=0.1, max=5, value=1, step=0.1),

    # labels
    checkboxInput(inputId = ns("labelsgroups"),
                  label = "Label groups",
                  value = TRUE),

    selectInput(inputId = ns("labelspoints"),
                label="Label points using:",
                choices=c(FALSE, colnames(data$fac)),
                          selected=FALSE,
                          multiple=FALSE,
                          selectize=FALSE),
    # morphospace
    checkboxInput(ns("morphospace"),
                  label = "Display morphospace",
                  value = TRUE),

    selectInput(ns("pos.shp"),
                label = "Shape position",
                choices = list("full" = "full",
                               "range" = "range",
                               "circle" = "circle",
                               "xy" = "xy",
                               "range_axes" = "range_axes",
                               "full_axes" = "full_axes"),
                selected = "full"),
    # points
    checkboxInput(ns("points"),
                  label = "Add points",
                  value = TRUE),

    # group dispersion
    checkboxInput(ns("ellipses"),
                  label = "Add (0.5) confidence ellipses",
                  value = TRUE),

    checkboxInput(ns("ellipsesax"),
                  label = "Add (0.5, 0.75, 0.9) conf. ellipses axes",
                  value = FALSE),

    checkboxInput(ns("chull"),
                  label = "Add convex hulls",
                  value = FALSE),

    # palette picker
    selectInput(ns("palette"),
                label="Color palette",
                choices=list("col_spring", "col_summer", "col_autumn", "col_qual", "col_solarized"),
                selected="col_qual")
  )
}


# server
module_server_pca <- function(input, output, session, data=reactive(data)){
  # cosmetics. because selectInput accepts characters only, not functions.
  palette_deliver <- function(input){
    switch(input,
           "col_spring"=col_spring,
           "col_summer"=col_summer,
           "col_autumn"=col_autumn,
           "col_qual"=col_qual,
           "col_solarized"=col_solarized)
  }

  renderPlot({
    if (is.null(input$fac2)) {
      fac <- input$fac1
    } else {
      fac <- as.formula(paste("~", input$fac1, "+", input$fac2))
    }
    plot(data,
         fac = fac,
         zoom=input$zoom, xax=input$xax, yax=input$yax,
         palette=palette_deliver(input$palette),
         morphospace = input$morphospace, pos.shp=input$pos.shp,
         points=input$points, ellipses=input$ellipses,
         ellipsesax=input$ellipsesax,
         chull=input$chull,
         labelsgroups=input$labelsgroups,
         labelspoints=ifelse(input$labelspoints==FALSE, FALSE, input$labelspoints),
         cex.labelspoints = 1.2)
  },
  width=exprToFunction(input$plot_width),
  height=exprToFunction(input$plot_width))
}


#####
#
# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       module_ui_pca("pca", data=data_pca)
#     ),
#     mainPanel(
#       plotOutput("pca")
#     )
#   )
# )
#
# server <- function(input, output, session) {
#   output$pca <- callModule(module_server_pca, "pca", data=data_pca)
# }
#
# shinyApp(ui, server)
#



