rm(list=ls())

# dependencies
library(shiny)
library(shinydashboard)
library(Momocs)
devtools::load_all("~/Research/Momocs/")

# load domestic functions
source("local.R")

load("data.rda")
# should you use it with another dataset
# data <- a list with your object()

# UI ------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Momecs v 0.0.0", disable = FALSE),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data",   tabName = "data",   icon = icon("database")),
      menuItem("PCA",    tabName = "pca",    icon = icon("compress")),
      menuItem("LDA",    tabName = "lda",    icon = icon("object-group")),
      menuItem("CLUST",  tabName = "clust",  icon = icon("tree")),
      menuItem("KMEANS", tabName = "kmeans", icon = icon("scissors")),
      #menuItem("MAP",    tabName = "map",    icon = icon("map-marker")),
      menuItem("Source", href="https://github.com/vbonhomme/Momecs", icon = icon("github"))
    )
  ),
  dashboardBody(
    tabItems(
      #data panel -------
      tabItem(tabName = "data",
              # data_full row
              fluidRow(
                column(3,
                       h3("Pick one dataset"),
                       # data picking
                       selectInput(inputId = "data_choice",
                                   label="",
                                   choices=names(data),
                                   multiple=FALSE,
                                   selectize=FALSE)),
                column(9,
                       h3("Full data"),
                       verbatimTextOutput("data_full"))
              ),
              # data_filtered row
              fluidRow(
                column(3,
                       h3("Filtering"),
                       uiOutput("filter_ui")),
                column(9,
                       h3("Filtered data"),
                       verbatimTextOutput("data_filtered")
                )
              )
      ),

      #pca panel ---------
      tabItem(tabName = "pca",
              fluidRow(
                column(3,
                       h3("Appearance"),
                       uiOutput("pca_ui")
                ),
                column(9,
                       h3("Plot"),
                       plotOutput("pca_plot")
                )
              )
      ),
      #lda panel ---------
      tabItem(tabName = "lda",
              fluidRow(
                column(3,
                       h3("Options")
                ),
                column(9,
                       h3("Plot")
                )
              )
      ),
      #clust panel ---------
      tabItem(tabName = "clust",
              fluidRow(
                column(3,
                       h3("Options")
                ),
                column(9,
                       h3("Plot")
                )
              )
      ),
      #kmeans panel ---------
      tabItem(tabName = "kmeans",
              fluidRow(
                column(3,
                       h3("Options")
                ),
                column(9,
                       h3("Plot")
                )
              )
      )
    ),
    skin="green"
  )
)

# SERVER -----------
server <- shinyServer(function(input, output) {
  # data picker --------
  data_full <- reactive(data[[input$data_choice]])
  output$data_full <- renderPrint(data_full())

  # filtering ----------
  output$filter_ui <- renderUI(
    lapply(colnames(data_full()$fac), function(i) {
      selectInput(inputId = paste0('fac_', i),
                  label = i,
                  choices =  levels(data_full()$fac[, i]),
                  selected = levels(data_full()$fac[, i]),
                  selectize = FALSE,
                  size = nlevels(data_full()$fac[, i]),
                  multiple = TRUE)})
  )

  data_filtered <- reactive({
    if (!is.fac(data_full())) return(data_full())
    res <- lapply(colnames(data_full()$fac), function(i) input[[paste0('fac_', i)]])
    names(res) <- colnames(data_full()$fac)
    filter_x_with_list(data_full(), res)
  })

  output$data_filtered <- renderPrint(data_filtered())

  # pca ------------
  data_pca <- reactive({
    PCA(data_filtered())
  })


  # pca ui
  # each column in fac becomes a selectInput
  output$pca_ui <- renderUI(
    list(
      selectInput(inputId = "fac1",
                  label="1st factor",
                  choices=colnames(data_pca()$fac),
                  selected=colnames(data_pca()$fac)[1],
                  multiple=FALSE,
                  selectize=FALSE),

      selectInput(inputId = "fac2",
                  label="2nd factor:",
                  choices=c("NULL", colnames(data_pca()$fac)),
                  selected="NULL",
                  multiple=FALSE,
                  selectize=FALSE),

      numericInput(inputId = "pca_xax",
                   label = "Axis1",
                   value = 1, 1, ncol(data_pca()$x), 1),

      numericInput(inputId = "pca_yax",
                   label = "Axis2",
                   value = 2, 1, ncol(data_pca()$x), 1),
      # plot width
      sliderInput(inputId = "pca_plot_width",
                  label = "Plot width",
                  min=200, max=1600, value=800, step = 100),

      # plot zoom
      sliderInput(inputId="pca_zoom",
                  label="zoom (<=1 to display all points)",
                  min=0.1, max=5, value=1, step=0.1),

      # labels
      checkboxInput(inputId = "pca_labelsgroups",
                    label = "Label groups",
                    value = TRUE),

      selectInput(inputId = "pca_labelspoints",
                  label="Label points using:",
                  choices=c(FALSE, colnames(data_pca()$fac)),
                  selected=FALSE,
                  multiple=FALSE,
                  selectize=FALSE),
      # morphospace
      checkboxInput("pca_morphospace",
                    label = "Display morphospace",
                    value = TRUE),

      selectInput("pca_pos.shp",
                  label = "Shape position",
                  choices = list("full" = "full",
                                 "range" = "range",
                                 "circle" = "circle",
                                 "xy" = "xy",
                                 "range_axes" = "range_axes",
                                 "full_axes" = "full_axes"),
                  selected = "full"),
      # points
      checkboxInput("pca_points",
                    label = "Add points",
                    value = TRUE),

      # group dispersion
      checkboxInput("pca_ellipses",
                    label = "Add (0.5) confidence ellipses",
                    value = TRUE),

      checkboxInput("pca_ellipsesax",
                    label = "Add (0.5, 0.75, 0.9) conf. ellipses axes",
                    value = FALSE),

      checkboxInput("pca_chull",
                    label = "Add convex hulls",
                    value = FALSE),

      # palette picker
      selectInput("pca_palette",
                  label="Color palette",
                  choices=list("col_spring", "col_summer", "col_autumn", "col_qual", "col_solarized"),
                  selected="col_qual")
    ))


  # produces the PCA plot
  output$pca_plot <- renderPlot({
    if (is.null(input$fac2)) {
      fac <- input$fac1
    } else {
      fac <- as.formula(paste("~", input$fac1, "+", input$fac2))
    }

    plot(data_pca(),
         fac = fac,
         zoom=input$pca_zoom, xax=input$pca_xax, yax=input$pca_yax,
         palette=palette_deliver(input$pca_palette),
         morphospace = input$pca_morphospace, pos.shp=input$pca_pos.shp,
         points=input$pca_points, ellipses=input$pca_ellipses,
         ellipsesax=input$pca_ellipsesax,
         chull=input$pca_chull,
         labelsgroups=input$pca_labelsgroups,
         labelspoints=ifelse(input$pca_labelspoints==FALSE, FALSE, input$pca_labelspoints),
         cex.labelspoints = 1.2)
  },
  width=exprToFunction(input$pca_plot_width),
  height=exprToFunction(input$pca_plot_width))

  # PCA and its plot


  # data <- callModule(module_server_data, "data", data=data)

  # filtering
  # data_filtered <- callModule(module_server_filter, "filter", data=data)
  # output$data_filtered <- renderPrint(data_filtered())

  # pca
  # data_pca <- reactive(PCA(data_filtered()))
  # output$pca_plot <- callModule(module_server_pca, "pca", data=data_pca())
  # output$pca_plot <- renderPlot(plot(data_pca(), 1), width = 800, height=800)

})


# shinyApp(ui, server)
shinyApp(ui, server)
