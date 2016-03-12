rm(list=ls())
# dependencies
library(shiny)
library(shinydashboard)
library(Momocs)

data_list <- list(oc=olea %>% opoly(5, nb.pts=60),
                  cc=efourier(slice(charring, 1:12), 5))
data <- data_list[[1]]

# very local solution far
# data MUST be a Coe not a PCA
#data <- olea %>% opoly(5, nb.pts=60)

# loads modules
source("module_data.R")
source("module_filter.R")
source("module_PCA.R")


# server -----------
ui <- dashboardPage(
  dashboardHeader(title = "Momecs v 0.0.0", disable = FALSE),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data",   tabName = "data",   icon = icon("database")),
      menuItem("Filter", tabName = "filter", icon = icon("filter")),
      menuItem("PCA",    tabName = "pca",    icon = icon("play-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              h3("Pick one dataset"),
              module_ui_data("data", data=data_list)),
      tabItem(tabName = "filter",
              fluidRow(
                column(4,
                       h3("Filtering"),
                       module_ui_filter("filter", data=data)),
                column(8,
                       h3("Filtered data"),
                       br(),
                       verbatimTextOutput("data_filtered"))
              )
      ),
      tabItem(tabName = "pca",
              fluidRow(
                # column(4,
                #        module_ui_pca("pca", data=reactive(data_pca))),
                # column(8,
                #        plotOutput("pca_plot"))

              )
      )
    ),
    skin="green"
  )
)

# server -----------
server <- shinyServer(function(input, output) {
  # data picker
  # data <- callModule(module_server_data, "data", data=data)

  # filtering
  data_filtered <- callModule(module_server_filter, "filter", data=data)
  output$data_filtered <- renderPrint(data_filtered())

  # pca
  # data_pca <- reactive(PCA(data_filtered()))
  # output$pca_plot <- callModule(module_server_pca, "pca", data=data_pca())
  # output$pca_plot <- renderPlot(plot(data_pca(), 1), width = 800, height=800)

})


# shinyApp(ui, server)
shinyApp(ui, server)
