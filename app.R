rm(list=ls())
library(Momocs)
oc <- olea %>% opoly(5, nb.pts=60)
op <- PCA(oc)
library(shiny)
data <- oc

source("module_filtering.R")


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      module_ui_filter("filter")
    ),
    mainPanel(
      verbatimTextOutput('data_filtered')
    )
  )
)


server <- function(input, output, session) {
  data_filtered <- callModule(module_server_filter, "filter", data=oc)
  output$data_filtered <- renderPrint(data_filtered())
}

shinyApp(ui, server)

