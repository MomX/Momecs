# rm(list=ls())
# library(shiny)

# library(Momocs)
# data <- list(oc=olea %>% opoly(5, nb.pts=60),
#              cc=efourier(slice(charring, 1:12), 5))


module_ui_data <- function(id, data) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    # each column in fac becomes a selectInput
    selectInput(inputId = ns("data"),
                label="",
                choices=names(data),
                multiple=FALSE,
                selectize=FALSE))
}


# server
module_server_data <- function(input, output, session, data_list){
  data <- reactive(data_list[[input$data]])
 return(data)
}


#####
#
# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       module_ui_data("data", data_list=data_list)
#     ),
#     mainPanel(
#       verbatimTextOutput("data_print")
#     )
#   )
# )
#
# server <- function(input, output, session) {
#   data <- callModule(module_server_data, "data", data_list=data_list)
#   output$data_print <- renderPrint(data())
# }
#
# shinyApp(ui, server)
#
#
#
#
