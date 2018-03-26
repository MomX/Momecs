#' Morphometric (or multivariate) exploration controlled by shiny
#'
#' @param x a Coe or PCA object
#' @param launch.brower `logical` to feed [shiny::runApp], whether to
#' launch the app within your default browser (default: `FALSE`)
#' @examples
#' \dontrun{
#' Momecs()
#' hearts %>% efourier(3)  %>% Momecs()
#' }
#' @export
Momecs <- function(x, launch.brower=FALSE) {

  # if no data is passed, we build one
  if (missing(x)) {
    message("* no data passed, building a dataset")
    x <- suppressMessages(list(
      bot       = Momocs::bot %>% Momocs::efourier(5),
      # chaff     = chaff %>% fgProcrustes(),
      # charring  = charring %>% efourier(5),
      flower    = Momocs::flower,
      # hearts    = hearts %>% fgProcrustes() %>% efourier,
      # molars    = molars %>% fgProcrustes(),
      # mosquito  = mosquito %>% fgProcrustes(),
      # oak       = oak %>% fgProcrustes(),
      olea      = Momocs::olea %>% Momocs::opoly(5, nb.pts=60)#,
      # shapes    = shapes %>% efourier(5),
      # trilo     = trilo %>% efourier(5),
      # wings     = wings %>% fgProcrustes()
    ))
    class(x) <- c("toy", class(x))
    x <<- x
  } else {
    x <<- x
  }
  # Momocs:::.check(Momocs::is_Coe(x),
  #                 "data must be a Coe object")
  # names(x) <- substitute(x)
  # x <- list(your_Coe=x)
  # x
  #}

  # __ui__ ---------
  ui <- dashboardPage(
    dashboardHeader(
      # title = paste(img(src=system.file("extdata", "Momecs_icon.svg", package = "testdat")),
      #               "Momecs")
      title="Momecs"
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Data",   tabName = "data",   icon = icon("database")),
        menuItem("PCA",    tabName = "pca",    icon = icon("compress"))#,
        #menuItem("LDA",    tabName = "lda",    icon = icon("object-group")),
        #menuItem("CLUST",  tabName = "clust",  icon = icon("tree")),
        #menuItem("KMEANS", tabName = "kmeans", icon = icon("scissors")),
        #menuItem("MAP",    tabName = "map",    icon = icon("map-marker")),
        #menuItem("Source", href="https://github.com/MomX/Momecs", icon = icon("github"))
      )
    ),
    dashboardBody(
      tabItems(
        #data panel -------
        tabItem(tabName = "data",
                  uiOutput("filter_toy"),
                # data_full row
                fluidRow(
                  # column(3,
                  #        h3("Pick one dataset"),
                  #        # data picking
                  #        selectInput(inputId = "data_choice",
                  #                    label="",
                  #                    choices=names(x),
                  #                    multiple=FALSE,
                  #                    selectize=FALSE)),
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
                uiOutput("pca_ui"),
                uiOutput("pca_plot")
        )
      )
    ),
    skin="blue"
  )

  # __server__ ----------
  server <- function(input, output) {
    # __filtering__ --------

    output$filter_toy <- renderUI({
      if (any("toy" %in% class(x))){
        fluidRow(
        h3("Pick one dataset"),
        # data picking
        selectInput(inputId = "data_choice",
                    label="",
                    choices=names(x),
                    multiple=FALSE,
                    selectize=FALSE)
        )
      } else {
        fluidRow()
      }
    })

    # data picker --------
    data_full <- reactive(
      # case: no dataset provided
      if (any("toy" %in% class(x)))
        x[[input$data_choice]]
      else
        x
    )

    output$data_full <- renderPrint(data_full())

    # reactive filtering ----------
    output$filter_ui <- renderUI(
      lapply(colnames(data_full()$fac), function(i) {
        selectInput(inputId = paste0('fac_', i),
                    label = i,
                    choices =  levels(data_full()$fac[, i] %>% unlist),
                    selected = levels(data_full()$fac[, i] %>% unlist),
                    selectize = FALSE,
                    size = 12,
                    multiple = TRUE)})
    )

    data_filtered <- reactive({
      if (!Momocs::is_fac(data_full()))
        return(data_full())
      res <- lapply(colnames(data_full()$fac),
                    function(i) input[[paste0('fac_', i)]])
      names(res) <- colnames(data_full()$fac)
      # return(res)
      filter_x_with_list(data_full(), res)
    })

    output$data_filtered <- renderPrint(data_filtered())

    # __PCA__ --------
    # pca_calculation ------------
    data_pca <- reactive({
      Momocs::PCA(data_filtered())
    })

    # pca_ui -----------
    output$pca_ui <- renderUI(
      fluidRow(
        # appearance column
        column(4,
               h4("Appearance"),
               # axes choice
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   numericInput(inputId = "pca_axis1",
                                label = "PCx",
                                value = 1, 1, ncol(data_pca()$x), 1)
               ),

               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   numericInput(inputId = "pca_axis2",
                                label = "PCy",
                                value = 2, 1, ncol(data_pca()$x), 1)
               ),

               br(),
               # points
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   checkboxInput("pca_points",
                                 label = "Points",
                                 value = TRUE)
               ),

               # eigen
               div(style="display: inline-block;vertical-align:top; width: 120px;",
                   checkboxInput("pca_eigen",
                                 label="Scree plot",
                                 value=TRUE)
               ),

               br(),
               # plot dims
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   numericInput(inputId = "pca_plot_width",
                                label = "Plot width",
                                min=200, max=2400, value=800, step = 100)
               ),

               # plot zoom
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   numericInput(inputId="pca_zoom",
                                label="Zoom",
                                min=0.1, max=3, value=0.9, step=0.1)
               )
        ),

        # groups column
        column(4,
               h4("Groups"),
               # each column in fac becomes a selectInput
               # fac choice (1)
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   selectInput(inputId = "pca_fac1",
                               label="1st cov",
                               choices=colnames(data_pca()$fac),
                               selected=colnames(data_pca()$fac)[1],
                               multiple=FALSE,
                               selectize=FALSE)
               ),
               # fac choice (2)
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   selectInput(inputId = "pca_fac2",
                               label="2nd cov",
                               choices=c("NULL", colnames(data_pca()$fac)),
                               selected="NULL",
                               multiple=FALSE,
                               selectize=FALSE)
               ),

               br(),
               # convex hulls
               div(style="display: inline-block;vertical-align:top; width: 120px;",
                   checkboxInput("pca_chull",
                                 "Convex hulls",
                                 TRUE)
               ),
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   checkboxInput("pca_chullfilled",
                                 "Filled",
                                 FALSE)
               ),

               br(),
               # legend and labels
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   checkboxInput("pca_legend",
                                 label="Legend",
                                 value=TRUE)
               ),
               div(style="display: inline-block;vertical-align:top; width: 120px;",
                   checkboxInput("pca_labelgroups",
                                 label="Label groups",
                                 value=FALSE)
               )
        ),

        # morphospace column
        column(4,
               h4("Morphospace"),
               # morphospace
               selectInput("pca_morphospace_position",
                           label = NULL,
                           choices = list("none" = "none",
                                          "full" = "full",
                                          "range" = "range",
                                          "circle" = "circle",
                                          "xy" = "xy",
                                          "range_axes" = "range_axes",
                                          "full_axes" = "full_axes"),
                           selected = "range"),
               # Cosmetics
               h4("Cosmetics"),
               # title
               textInput("pca_title",
                         label=NULL,
                         value="",
                         placeholder="Type a title here")
        )
      )
    )

    # pca_plot -----
    output$pca_plot0 <- renderPlot({
      # masticate f for plot_PCA
      if (is.null(input$pca_fac2)) {
        f <- stats::as.formula(paste0("~", input$pca_fac1))
      } else {
        f <- stats::as.formula(paste("~", input$pca_fac1, "+", input$pca_fac2))
      }

      pca_morphospace <- ifelse(input$pca_morphospace_position=="none", FALSE, TRUE)
      Momocs::plot_PCA(data_pca(),
                       f = f,

                       axes=c(input$pca_axis1, input$pca_axis2),
                       zoom=input$pca_zoom,
                       points=input$pca_points,

                       labelgroups=input$pca_labelgroups,
                       legend=input$pca_legend,

                       # palette=palette_deliver(input$pca_palette),

                       morphospace = pca_morphospace,
                       morphospace_position =  input$pca_morphospace_position,

                       chull=input$pca_chull,
                       chullfilled=input$pca_chullfilled,

                       eigen=input$pca_eigen,
                       title=input$pca_title)
    },
    width=exprToFunction(input$pca_plot_width),
    height=exprToFunction(input$pca_plot_width))

    output$pca_plot <- renderUI({
      plotOutput("pca_plot0", height = input$pca_plot_width)
    })

  } # server end


  # runApp(list(ui = ui, server = server), launch.browser = launch.brower)
  shinyApp(ui, server)
}

# Momecs()
# hearts %>% efourier(3)  %>% Momecs()
