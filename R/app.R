#' Morphometric (and multivariate) exploration controlled by shiny
#'
#' @param x a Coe or PCA object. If missing, [toy] datasets are used
#' @examples
#' \dontrun{
#' # on toy dataset(s)
#' Momecs()
#'
#' # passing a Coe
#' h <- hearts %>% efourier(3)
#' Momecs(h)
#' }
#' @export
Momecs <- function(x) {
  requireNamespace("shiny")
  requireNamespace("shinydashboard")
  requireNamespace("Momocs")
  # __before__ ----
  # toy dataset to start playing/developing
  if (missing(x))
    x <- Momecs::toy

  # # check validity of name when passed
  # .check(exists(deparse(substitute(x))),
  #        "x does not exist")
  # }

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
        # uiOutput("filter_toy"),
        uiOutput("filter_ui"),
        menuItem("PCA",    tabName = "pca",    icon = icon("compress")),
        menuItem("LDA",    tabName = "lda",    icon = icon("object-group")),
        menuItem("CLUST",  tabName = "clust",  icon = icon("tree"))#,
        #menuItem("KMEANS", tabName = "kmeans", icon = icon("scissors")),
        #menuItem("MAP",    tabName = "map",    icon = icon("map-marker")),
        #menuItem("Source", href="https://github.com/MomX/Momecs", icon = icon("github"))
      )
    ),
    dashboardBody(
      tabItems(
        #data panel -------
        tabItem(tabName = "data",
                uiOutput("filter_x"),
                # data_full row
                fluidRow(
                  column(2,
                         h4("Columns"),
                         uiOutput("filter_columns")),
                  column(4,
                         h4("Full"),
                         verbatimTextOutput("data_full")),
                  column(2,
                         h4("Include"),
                         textOutput("filter_prop")),
                  # uiOutput("filter_ui")),
                  column(4,
                         h4("Filtered"),
                         verbatimTextOutput("data_filtered"))
                )
        ),

        #pca panel ---------
        tabItem(tabName = "pca",
                uiOutput("pca_ui"),
                uiOutput("pca_plot")
        ),

        #lda panel ---------
        tabItem(tabName = "lda",
                uiOutput("lda_ui"),
                fluidRow(
                  column(6,
                         h4("Leave-one-out CV Accuracy"),
                         verbatimTextOutput("lda_accuracy")),
                  column(6,
                         h4("Class accuracy"),
                         verbatimTextOutput("lda_accuracy_class"))
                ),
                fluidRow(
                  column(6,uiOutput("lda_plot")),
                  column(6, uiOutput("lda_CV"))
                )
        ),
        #clust panel ---------
        tabItem(tabName = "clust",
                uiOutput("clust_ui"),
                uiOutput("clust_plot")

        )
      )
    ),
    skin="black"
  )

  # __server__ ----------
  server <- function(input, output) {
    # __filtering__ --------

    # filter_x ----
    output$filter_x <- renderUI({
      if (!is_Coe(x)){
        fluidRow(
          column(4,
                 h4("Pick one dataset"),
                 # data picking
                 selectInput(inputId = "data_choice",
                             label="",
                             choices=names(x),
                             multiple=FALSE,
                             selectize=FALSE)
          )
        )
      } else {
        fluidRow()
      }
    })

    # prepare data_full and finish to handle toy
    data_full <- reactive(
      # case: no dataset provided
      if (!is_Coe(x))
        x[[input$data_choice]]
      else
        # x[[]]
        x
    )

    output$data_full <- renderPrint(data_full())

    output$filter_prop <- renderText({
      fr <- data_full() %>% length()
      to <- data_filtered() %>% length()
      paste0(signif(100*to/fr, 3), "% [ ", to, "/", fr, " ]")
    })

    # filter_ui ----------
    output$filter_columns <- renderUI({
      selectInput(inputId = "filter_columns",
                  label="Columns to filter with",
                  choices = colnames(data_full()$fac),
                  selected = colnames(data_full()$fac),
                  selectize=FALSE,
                  size=ncol(data_full()$fac),
                  multiple=TRUE)
    })



    output$filter_ui <- renderUI(
      lapply(input[['filter_columns']], function(i) {
        levels_i <- levels(data_full()$fac[, i] %>% unlist)
        size     <- ifelse(nlevels(levels_i) <= 8, nlevels(levels_i), 8)
        selectInput(inputId = paste0('fac_', i),
                    label = i,
                    choices =  levels_i,
                    selected = levels_i,
                    selectize = FALSE,
                    size = size,
                    multiple = TRUE)
      })
    )

    data_filtered <- reactive({
      if (!Momocs::is_fac(data_full()))
        return(data_full())
      res <- lapply(colnames(data_full()$fac),
                    function(i) input[[paste0('fac_', i)]])
      names(res) <- colnames(data_full()$fac)
      # return(res)
      filter_with_list(data_full(), res)
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
                               choices=c("NULL", colnames(data_pca()$fac)),
                               selected="NULL",
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
      plotOutput("pca_plot0", height = input$pca_plot_width, width = input$pca_plot_width)
    })

    # __LDA__ --------

    # lda_calculation ------------
    data_lda <- reactive({
      # masticate f for plot_LDA
      # if (is.null(input$lda_fac2)) {
      # f <- stats::as.formula(paste0("~", input$lda_fac1))
      # }

      if (is.null(input$lda_fac2) | input$lda_fac2=="NULL") {
        f <- stats::as.formula(paste0("~", input$lda_fac1))
      } else {
        f <- stats::as.formula(paste("~", input$lda_fac1, "+", input$lda_fac2))
      }
      # list(input$lda_fac1, input$lda_fac2, f)
      data_filtered() %>% Momocs::LDA(f)
    })

    # output$lda_print <- renderPrint(data_lda())
    # output$lda_print2 <- renderPrint(data_lda() %>% plot_LDA %>% str)
    # lda_ui -----------
    nLDs <- reactive({
      if (is.null(data_lda()))
        return(2)
      else
        ncol(data_lda()$LDs)
    })
    output$lda_ui <- renderUI(
      fluidRow(
        # appearance column
        column(4,
               h4("Appearance"),
               # axes choice


               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   numericInput(inputId = "lda_axis1",
                                label = "LDx",
                                value = 1, 1, 8, 1)
               ),

               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   numericInput(inputId = "lda_axis2",
                                label = "LDy",
                                value = 2, 1, 8, 1)
               ),

               br(),
               # points
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   checkboxInput("lda_points",
                                 label = "Points",
                                 value = TRUE)
               ),

               # eigen
               div(style="display: inline-block;vertical-align:top; width: 120px;",
                   checkboxInput("lda_eigen",
                                 label="Scree plot",
                                 value=TRUE)
               ),

               br(),
               # plot dims
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   numericInput(inputId = "lda_plot_width",
                                label = "Plot width",
                                min=200, max=2400, value=400, step = 100)
               ),

               # plot zoom
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   numericInput(inputId="lda_zoom",
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
                   selectInput(inputId = "lda_fac1",
                               label="1st cov",
                               choices=colnames(data_filtered()$fac),
                               selected=NULL,
                               multiple=FALSE,
                               selectize=FALSE)
               ),
               # fac choice (2)
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   selectInput(inputId = "lda_fac2",
                               label="2nd cov",
                               choices=c("NULL", colnames(data_filtered()$fac)),
                               selected=NULL,
                               multiple=FALSE,
                               selectize=FALSE)
               ),

               br(),
               # convex hulls
               div(style="display: inline-block;vertical-align:top; width: 120px;",
                   checkboxInput("lda_chull",
                                 "Convex hulls",
                                 TRUE)
               ),
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   checkboxInput("lda_chullfilled",
                                 "Filled",
                                 FALSE)
               ),

               br(),
               # legend and labels
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   checkboxInput("lda_legend",
                                 label="Legend",
                                 value=TRUE)
               ),
               div(style="display: inline-block;vertical-align:top; width: 120px;",
                   checkboxInput("lda_labelgroups",
                                 label="Label groups",
                                 value=FALSE)
               )
        ),

        # morphospace column
        column(4,
               h4("Morphospace"),
               # morphospace
               selectInput("lda_morphospace_position",
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
               textInput("lda_title",
                         label=NULL,
                         value="",
                         placeholder="Type a title here")
        )
      )
    )

    # lda_plot -----
    output$lda_plot0 <- renderPlot({

      lda_morphospace <- ifelse(input$lda_morphospace_position=="none", FALSE, TRUE)
      Momocs::plot_LDA(data_lda(),

                       axes=c(input$lda_axis1, input$lda_axis2),
                       # axes=1:2,
                       zoom=input$lda_zoom,
                       points=input$lda_points,

                       labelgroups=input$lda_labelgroups,
                       legend=input$lda_legend,

                       # palette=palette_deliver(input$lda_palette),

                       morphospace = lda_morphospace,
                       morphospace_position =  input$lda_morphospace_position,

                       chull=input$lda_chull,
                       chullfilled=input$lda_chullfilled,

                       eigen=input$lda_eigen,
                       title=input$lda_title)
    },
    width=exprToFunction(input$lda_plot_width),
    height=exprToFunction(input$lda_plot_width))

    output$lda_plot <- renderUI({
      plotOutput("lda_plot0", height = input$lda_plot_width, width = input$lda_plot_width)
    })

    # lda_CV -----
    output$lda_CV0 <- renderPlot({
      Momocs::plot_CV(data_lda())
    },
    width=exprToFunction(input$lda_plot_width),
    height=exprToFunction(input$lda_plot_width))

    output$lda_CV <- renderUI({
      plotOutput("lda_CV0", height = input$lda_plot_width, width = input$lda_plot_width)
    })

    output$lda_accuracy <-renderPrint({
      data_lda()$CV.correct
    })

    output$lda_accuracy_class <-renderPrint({
      data_lda()$CV.ce
    })

    # __CLUST__ ------

    # clust_ui -----
    output$clust_ui <- renderUI(
      fluidRow(
        # appearance column
        column(4,
               h4("Calculation"),

               # dist method
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   selectInput("clust_dist_method",
                               "dist_method",
                               choices=c("euclidean", "maximum", "manhattan",
                                         "canberra", "binary", "minkowski"),
                               selected="euclidean",
                               multiple = FALSE,
                               selectize = FALSE)
               ),

               # hclust method
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   selectInput("clust_hclust_method",
                               "hclust_method",
                               choices=c("ward.D", "ward.D2", "single",
                                         "complete", "average", "mcquitty",
                                         "median", "centroid"),
                               selected="complete",
                               multiple = FALSE,
                               selectize = FALSE)
               )
        ),
        column(4,
               h4("Groups"),

               # fac choice
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   selectInput(inputId = "clust_fac",
                               label="Color with",
                               choices=c("NULL", colnames(data_filtered()$fac)),
                               # selected="NULL",
                               multiple=FALSE,
                               selectize=FALSE)
               ),

               # labels choice (2)
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   selectInput(inputId = "clust_labels",
                               label="Label with",
                               choices=c("NULL", colnames(data_filtered()$fac)),
                               # selected="NULL",
                               multiple=FALSE,
                               selectize=FALSE)
               ),

               # k
               numericInput("clust_k",
                            "Cut into",
                            value=1,
                            min=1,
                            max=20,
                            step=1)
        ),

        column(4,
               h4("Cosmetics"),
               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   numericInput(inputId = "clust_cex",
                                label="Labels size",
                                min=0.25, max=2, value=0.5, step=0.25)
               ),

               div(style="display: inline-block;vertical-align:top; width: 80px;",
                   numericInput(inputId = "clust_lwd",
                                label="Branches width",
                                min=0.25, max=2, value=0.5, step=0.25)
               ),

               numericInput(inputId = "clust_plot_width",
                            label = "Plot width",
                            min=200, max=2400, value=800, step = 100)
        )
      )
    )

    # clust_plot -----
    output$clust_plot0 <- renderPlot({
      Momocs::CLUST(data_filtered(),
                    fac           = input$clust_fac,
                    labels        = input$clust_labels,
                    dist_method   = input$clust_dist_method,
                    hclust_method = input$clust_hclust_method,
                    k             = input$clust_k,
                    cex           = input$clust_cex,
                    lwd           = input$clust_lwd)
    },

    width=exprToFunction(input$clust_plot_width),
    height=exprToFunction(input$clust_plot_width))

    output$clust_plot <- renderUI({
      plotOutput("clust_plot0",
                 width = input$clust_plot_width,
                 height = input$clust_plot_width)
    })

  } # server ends

  # run the app
  shinyApp(ui, server)
}
# Momecs()
# hearts %>% efourier(3)  %>% Momecs()
# olea %>% chop(~view) %>% lapply(opoly, 5, nb.pts=50) -> a
# Momecs(a)
