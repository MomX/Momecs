
# dependencies
Momecs <- function(data){
  require(Momocs)
  require(shiny)
  require(shinydashboard)

  # if no data is passed, we build one
  if (missing(data)) {
    message("no data passed, building a dataset")
    data <- suppressMessages(list(
      bot       = bot %>% efourier(5),
      # chaff     = chaff %>% fgProcrustes(),
      charring  = charring %>% efourier(5),
      flower    = flower,
      # hearts    = hearts %>% fgProcrustes() %>% efourier,
      # molars    = molars %>% fgProcrustes(),
      mosquito  = mosquito %>% fgProcrustes(),
      # oak       = oak %>% fgProcrustes(),
      olea      = olea %>% opoly(5, nb.pts=60)#,
      # shapes    = shapes %>% efourier(5),
      # trilo     = trilo %>% efourier(5),
      # wings     = wings %>% fgProcrustes()
    ))
  } else {
    Momocs:::.check(is.Coe(data),
           "data must be a Coe object")
    data <- list(your_Coe=data)
  }

  # stupid function to cope with dplyr's non standard evaluation
  filter_x_with_list <- function(x, l){
    id <- vector("list", length(l))
    for (i in seq_along(l))
      id[[i]] <- x$fac[, names(l)[i]] %in% l[[i]]
    keep <- apply(as.data.frame(id), 1, all)
    subset(x, keep)
  }

  # cosmetics. because selectInput accepts characters only, not functions
  palette_deliver <- function(input){
    switch(input,
           "col_spring"=col_spring,
           "col_summer"=col_summer,
           "col_autumn"=col_autumn,
           "col_qual"=col_qual,
           "col_solarized"=col_solarized)
  }

  #load("data.rda")
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
                         h3("Options"),
                         uiOutput("lda_ui")
                  ),
                  column(9,
                         h3("Plot"),
                         plotOutput("lda_plot"),
                         h3("CV confusion matrix"),
                         plotOutput("lda_plot_CV")
                  )
                )
        ),
        #clust panel ---------
        tabItem(tabName = "clust",
                fluidRow(
                  column(3,
                         h3("Options"),
                         uiOutput("clust_ui")
                  ),
                  column(9,
                         h3("Plot"),
                         plotOutput("clust_plot")
                  )
                )
        ),
        #kmeans panel ---------
        tabItem(tabName = "kmeans",
                fluidRow(
                  column(3,
                         h3("Options"),
                         em("work in progress"),
                         uiOutput("kmeans_ui")
                  ),
                  column(9,
                         h3("Plot"),
                         plotOutput("kmeans_plot")
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
                    size = 12,
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

    # pca ui -----------
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

        # points
        h3("Points"),
        checkboxInput("pca_points",
                      label = "Add points",
                      value = TRUE),

        sliderInput(inputId = "pca_cex",
                    label = "Points cex",
                    min=0.1, max=5, value=1, step=0.1),

        checkboxInput(inputId = "pca_labelsgroups",
                      label = "Label groups",
                      value = TRUE),

        selectInput(inputId = "pca_labelspoints",
                    label="Label points using",
                    choices=c(FALSE, colnames(data_pca()$fac)),
                    selected=FALSE,
                    multiple=FALSE,
                    selectize=FALSE),

        checkboxInput(inputId = "pca_abbreviate.labelspoints",
                      label="Abbreviate points labels",
                      value=FALSE),

        # Groups
        h3("Groups"),
        checkboxInput(inputId = "pca_abbreviate.labelsgroups",
                      label="Abbreviate groups labels",
                      value=FALSE),

        checkboxInput("pca_ellipses",
                      label = "Add confidence ellipses",
                      value = TRUE),

        sliderInput("pca_conf.ellipses",
                    "Confidence level",
                    min=0, max=1, value=0.5, step=0.05),

        checkboxInput("pca_ellipsesax",
                      "Add (0.5, 0.75, 0.9) conf. ellipses axes",
                      value = FALSE),

        checkboxInput("pca_delaunay",
                      "Add Delaunay mesh",
                      FALSE),

        checkboxInput("pca_chull",
                      "Add convex hulls",
                      FALSE),

        checkboxInput("pca_chull.filled",
                      "Add filled convex hulls",
                      FALSE),

        sliderInput("pca_chull.filled.alpha",
                    "Filled convex hulls transparency",
                    0.5, 1, 0.92, 0.01),

        checkboxInput("pca_density",
                      "Add kde density",
                      FALSE),

        sliderInput("pca_lev.n.kde2d",
                    "Number of grid points",
                    10, 200, 10, 10),

        sliderInput("pca_lev.density",
                    "Number of density levels",
                    1, 100, 10, 1),

        checkboxInput("pca_contour",
                      "Add contours",
                      FALSE),

        sliderInput("pca_lev.contour",
                    "Number of contour levels",
                    1, 20, 5, 1),

        # morphospace
        h3("Morphospace"),
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

        #Cosmetics
        h3("Cosmetics"),

        textInput("pca_title",
                  "Plot title",
                  "PCA",
                  "100%"),

        selectInput("pca_palette",
                    label="Color palette",
                    choices=list("col_spring", "col_summer", "col_autumn", "col_qual", "col_solarized"),
                    selected="col_qual"),

        textInput("pca_bg",
                  "Background color",
                  "#FFFFFF",
                  "100%"),

        sliderInput(inputId = "pca_plot_width",
                    label = "Plot width",
                    min=200, max=1600, value=800, step = 100),

        # plot zoom
        sliderInput(inputId="pca_zoom",
                    label="zoom (<=1 to display all points)",
                    min=0.1, max=5, value=1, step=0.1),

        checkboxInput("pca_grid",
                      "Add grid",
                      TRUE),

        numericInput("pca_nb.grids",
                     "Number of grids",
                     3, 0, 10, 1),

        checkboxInput("pca_rug",
                      "Add rug",
                      TRUE),

        checkboxInput("pca_eigen",
                      "Add eigen screeplot",
                      TRUE),

        checkboxInput("pca_box",
                      "Add a box",
                      TRUE)
      ))


    # pca_plot -------------
    output$pca_plot <- renderPlot({
      if (is.null(input$fac2)) {
        fac <- input$fac1
      } else {
        fac <- as.formula(paste("~", input$fac1, "+", input$fac2))
      }

      plot(data_pca(),
           fac = fac,
           zoom=input$pca_zoom, xax=input$pca_xax, yax=input$pca_yax,
           points=input$pca_points, cex=input$pca_cex,
           palette=palette_deliver(input$pca_palette),

           morphospace = input$pca_morphospace,
           pos.shp=input$pca_pos.shp,

           labelspoints=ifelse(input$pca_labelspoints==FALSE, FALSE, input$pca_labelspoints),
           abbreviate.labelspoints=input$pca_abbreviate.labelspoints,
           labelsgroups=input$pca_labelsgroups,

           abbreviate.labelsgroups=input$pca_abbreviate.labelsgroups,
           ellipses=input$pca_ellipses,
           conf.ellipses=input$pca_conf.ellipses,

           ellipsesax=input$pca_ellipsesax,

           chull=input$pca_chull,
           chull.filled=input$pca_chull.filled,
           chull.filled.alpha=input$pca_chull.filled.alpha,

           delaunay=input$pca_delaunay,

           density=input$pca_density, lev.n.kde2d=input$pca_lev.n.kde2d,

           contour=input$pca_contour, lev.contour=input$pca_lev.contour,
           cex.labelspoints = 1.2,

           title=input$pca_title, grid=input$pca_grid, nb.grids=input$pca_nb.grids,
           rug=input$pca_rug, eigen=input$pca_eigen, box=input$pca_box, bg=input$pca_bg)
    },
    width=exprToFunction(input$pca_plot_width),
    height=exprToFunction(input$pca_plot_width))

    # lda --------------
    data_lda <- reactive({
      LDA(data_pca(), input$lda_fac, retain=as.numeric(input$lda_retain))
    })

    # lda_ui ------------
    output$lda_ui <- renderUI({
      list(
        selectInput(inputId = "lda_fac",
                    label="Factor",
                    choices=colnames(data_pca()$fac),
                    selected=NULL,
                    multiple=FALSE,
                    selectize=FALSE),

        textInput(inputId = "lda_retain",
                  "if <= 1, proportion of PCA variance; if >1 number of PCs",
                  value="0.99"),

        numericInput(inputId = "lda_xax",
                     label = "Axis1",
                     value = 1, 1, ncol(data_pca()$x), 1),

        numericInput(inputId = "lda_yax",
                     label = "Axis2",
                     value = 2, 1, ncol(data_pca()$x), 1),

        # points
        h3("Points"),
        checkboxInput("lda_points",
                      label = "Add points",
                      value = TRUE),

        sliderInput(inputId = "lda_cex",
                    label = "Points cex",
                    min=0.1, max=5, value=1, step=0.1),

        checkboxInput(inputId = "lda_labelsgroups",
                      label = "Label groups",
                      value = TRUE),

        selectInput(inputId = "lda_labelspoints",
                    label="Label points using",
                    choices=c(FALSE, colnames(data_pca()$fac)),
                    selected=FALSE,
                    multiple=FALSE,
                    selectize=FALSE),

        checkboxInput(inputId = "lda_abbreviate.labelspoints",
                      label="Abbreviate points labels",
                      value=FALSE),

        # Groups
        h3("Groups"),
        checkboxInput(inputId = "lda_abbreviate.labelsgroups",
                      label="Abbreviate groups labels",
                      value=FALSE),

        checkboxInput("lda_ellipses",
                      label = "Add confidence ellipses",
                      value = TRUE),

        sliderInput("lda_conf.ellipses",
                    "Confidence level",
                    min=0, max=1, value=0.5, step=0.05),

        checkboxInput("lda_ellipsesax",
                      "Add (0.5, 0.75, 0.9) conf. ellipses axes",
                      value = FALSE),

        checkboxInput("lda_delaunay",
                      "Add Delaunay mesh",
                      FALSE),

        checkboxInput("lda_chull",
                      "Add convex hulls",
                      FALSE),

        checkboxInput("lda_chull.filled",
                      "Add filled convex hulls",
                      FALSE),

        #Cosmetics
        h3("Cosmetics"),

        textInput("lda_title",
                  "Plot title",
                  "PCA",
                  "100%"),

        selectInput("lda_palette",
                    label="Color palette",
                    choices=list("col_spring", "col_summer", "col_autumn", "col_qual", "col_solarized"),
                    selected="col_qual"),

        textInput("lda_bg",
                  "Background color",
                  "#FFFFFF",
                  "100%"),

        sliderInput(inputId = "lda_plot_width",
                    label = "Plot width",
                    min=200, max=1600, value=600, step = 100),

        # plot zoom
        sliderInput(inputId="lda_zoom",
                    label="zoom (<=1 to display all points)",
                    min=0.1, max=5, value=1, step=0.1),

        checkboxInput("lda_grid",
                      "Add grid",
                      TRUE),

        numericInput("lda_nb.grids",
                     "Number of grids",
                     3, 0, 10, 1),

        checkboxInput("lda_rug",
                      "Add rug",
                      TRUE),

        checkboxInput("lda_eigen",
                      "Add eigen screeplot",
                      TRUE),

        checkboxInput("lda_box",
                      "Add a box",
                      TRUE)
      )
    })
    # lda_plot -------
    output$lda_plot <- renderPlot({
      plot(data_lda(),
           fac = input$lda_fac,
           zoom=input$lda_zoom, xax=input$lda_xax, yax=input$lda_yax,
           points=input$lda_points, cex=input$lda_cex,
           palette=palette_deliver(input$lda_palette),


           labelspoints=ifelse(input$lda_labelspoints==FALSE, FALSE, input$lda_labelspoints),
           abbreviate.labelspoints=input$lda_abbreviate.labelspoints,
           labelsgroups=input$lda_labelsgroups,

           abbreviate.labelsgroups=input$lda_abbreviate.labelsgroups,
           ellipses=input$lda_ellipses,
           conf.ellipses=input$lda_conf.ellipses,

           ellipsesax=input$lda_ellipsesax,

           chull=input$lda_chull,
           chull.filled=input$lda_chull.filled,

           delaunay=input$lda_delaunay,

           cex.labelspoints = 1.2,

           title=input$lda_title, grid=input$lda_grid, nb.grids=input$lda_nb.grids,
           rug=input$lda_rug, eigen=input$lda_eigen, box=input$lda_box, bg=input$lda_bg)
    },
    width=exprToFunction(input$lda_plot_width),
    height=exprToFunction(input$lda_plot_width))


    # lda_plot_CV ---------
    output$lda_plot_CV <- renderPlot({
      plot_CV(data_lda())
    },
    width=exprToFunction(input$lda_plot_width),
    height=exprToFunction(input$lda_plot_width))


    # clust_ui --------
    output$clust_ui <- renderUI({
      list(
        selectInput(inputId = "clust_fac",
                    label="Factor",
                    choices=colnames(data_pca()$fac),
                    selected=NULL,
                    multiple=FALSE,
                    selectize=FALSE),

        selectInput(inputId = "clust_type",
                    label="Plot type",
                    choices=c("cladogram", "phylogram", "radial", "unrooted", "fan"),
                    selected="fan",
                    multiple=FALSE,
                    selectize=FALSE),

        selectInput("clust_dist_method",
                    "dist_method",
                    choices=c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                    selected="euclidean",
                    multiple = FALSE,
                    selectize = FALSE),

        selectInput("clust_hclust_method",
                    "hclust_method",
                    choices=c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                    selected="complete",
                    multiple = FALSE,
                    selectize = FALSE),

        textInput(inputId = "clust_retain",
                  "if <= 1, proportion of PCA variance; if >1 number of PCs",
                  value="0.99"),

        selectInput(inputId = "clust_tip_labels",
                    label="tip labels",
                    choices=colnames(data_pca()$fac),
                    selected=colnames(data_pca()$fac),
                    multiple=FALSE,
                    selectize=FALSE),

        selectInput("clust_palette",
                    label="Color palette",
                    choices=list("col_spring", "col_summer", "col_autumn", "col_qual", "col_solarized"),
                    selected="col_qual")
      )
    })

    # clust_plot -------------
    output$clust_plot <- renderPlot({
      CLUST(data_pca(),
            fac=input$clust_fac,
            type=input$clust_type,
            dist_method=input$clust_dist_method,
            hclust_method=input$clust_hclust_method,
            retain=as.numeric(input$clust_retain),
            #tip_labels=input$clust_tip_labels,
            palette=palette_deliver(input$clust_palette))
    })

    # kmeans ------

    output$kmeans_ui <- renderUI({
      numericInput("kmeans_centers",
                   "Number of centers",
                   3, 1, 20, 1)
    })

    output$kmeans_plot <- renderPlot({
      KMEANS(data_pca(), centers=input$kmeans_centers)
    })

  })


  # shinyApp(ui, server)
  shinyApp(ui, server)

}
