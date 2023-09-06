library(shiny)
library(plotly)

# Define the UI components
ui <- fluidPage(
  tags$style("
    html, body {
      margin: 0;
      padding: 0;
    }
    .navbar {
      margin-bottom: 20px;
    }

  "),
  navbarPage(
    "Rtraj GL",
    tabPanel(
      "Trajectory plot",
      sidebarLayout(
        sidebarPanel(
          "Controls",
          fluidRow(
            column(
               width = 12,
              selectInput("localization", "Localization", choices=c("ANT", "GL"), multiple = FALSE,selected = "GL"),
              selectInput("reanalysis", "Reanalysis", choices = NULL, multiple = TRUE),
              selectInput("samplingId", "Sampling Id", choices = NULL, multiple = TRUE),
              selectInput("samplingPoint", "Sampling point", choices = NULL, multiple = TRUE),
              selectInput("mix", "Limit layer proportion", choices = NULL, multiple = TRUE),
              checkboxInput("showCurvePoints", label = "Show points",value = FALSE),
              checkboxInput("showCurveLines", label = "Show curves", value = TRUE),
              checkboxInput("showEndpoints", label = "Show endpoints", value = FALSE),
              selectInput("doKmeans", label = "Perform k-means clustering", choices = c("No", "Frechet", "Hellinger", "Rdimf"), multiple = F),
              selectInput("nClusters", label="Number of clusters", choices = 2:9),
              checkboxInput("showDensityPlot", label = "Show density plot", value = FALSE),
              textInput("radius", label = "Density plot radius",value = 3,placeholder = "3"),
              actionButton("apply",label = "Apply!"),
            )
          ),
          width = 2
        ),
        mainPanel(
          plotlyOutput("plot",height = "90vh"),
          width = 10,
          height = 10
        )
        
      )
    ),
             
      
    )
    
    
  )