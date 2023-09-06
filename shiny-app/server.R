library(shiny)
library(plotly)

source("shiny-app/ui.R")

source("src/loading/load_traj-arctic.R")
source("src/loading/load_traj-ant.R")
source("src/general-use-fcns/filter-traj.R")
source("src/plotting/plot-traj.R")
source("src/plotting/plot-pointcloud-density.R")

# Define the server function
server <- function(input, output,session) {
  
  #Poor (and quick) choice. control for the location needed and dynamic options!
  btraj <- c(btraj.GL,btraj.ANT)
  
  observe({
        # Replace the placeholder code with your logic to update the choices
    updateSelectInput(session, "reanalysis", choices = c("ERA5", "GDAS"))
    updateSelectInput(session, "samplingId", choices = 1:37)
    updateSelectInput(session, "samplingPoint", choices = 1:20)
    updateSelectInput(session, "mix", choices = 1:10/10)
    updateActionButton(session, "apply",label = "Apply!")
  })
  
  
  
  observeEvent(input$apply, {
    print("Controls have been modified")
    filtered_traj <- filter_traj(btraj, 
                                 reanalysis = input$reanalysis, 
                                 sampling_points = input$samplingPoint,
                                 sampling_ids = input$samplingId,
                                 mix_values = input$mix)
    
    
    if(input$showDensityPlot){
      plot <- plot_pointcloud_density(filtered_traj,input$radius)
    } 
    else {
      plot <- initialize_plot_GL(input$localization)
      
      traj_colors <- NULL
      if(input$doKmeans != "No"){
        source("src/general-use-fcns/get-ids-from-obj-list.R")
        source("src/m2-cluster/kmedoids.R")
        source("src/general-use-fcns/custom_palettes.R")
        
        groups <- kmedoids_traj(input$localization, distance = input$doKmeans,btraj.obj.list = filtered_traj,centers = input$nClusters)$Cluster
        print(groups)
        traj_colors <- set1_pal[groups]
        
      }
      
      #
      #
      
      if(input$showCurveLines)  plot <- plot %>% paint_lines_traj_list(filtered_traj,color = traj_colors)
        
      if(input$showCurvePoints)  plot <- plot %>% paint_points_traj_list(filtered_traj)
      
      if(input$showEndpoints) plot <- plot %>% paint_endpoints_traj_list(filtered_traj)
    }
      
      
    
    # Define the plotly output
    output$plot <- renderPlotly({
      # Add your plotly code here, using reactive inputs from `input`
      plot
    })
  })
  
  output$plot <- renderPlotly({
    # Add your plotly code here, using reactive inputs from `input`
    initialize_plot_GL()
  })
  

}

# Run the Shiny app
shinyApp(ui = ui, server = server)
