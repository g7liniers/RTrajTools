source("shiny-app/ui.R")
source("shiny-app/server.R")

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
