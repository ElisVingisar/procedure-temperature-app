# --- app.R ---

library(shiny)

# Load UI and server logic from separate files
source("app/UI.R")
source("app/server.R")

# Run the Shiny application
shinyApp(ui = ui, server = server)
