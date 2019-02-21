#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
ram <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/ram_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  DT::data 
   
  
  # sidebarPanel(
      #selectInput("input_id", "Choose a variabel to display:", 
                 # c(" ", "percent black", "percent Hispanic", "percent Asian"),
                  #"Percent White")
   # )
   )
  
     


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

