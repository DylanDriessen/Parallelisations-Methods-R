#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
ram <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/ram_data.rds")
benchmark <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/microbenchmark_data.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
 sidebarPanel(
       helpText("Select wich axes"),
      selectInput("input_id", "Choose a variabel to display:", c("parallelForeach
", "clusterApply", "parLapplyParallel"))),
        
    
  
  
  mainPanel(
    plot_ly(
      x = row.names(ram),
      y = ram[,"Elapsed_Time_sec"],
      type = "bar")
    ),
 
 
    
  DT::dataTableOutput("table"),
  DT::dataTableOutput("tableBenchMark")
  )
  # sidebarPanel(
      #selectInput("input_id", "Choose a variabel to display:", 
                 # c(" ", "percent black", "percent Hispanic", "percent Asian"),
                  #"Percent White")
   # )
   
  
     


# Define server logic required to draw a histogram
server <- function(input, output) {
   output$table <- DT::renderDataTable(DT::datatable({
     data <- ram
   }))
   
   output$tableBenchMark <- DT::renderDataTable(DT::datatable({
     data <- benchmark
   }))
   
   setwd("../")
   source("lib/readFiles_peakRAM.r")
   call_functions_for_ram()
}

# Run the application 
shinyApp(ui = ui, server = server)

