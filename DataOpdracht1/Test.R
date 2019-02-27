library(shiny)
library(magrittr)
library(future)


source("lib/readFiles.r")
source("lib/readFiles_peakRAM.r")
source("lib/realtime_sysinfo.r")
import(c("readr","tibble","data.table", "peakRAM", "foreach", "doParallel", "parallel", "microbenchmark"))
plan(multiprocess)

#ram_vector[1: length(ram_vector)]
ui <- shinyServer(fluidPage(
  plotOutput("first_column")
  ))

server <- shinyServer(function(input, output, session){
     # Function to get new observations
  #futureCall(read_doparallel_foreach_peakRAM)
  
  onStop(function() tclTaskDelete())
  
  future(print("hallo"))
  future(read_doparallel_foreach_peakRAM())
  get_new_data <- function(){
    data <-c(time = as.numeric(Sys.time())  , ram = as.numeric(system("../scripts/my_ram_usage.sh", intern = TRUE))*10) %>% rbind %>% data.frame
    return(data)
  }
  
  # Initialize my_data
  my_data <<- get_new_data()
  
  # Function to update my_data
  update_data <- function(){
    my_data <<- rbind(get_new_data(), my_data)
  }
  
  # Plot the 30 most recent values
  output$first_column <- renderPlot({
    print("Render")
    invalidateLater(1000, session)
    update_data()
    print(my_data)
    plot(data = my_data, x = my_data$time, y = my_data$ram)
  })
  
})

shinyApp(ui=ui,server=server)




