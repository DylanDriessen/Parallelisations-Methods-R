#source(util/importPackage.r)
import("tcltk2")

#create empty vectors
ram_vector <<- NULL
cpu_vector <<- NULL

monitor <- function() {

  ram_vector <<- c(ram_vector, as.numeric(system("../scripts/my_ram_usage.sh", intern = TRUE)))
  cpu_vector <<- c(cpu_vector, as.numeric(system("../scripts/my_cpu_usage.sh", intern = TRUE)))
  #saveRDS(ram_vector, file="~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/ram_vector.rds")
}

start_monitor <- function() {
  ram_vector <<- NULL
  cpu_vector <<- NULL
  tclTaskSchedule(1000, monitor(), id = "monitor", redo = TRUE)
}

end_monitor <- function() {
  tclTaskDelete("monitor")
}