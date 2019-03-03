import(c("tcltk2", "pryr", "dplyr", "microbenchmark"))



get_new_data <- function() {
  return(c(time = Sys.time(), ram = as.numeric(system("../scripts/my_ram_usage.sh", intern = TRUE))/1024/1024, cpu = as.numeric(system("../scripts/my_cpu_usage.sh", intern = TRUE))/8) %>% rbind %>% data.frame)
}

my_data <<- get_new_data() 

monitor <- function() {
  my_data <<- rbind(get_new_data(), my_data)
}

start_monitor <- function() {
  my_data <<- get_new_data()
  tclTaskSchedule(1000, monitor(), id = "monitor", redo = TRUE)
}

end_monitor <- function() {
  tclTaskDelete("monitor")
}