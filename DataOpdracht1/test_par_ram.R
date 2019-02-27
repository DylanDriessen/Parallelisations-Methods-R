check_ram <- function() {
  current_ram <- pryr::mem_used()
  current_time <- format(Sys.time(), "%D %X")
  
  # if this date-time already exists, add time
  if (hasName(ram_data, current_time)) {
    ram_data[current_time] <- ram_data[current_time] + current_ram
  } else {
    ram_data[time] <- current_ram
  }
}

in_make_cluster <- function() {
  #initialize empty vector
  ram_data <<- NULL
  clusterEvalQ(cl,  {
    #start looping on this worker
    startLoop() check_ram()
  })
}

before_stop_closter <- function() {
  clusterEvalQ(cl, {
    stopLoop()
  })
}

