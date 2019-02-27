ps -fauxw | awk '(index($0, "/usr/lib/rstudio-server/bin/rsession") !=0 ) { sum += $4} END { print sum }'

