ps -fuxw | awk '(index($0, "/usr/lib/rstudio-server/bin/rsession")!=0) { sum += $3} END { print sum }'

