ps -fuxw | awk '(index($0, "/usr/lib/rstudio-server/bin/rsession")+index($0, "/usr/lib/R/bin/exec/R") !=0 ) { sum += $6} END { print sum }'

