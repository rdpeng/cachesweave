suppressMessages(library(cacheSweave))

infile <- system.file("example", "simple.Rnw", package = "cacheSweave")

setCacheDir("cache")

Sweave(infile, driver = cacheSweaveDriver)

ls()


## Check to see that objects were properly cached
## Database should contain 'airquality' and 'fit'

dname <- dir("cache", full.names = TRUE)
dir(dname, recursive = TRUE)
