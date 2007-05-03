library(cacheSweave)

infile <- system.file("example", "simpleRR.Rnw", package = "cacheSweave")

setCacheDir("cache")

Sweave(infile, driver = cacheSweaveDriver)
