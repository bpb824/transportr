require(RCurl)
require(httr)

files = GET("http://lehd.ces.census.gov/data/lodes/")
c = content(files,"parsed")
filenames <- getURL("http://lehd.ces.census.gov/data/lodes/",
                    ftp.use.epsv = FALSE,dirlistonly = TRUE)

