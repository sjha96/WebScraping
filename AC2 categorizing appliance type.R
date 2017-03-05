setwd("F:/Shoude/AC Scraping")
wd <- getwd()
wd

.libPaths( c( .libPaths(), "H:/RLIB") )
install.packages("rvest", lib="H:/RLIB")
library(rvest)

goodpid <- read.csv(file.path(wd, "ACdf.csv"))

goodpid["Appliance Type"] <- NA
#edit(goodpid)

productLinks <- paste("http://www.sears.com/kenmore-5-200-btu-room-air-conditioner/p-", goodpid$pid, sep="")
# productLinks

x <- length(productLinks)
x

n = 1 # Sleep counter

for(i in 1:x){
  if (n == 5) { # System sleeps after running through every 5 links
    Sys.sleep(6) 
    n = 0
  }
  
  n = n+1
  
  ACURL <- html(productLinks[i])
  test <- try(ACURL %>% html_node("#breadcrumb-container .ng-binding") %>% html_text(), silent = TRUE)
  
  message <- sub("(.*): .*","\\1",test)
  message
  
  if(message != "Results For"){ 
    goodpid[i, 2] <- ACURL %>% html_node(".level2 a") %>% html_text()  
    print(paste(i, goodpid[i, 1], " is a ", goodpid[i, 2]))
  } else{
    print(paste("Bad i at", i, sep= " ")) 
  }
}

unique(goodpid[,2]) # Find how many unique types are in level2

# Need to get rid of rows that went to "Results For"
j <- 1 # Separate counter for new all-working dataframe

ACgoodpid <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(ACgoodpid) <- c("pid", "Appliance Type")

for (i in 1:x){
  if (!is.na(goodpid[i,2])){ 
    # Only transfer to ACgoodpid if it's a good site
    ACgoodpid[j,1] <- paste(goodpid[i,1])
    ACgoodpid[j,2] <- goodpid[i,2]
    
    j <- j + 1
  }
}
# Last update 7/13/2015, ACgoodpid has 214 rows
write.csv(ACgoodpid, file = "ACgoodpid.csv", row.names= FALSE)