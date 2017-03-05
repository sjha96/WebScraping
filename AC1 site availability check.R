setwd("F:/Shoude/AC Scraping")
wd <- getwd()
wd

acpid <- read.csv(file.path(wd,"pid_ac_500_2007_2012_all.csv"))
# edit(acpid)

productLinks <- paste("http://www.sears.com/kenmore-5-200-btu-room-air-conditioner/p-", acpid$pid, sep="")
# edit(productLinks)

x<- length(productLinks)
x


.libPaths( c( .libPaths(), "H:/RLIB") )
install.packages("rvest", lib="H:/RLIB")
library(rvest)


n = 1 # Sleep counter
j= 1 # Number of iterations that has happened

goodsitelist <- rep("0", times = 1) # counting by goodcount
badsitelist <- rep("0", times = 1) # counting by badcount

goodcount <- 1
badcount <- 1

for (i in sample(1:x)){
  if (n == 5) { # System sleeps after running through every 5 links
    Sys.sleep(5) 
    n = 0
  }
  n = n+1

  # The try() function tests if there will be an error message.
  # If there is an error message (meaning bad site), returnval will be that error message.
  # If there is no error message (meaning good site), returnval will be the HTML code.
  returnval <- try(html(productLinks[i]), silent = TRUE)

  # We again use the try() function to see if we can grep() for "Error in parse.response".
  # If that does appear in returnval (bad site), iserrorpresent will be 1.
  # If that does not appear in returnval (good site), we actually get another error message.
  iserrorpresent <- try(grep("Error in parse.response", returnval), silent = TRUE)
  iserrorpresent # 1 means bad URL, an "error message" means good URL
  
  # This only runs if it is a good site
  if (iserrorpresent != 1){
    print(paste(j, "Site available ", productLinks[i], i, sep=" "))
    goodsitelist[goodcount] <- paste(acpid[i, 1])
    goodcount = goodcount + 1
    j= j+1
    
  } else {
    print(paste(j, "Site unavailable ", i))
    badsitelist[badcount] <- paste(acpid[i, 1])
    badcount = badcount + 1
    j = j + 1
  }
}

print(paste("Goodcount is ", goodcount))
print(paste("Badcount is ", badcount))

# To convert goodsitelist into a dataframe called ACdf with correct row and column names
ACdf1 <- as.data.frame(goodsitelist)
ACdf <- unname(ACdf1)
colnames(ACdf) <- c("pid")
rownames(ACdf) <- NULL

# Then export that data frame as an excel or csv file to be imported into next code

ACdf

write.csv(ACdf, file = "ACdf.csv", row.names = FALSE)