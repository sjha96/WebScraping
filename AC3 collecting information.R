setwd("F:/Shoude/AC Scraping")
wd <- getwd()
wd

.libPaths( c( .libPaths(), "H:/RLIB") )
install.packages("rvest", lib="H:/RLIB")
library(rvest)

# goodpid <- read.csv(file.path(wd, "ACgoodpid.csv"))
# productLinks <- paste("http://www.sears.com/kenmore-5-200-btu-room-air-conditioner/p-", goodpid$pid, sep="")

colnames(goodpid)[2] <- "Appliance_Type"
goodpid["Brand"] <- NA # column 3
goodpid["kWh/yr"] <- NA # column 4
goodpid["EER_Energy_Efficiency_Ratio"] <- NA # column 5
goodpid["SEER_Seasonal_Energy_Efficiency_Ratio"] <- NA # column 6
goodpid["Cooling_Capacity_BTUs"] <- NA # column 7
goodpid["Energy_Star"] <- NA # column 8
goodpid["Size"] <- NA # column 9
goodpid["Product_Width_(in.)"] <- NA # column 10
goodpid["Product_Depth_(in.)"] <- NA # column 11
goodpid["Product_Height_(in.)"] <- NA # column 12
goodpid["Product_Volume_(cu._in.)"] <- NA # column 13

levels(goodpid$Appliance_Type) <- c(levels(goodpid$Appliance_Type), "Window Air Conditioners")
levels(goodpid$Appliance_Type) <- c(levels(goodpid$Appliance_Type), "Wall Air Conditioners")
levels(goodpid$Appliance_Type) <- c(levels(goodpid$Appliance_Type), "Portable Air Conditioners")

# Right now, this contains PID, type, brand, EER, Cooling Capacity (BTUs), and Energy Star
goodpid <- read.csv(file.path(wd, "ACtable.csv"))
productLinks <- paste("http://www.sears.com/kenmore-5-200-btu-room-air-conditioner/p-", goodpid$pid, sep="")

# edit(goodpid)

n <- 0
x <- length(productLinks)

for (i in 1: x){
  if (n == 5) { # System sleeps after running through every 5 links
    Sys.sleep(5) 
    n = 0
  }
  n = n+1
  
  # Brand name
  returnval <- try(ACURL <- html(productLinks[i]), silent = TRUE) # not 22, 39, 46, 50 (Yikes page)
  iserrorpresent <- try(grep("Error in parse.response", returnval), silent = TRUE)
  # iserrorpresent # 1 means bad URL, an "error message" means good URL
  
  # This only runs if it is a good site
  if (iserrorpresent != 1){
    # print(paste("Site available ", productLinks[i], i, sep=" "))
    brand <- returnval %>% html_nodes(".productBrand-module") %>% html_text()

    brandname <- sub(".* All (.*)\\n.*","\\1",brand)
    brandname <- substr(brandname, 1, nchar(brandname)-2)
    if(length(brandname)!=0){
      goodpid[i, 3] <- paste(brandname)
      print(paste(i, goodpid[i, 3], sep= " "))
    } 
  } else {
    # print(paste("Site unavailable ", i))
  }
}

# Manually input missing brands
goodpid[40, 3] <- "No Brand"
goodpid[53, 3] <- "Keystone"
goodpid[57, 3] <- "No Brand"
goodpid[83, 3] <- "Keystone"
goodpid[85, 3] <- "No Brand"
goodpid[98, 3] <- "Keystone"
goodpid[103, 3] <- "Keystone"
goodpid[104, 3] <- "Keystone"
goodpid[129, 3] <- "SPT"
goodpid[177, 3] <- "No Brand"
goodpid[179, 3] <- "Keystone"
goodpid[202, 3] <- "Keystone"

foundinspecs <- FALSE
# This loop will get all the specs
for (i in 1: x){
  if (n == 5) { # System sleeps after running through every 5 links
    Sys.sleep(5) 
    n = 0
  }
  n = n+1
  
  returnval2 <- html(productLinks[i])
  
  specs <- returnval2 %>% html_nodes("#specifications") %>% html_text() # Specifications
  if(length(specs) != 0){ # Need to do this because some appliances don't have specs
    s <- strsplit(specs, '\\n')
    s2 <- gsub('\\s{2,}', '', s[[1]])
    indx <- grep(':', s2)
    specstable<- paste(s2[indx], s2[indx+1])
    
    # ENERGY STAR COMPLIANCE
    lES <- grep("ENERGY STAR Compliant:", specstable)
    if (length(lES) !=0) {
      goodpid[i,8] <- gsub(".*: ","", specstable[lES])
    } else{
      goodpid[i,8] <- "-"
    }
    
    # ENERGY EFFICIENCY RATING
    lEER <- grep("Energy Efficiency Rating:", specstable)
    if (length(lEER) !=0) {
      foundinspecs <- TRUE
      goodpid[i,5] <- gsub(".*: ","", specstable[lEER])
    } else{
      goodpid[i,5] <- "-"
    }
    
    if(foundinspecs == FALSE){
      descrip <- returnval2 %>% html_nodes("#description") %>% html_text() # Description
      lEER2 <- grep("EER", descrip, ignore.case=TRUE)
      if(length(lEER2) !=0){
        EERindex <- gregexpr(pattern = 'EER', descrip[[lEER2]], ignore.case = TRUE)
        Eindex <- EERindex[[1]][1]
        EERvalue <- as.numeric(substring(descrip[lEER2], Eindex+3, Eindex+9))
        
        goodpid[i,5] <- paste(EERvalue)
        print(paste(goodpid[i,5], i, sep = " "))
      } else{
        goodpid[i,5] <- "-"
        print(paste("- at ", i))
      }
    }
    
    # KWh/yr
    lES <- grep("Kilowatt Hrs. per Year:", specstable)
    if (length(lES) !=0) {
      goodpid[i,4] <- gsub(".*: ","", specstable[lES])
      print(paste(goodpid[i,7], i, sep = " "))
    } else{
      goodpid[i,4] <- "-"
      print(paste("KWh - at ", i))
    }
  } else {
    print(paste("No specstable ", i))
    goodpid[i,5] <- "-"
  }
  
  # BTU
  # If BTU appears in title of appliance
  
  title <- returnval2 %>% html_nodes(".product-content") %>% html_text() # Title
  t <- strsplit(title, '\\n')
  t2 <- gsub('\\t{2,}', '', t[[1]])
  lBTUs <- grep("BTU", t2, ignore.case=TRUE)
  
  if (length(lBTUs) !=0) {
    BTUindex <- gregexpr(pattern = 'BTU', t2[lBTUs], ignore.case = TRUE)
    Bindex <- BTUindex[[1]][1]
    BTUvalue <- substring(t2[lBTUs], Bindex-7, Bindex+2)
    BTUnum <- gsub(",","",BTUvalue)
    BTUval <- gsub("\\D","",BTUnum)
    
    goodpid[i,7] <- paste(BTUval)
     print(paste(goodpid[i,7], i, sep = " "))
  } else{
    goodpid[i,7] <- "-"
#     print(paste("- at ", i))
  }
}

# Manually input BTUs for unclear BTU pid's
goodpid[96,7] <- 12000 # Says 12K in title
goodpid[134,7] <- 14000 # Says 14K in title
goodpid[183,7] <- 14000 # Says 14K in title
goodpid[28,7] <- 10000 # From here down, BTU is found in description
goodpid[70,7] <- 7000
goodpid[170,7] <- 10000
goodpid[171,7] <- 12000
goodpid[184,7] <- 12000
goodpid[190,7] <- 9000
goodpid[203,7] <- 8000
goodpid[205,7] <- 12000

# SIZE
for (i in 1:x){
  if (n == 5) { # System sleeps after running through every 5 links
    Sys.sleep(5) 
    n <- 0
  }
  n <- n + 1
  
  returnval2 <- html(productLinks[i])
  
  if (goodpid[i,2] == "Air Conditioners"){
    # Need to specify which type of AC in Appliance_Type column
    
    level3 <- returnval2 %>% html_node(".level3 a") %>% html_text()
    
    goodpid[i,2] <- level3 # This is wall, window, or portable AC
    
    # print(paste("Level 3 at", i, "is", level3, sep = " "))
    specs <- returnval2 %>% html_nodes("#specifications") %>% html_text() # Specifications
    
    if (length(specs) != 0){ # As long as specs table exists
      s <- strsplit(specs, '\\n')
      s2 <- gsub('\\s{2,}', '', s[[1]])
      indx <- grep(':', s2)
      specstable<- paste(s2[indx], s2[indx+1])
      # print(paste(level3, "at", i))
      
      lACwidth <- grep("Cabinet Width", specstable)
      ACwidth <- gsub(".*: ","", specstable[lACwidth])
      
      lACdepth <- grep("Cabinet Depth", specstable)
      ACdepth <- gsub(".*: ","", specstable[lACdepth])
      
      lACheight <- grep("Cabinet Height", specstable)
      ACheight <- gsub(".*: ","", specstable[lACheight])
      
      goodpid[i,9] <- paste(ACwidth, "in. W x", ACdepth, "in. D x", ACheight, "in. H", sep= " ")
      # print(paste(ACwidth, "in. W x", ACdepth, "in. D x", ACheight, "in. H", sep= " "))
      
      if (length(ACwidth) != 0 && length(ACdepth) != 0 && length(ACheight) != 0){
        goodpid[i,10] <- ACwidth
        goodpid[i,11] <- ACdepth
        goodpid[i,12] <- ACheight
        goodpid[i,13] <- as.numeric(ACwidth) * as.numeric(ACdepth) * as.numeric(ACheight) # Total volume
      } else {
        # Need to add in manually
      }
    }
  } else if (goodpid[i,2] == "Water Softeners" || goodpid[i,2] == "Thermostats & Accessories"){
    specs <- returnval2 %>% html_nodes("#specifications") %>% html_text() # Specifications
    
    if (length(specs) != 0){ # As long as specs table exists
      s <- strsplit(specs, '\\n')
      s2 <- gsub('\\s{2,}', '', s[[1]])
      indx <- grep(':', s2)
      specstable<- paste(s2[indx], s2[indx+1])
      
      lACwidth <- grep("Width", specstable)
      ACwidth <- gsub(".*: ","", specstable[lACwidth])
      
      lACdepth <- grep("Depth", specstable)
      ACdepth <- gsub(".*: ","", specstable[lACdepth])
      
      lACheight <- grep("Height", specstable)
      ACheight <- gsub(".*: ","", specstable[lACheight])
      
      goodpid[i,9] <- paste(ACwidth, "in. W x", ACdepth, "in. D x", ACheight, "in. H", sep= " ")
      # print(paste(ACwidth, "in. W x", ACdepth, "in. D x", ACheight, "in. H", sep= " "))
      if (length(ACwidth) != 0 && length(ACdepth) != 0 && length(ACheight) != 0){
        goodpid[i,10] <- ACwidth
        goodpid[i,11] <- ACdepth
        goodpid[i,12] <- ACheight
        goodpid[i,13] <- as.numeric(ACwidth) * as.numeric(ACdepth) * as.numeric(ACheight) # Total volume
      } else {
        # Need to be done manually
      }
    } else if (goodpid[i,2] == "Accessories" || goodpid[i,2] == "Home Decor" || goodpid[i,2] == "Water Coolers & Filter Systems"){
      goodpid[i,9] <- "-"
#      goodpid[i,10] <- "-"
#      goodpid[i,11] <- "-"
#      goodpid[i,12] <- "-"
#      goodpid[i,13] <- "-"
    } else if (goodpid[i,2] == "Air Purifiers & Dehumidifiers"){
      # Manually from description
      goodpid[i,9] <- "FROM DESCRIPTION"
    }
  }
  if (is.na(goodpid[i,9])){
    goodpid[i,9] <- "-"
  }
  
  print(paste(i, "is", goodpid[i,2], ":", goodpid[i,9], "which is", goodpid[i,13], "cu. in.", sep= " "))
}

# Manually input sizes for products with dimensions in the description
goodpid[31,9] <- "15 in. W x 16 in. D x 30 in. H"
goodpid[31,10] <- "15"
goodpid[31,11] <- "16"
goodpid[31,12] <- "30"
goodpid[31,13] <- "7200"
goodpid[34,9] <- "-"
goodpid[35,9] <- "-"
goodpid[76,9] <- "-"
goodpid[87,9] <- "-"
goodpid[100,9] <- "16.5 in. W x 15.25 in. D x 30.5 in. H" 
goodpid[100,10] <- "16.5"
goodpid[100,11] <- "15.25"
goodpid[100,12] <- "30.5"
goodpid[100,13] <- "7674.5625"
goodpid[105,9] <- "15.75 in. W x 15.5 in. D x 30.5 in. H" 
goodpid[105,10] <- "15.75"
goodpid[105,11] <- "15.5"
goodpid[105,12] <- "30.5"
goodpid[105,13] <- "7445.8125"
goodpid[120,9] <- "17 in. W x 16 in. D x 30 in. H"
goodpid[120,10] <- "17"
goodpid[120,11] <- "16"
goodpid[120,12] <- "30"
goodpid[120,13] <- "8160"
goodpid[127,9] <- "-"
goodpid[133,9] <- "14 in. W x 20 in. D x 30.25 in. H"
goodpid[133,10] <- "14"
goodpid[133,11] <- "20"
goodpid[133,12] <- "30.25"
goodpid[133,13] <- "8470"
goodpid[140,9] <- "16 in. W x 16 in. D x 30 in. H"
goodpid[140,10] <- "16"
goodpid[140,11] <- "16"
goodpid[140,12] <- "30"
goodpid[140,13] <- "7680"
goodpid[142,9] <- "42 in. W x 21 in. D x 16 in. H"
goodpid[142,10] <- "42"
goodpid[142,11] <- "21"
goodpid[142,12] <- "16"
goodpid[142,13] <- "14112"
goodpid[192,9] <- "-"
goodpid[197,9] <- "16.5 in. W x 15.25 in. D x 30.5 in. H"
goodpid[197,10] <- "16.5"
goodpid[197,11] <- "15.25"
goodpid[197,12] <- "30.5"
goodpid[197,13] <- "7674.5625"
goodpid[198,9] <- "14 in. W x 20 in. D x 30.25 in. H"
goodpid[198,10] <- "14"
goodpid[198,11] <- "20"
goodpid[198,12] <- "30.25"
goodpid[198,13] <- "8470"
goodpid[201,9] <- "12.8 in. W x 18.1 in. D x 33.1 in. H"
goodpid[201,10] <- "12.8"
goodpid[201,11] <- "18.1"
goodpid[201,12] <- "33.1"
goodpid[201,13] <- "7668.608"
goodpid[203,9] <- "12 in. W x 15 in. D x 30 in. H"
goodpid[203,10] <- "12"
goodpid[203,11] <- "15"
goodpid[203,12] <- "30"
goodpid[203,13] <- "5400"
goodpid[205,9] <- "20 in. W x 31.5 in. D x 18 in. H"
goodpid[205,10] <- "20"
goodpid[205,11] <- "31.5"
goodpid[205,12] <- "18"
goodpid[205,13] <- "11340"
goodpid[3,9] <- "8.5 in. W x 6.25 in. D x 13 in. H"
goodpid[3,10] <- "8.5"
goodpid[3,11] <- "6.25"
goodpid[3,12] <- "13"
goodpid[3,13] <- "690.625"
goodpid[4,9] <- "13.4 in. W x 10 in. D x 20 in. H"
goodpid[4,10] <- "13.4"
goodpid[4,11] <- "10"
goodpid[4,12] <- "20"
goodpid[4,13] <- "2680"
goodpid[14,9] <- "13 in. W x 9.25 in. D x 20.75 in. H"
goodpid[14,10] <- "13"
goodpid[14,11] <- "9.25"
goodpid[14,12] <- "20.75"
goodpid[14,13] <- "2495.1875"
goodpid[25,9] <- "-"

count <- 0
for(i in 1:x){
  if (goodpid[i,9] == "-"){
    goodpid[i,10] <- "-"
    goodpid[i,11] <- "-"
    goodpid[i,12] <- "-"
    goodpid[i,13] <- "-"
  }
}
for(i in 1:x){
  if (is.na(goodpid[i,4]) == TRUE){
    goodpid[i,4] <- "-"
  
  }
}
for(i in 1:x){
  
    goodpid[i,6] <- "-"
    
  
}

test <- unique(goodpid[,2])

write.csv(goodpid, file = "Finaldataset.csv", row.names = FALSE)
