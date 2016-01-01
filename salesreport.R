report <- function (file, date = 1:12, product="all", store="all") {

  #read the file
  orders <- read.csv(file, header=TRUE, sep ="\t")

  #change factor dates into date class
  library(lubridate)
  orders$newdates <- ymd(as.character(orders$placed_date))

  #get the right columns we need
  cleandata <- orders[, c(1, 4, 5, 7)]

  #convert the dates to the right format
  for (d in date < 10) {
  gooddates <- lapply(date, function(n) sprintf("%02d", n))
  }

  #find the gross sales for the specified months
  monthsales <- subset(cleandata, format.Date(newdates, "%m") %in% gooddates)
  cat("\nGross sales: \n$", sum(monthsales$product_price), "\n")
  cat("\nPercentage of YTD: \n", (sum(monthsales$product_price)) / sum(cleandata$product_price) * 100, "%",  "\n")

  #validate product and order the rows by the specified order
  if (product == "skip") {
    cat("\n")
  } else if (product == "best") {
    #find the top twenty products
    productlist <- levels(factor(monthsales$product_name))

    totals <- lapply(productlist, function(prodname) {
      subset1 <- monthsales[monthsales$product_name == prodname, ]
      return(sum(subset1$product_price))
    })

    framelen <- length(totals)

    nodata <- data.frame(product=character(framelen), total=integer(framelen), percent=integer(framelen))
    nodata$total <- as.numeric(totals)
    nodata$product <- as.character(productlist)
    nodata$percent <- as.numeric(nodata$total / sum(monthsales$product_price) * 100)

    rankproducts <- nodata[order(-nodata$total, nodata$product), ]
    cat("\nTop twenty products sold for specified dates:\n")
    print(rankproducts[1:20, ])

    cat("\nGross sales for top twenty products: \n$", sum(rankproducts[1:20, ]$total), "\n")
    cat("\nPercentage of sales for specified dates: \n", (sum(rankproducts[1:20, ]$total)) / sum(rankproducts$total) * 100, "%\n")

  } else if (product == "worst"){
    #find the bottom twenty products
    productlist <- levels(factor(monthsales$product_name))

    totals <- lapply(productlist, function(prodname) {
      subset1 <- monthsales[monthsales$product_name == prodname, ]
      return(sum(subset1$product_price))
    })

    framelen <- length(totals)

    nodata <- data.frame(product=character(framelen), total=integer(framelen), percent=integer(framelen))
    nodata$total <- as.numeric(totals)
    nodata$product <- as.character(productlist)
    nodata$percent <- as.numeric(nodata$total / sum(monthsales$product_price) * 100)

    rankproducts <- nodata[order(nodata$total, nodata$product), ]
    cat("\nBottom twenty products sold for specified dates:\n")
    print(rankproducts[1:20, ])
    cat("\nGross sales for bottom twenty products: \n$", sum(rankproducts[1:20, ]$total), "\n")


  }  else if (product == "all"){
    #find the all the products sold
    productlist <- levels(factor(monthsales$product_name))

    totals <- lapply(productlist, function(prodname) {
      subset1 <- monthsales[monthsales$product_name == prodname, ]
      return(sum(subset1$product_price))
    })

    framelen <- length(totals)

    nodata <- data.frame(product=character(framelen), total=integer(framelen), percent=integer(framelen))
    nodata$total <- as.numeric(totals)
    nodata$product <- as.character(productlist)
    nodata$percent <- as.numeric(nodata$total / sum(monthsales$product_price) * 100)

    rankproducts <- nodata[order(-nodata$total, nodata$product), ]
    cat("\nAll products sold for specified dates:\n")
    print(rankproducts)

  } else if (product %in% monthsales$product_name) {
    #find the exact product
    as.list(product)
    monthproduct <- subset(monthsales, monthsales$product_name %in% product)
    cat("\nGross sales for this product: \n$", sum(monthproduct$product_price), "\n")
    cat("\nPercentage of YTD for this product: \n", (sum(monthproduct$product_price)) / sum(monthsales$product_price) * 100, "%\n")

  } else if (!(product %in% monthsales$product_name)) {
    #throw an error
    stop("invalid product")
  }

  #validate store and order the rows by the specified store
  if (store == "skip") {
    cat("\n")
  } else if (store == "best"){
    #find the top ten stores
    storelist <- levels(factor(monthsales$user_id))

    storetotals <- lapply(storelist, function(storename) {
      subset1 <- monthsales[monthsales$user_id == storename, ]
      return(sum(subset1$product_price))
    })

    framelen <- length(storetotals)

    nodata1 <- data.frame(store=character(framelen), total=integer(framelen), percent=integer(framelen))
    nodata1$total <- as.numeric(storetotals)
    nodata1$store <- as.character(storelist)
    nodata1$percent <- as.numeric(nodata1$total / sum(monthsales$product_price) * 100)

    #change ids to store names
    nodata1$store <- as.factor(nodata1$store)
    levels(nodata1$store)[levels(nodata1$store) == "4"] <- "Hy-Vee St Joseph"
    levels(nodata1$store)[levels(nodata1$store) == "26"] <- "Hy-Vee Belton"
    levels(nodata1$store)[levels(nodata1$store) == "6"] <- "Hy-Vee Blue Springs"
    levels(nodata1$store)[levels(nodata1$store) == "30"] <- "Bulk-it"
    levels(nodata1$store)[levels(nodata1$store) == "56"] <- "Daisy House Atlantic"
    levels(nodata1$store)[levels(nodata1$store) == "59"] <- "Hy-Vee Gladstone"
    levels(nodata1$store)[levels(nodata1$store) == "39"] <- "Grandma Hoerner's Foods"
    levels(nodata1$store)[levels(nodata1$store) == "52"] <- "Green Acres"
    levels(nodata1$store)[levels(nodata1$store) == "44"] <- "Hen House Market"
    levels(nodata1$store)[levels(nodata1$store) == "40"] <- "Hy-Vee Lenexa"
    levels(nodata1$store)[levels(nodata1$store) == "10"] <- "Hy-Vee Independence #1"
    levels(nodata1$store)[levels(nodata1$store) == "11"] <- "Hy-Vee Independence #2"
    levels(nodata1$store)[levels(nodata1$store) == "9"] <- "Hy-Vee Kansas City #1"
    levels(nodata1$store)[levels(nodata1$store) == "7"] <- "Hy-Vee Kansas City #2"
    levels(nodata1$store)[levels(nodata1$store) == "8"] <- "Hy-Vee Kansas City #3"
    levels(nodata1$store)[levels(nodata1$store) == "61"] <- "Landmark Inn"
    levels(nodata1$store)[levels(nodata1$store) == "14"] <- "Hy-Vee Lawrence #1"
    levels(nodata1$store)[levels(nodata1$store) == "15"] <- "Hy-Vee Lawrence #2"
    levels(nodata1$store)[levels(nodata1$store) == "12"] <- "Hy-Vee Lee's Summit #1"
    levels(nodata1$store)[levels(nodata1$store) == "13"] <- "Hy-Vee Lee's Summit #2"
    levels(nodata1$store)[levels(nodata1$store) == "16"] <- "Hy-Vee Liberty"
    levels(nodata1$store)[levels(nodata1$store) == "17"] <- "Hy-Vee Manhattan"
    levels(nodata1$store)[levels(nodata1$store) == "37"] <- "Hy-Vee Maryville"
    levels(nodata1$store)[levels(nodata1$store) == "43"] <- "Price Chopper Mckeever's"
    levels(nodata1$store)[levels(nodata1$store) == "18"] <- "Hy-Vee Mission"
    levels(nodata1$store)[levels(nodata1$store) == "58"] <- "Newport Grill KC"
    levels(nodata1$store)[levels(nodata1$store) == "23"] <- "Hy-Vee Olathe #1"
    levels(nodata1$store)[levels(nodata1$store) == "24"] <- "Hy-Vee Olathe #2"
    levels(nodata1$store)[levels(nodata1$store) == "46"] <- "Old Shawnee Pizza"
    levels(nodata1$store)[levels(nodata1$store) == "41"] <- "One More Cup"
    levels(nodata1$store)[levels(nodata1$store) == "19"] <- "Hy-Vee Overland Park #1"
    levels(nodata1$store)[levels(nodata1$store) == "20"] <- "Hy-Vee Overland Park #2"
    levels(nodata1$store)[levels(nodata1$store) == "38"] <- "Parkville Coffeehouse"
    levels(nodata1$store)[levels(nodata1$store) == "50"] <- "Pizza Shoppe Belton"
    levels(nodata1$store)[levels(nodata1$store) == "34"] <- "Pizza Shoppe Kearney"
    levels(nodata1$store)[levels(nodata1$store) == "36"] <- "Pizza Shoppe Platte City"
    levels(nodata1$store)[levels(nodata1$store) == "33"] <- "Pizza Shoppe Savannah"
    levels(nodata1$store)[levels(nodata1$store) == "35"] <- "Pizza Shoppe Smithville"
    levels(nodata1$store)[levels(nodata1$store) == "32"] <- "Pizza Shoppe St Joseph"
    levels(nodata1$store)[levels(nodata1$store) == "47"] <- "Poppy's Bakery"
    levels(nodata1$store)[levels(nodata1$store) == "21"] <- "Hy-Vee Prairie Village"
    levels(nodata1$store)[levels(nodata1$store) == "31"] <- "Price Chopper Liberty"
    levels(nodata1$store)[levels(nodata1$store) == "29"] <- "Price Chopper Platte City"
    levels(nodata1$store)[levels(nodata1$store) == "57"] <- "Price Chopper Queen's Stanley KS"
    levels(nodata1$store)[levels(nodata1$store) == "28"] <- "Rays Green Hills"
    levels(nodata1$store)[levels(nodata1$store) == "22"] <- "Hy-Vee Raytown"
    levels(nodata1$store)[levels(nodata1$store) == "48"] <- "Hy-Vee Shawnee"
    levels(nodata1$store)[levels(nodata1$store) == "49"] <- "Shawnee Mission Medical Center"
    levels(nodata1$store)[levels(nodata1$store) == "3"] <- "testpass"
    levels(nodata1$store)[levels(nodata1$store) == "45"] <- "The Merc"
    levels(nodata1$store)[levels(nodata1$store) == "25"] <- "Hy-Vee Topeka"
    levels(nodata1$store)[levels(nodata1$store) == "54"] <- "Tornado Alley Trading Co"
    levels(nodata1$store)[levels(nodata1$store) == "42"] <- "Yia Yia's EuroBistro"

    rankstores <- nodata1[order(-nodata1$total, nodata1$store), ]
    cat("\nTop ten stores for specified dates:\n")
    print(rankstores[1:10, ])

    cat("\nGross sales for top ten stores: \n$", sum(rankstores[1:10, ]$total), "\n")
    cat("\nPercentage of sales for specified dates: \n", (sum(rankstores[1:10, ]$total) / sum(rankstores$total)) * 100, "%", "\n")

  } else if (store == "worst"){
    #find the bottom ten stores
    storelist1 <- levels(factor(monthsales$user_id))

    storetotals1 <- lapply(storelist1, function(storename) {
      subset2 <- monthsales[monthsales$user_id == storename, ]
      return(sum(subset2$product_price))
    })

    framelen <- length(storetotals1)

    nodata2 <- data.frame(store=character(framelen), total=integer(framelen), percent=integer(framelen))
    nodata2$total <- as.numeric(storetotals1)
    nodata2$store <- as.character(storelist1)
    nodata2$percent <- as.numeric(nodata2$total / sum(monthsales$product_price) * 100)

    #change ids to store names
    nodata2$store <- as.factor(nodata2$store)
    levels(nodata2$store)[levels(nodata2$store) == "4"] <- "Hy-Vee St Joseph"
    levels(nodata2$store)[levels(nodata2$store) == "26"] <- "Hy-Vee Belton"
    levels(nodata2$store)[levels(nodata2$store) == "6"] <- "Hy-Vee Blue Springs"
    levels(nodata2$store)[levels(nodata2$store) == "30"] <- "Bulk-it"
    levels(nodata2$store)[levels(nodata2$store) == "56"] <- "Daisy House Atlantic"
    levels(nodata2$store)[levels(nodata2$store) == "59"] <- "Hy-Vee Gladstone"
    levels(nodata2$store)[levels(nodata2$store) == "39"] <- "Grandma Hoerner's Foods"
    levels(nodata2$store)[levels(nodata2$store) == "52"] <- "Green Acres"
    levels(nodata2$store)[levels(nodata2$store) == "44"] <- "Hen House Market"
    levels(nodata2$store)[levels(nodata2$store) == "40"] <- "Hy-Vee Lenexa"
    levels(nodata2$store)[levels(nodata2$store) == "10"] <- "Hy-Vee Independence #1"
    levels(nodata2$store)[levels(nodata2$store) == "11"] <- "Hy-Vee Independence #2"
    levels(nodata2$store)[levels(nodata2$store) == "9"] <- "Hy-Vee Kansas City #1"
    levels(nodata2$store)[levels(nodata2$store) == "7"] <- "Hy-Vee Kansas City #2"
    levels(nodata2$store)[levels(nodata2$store) == "8"] <- "Hy-Vee Kansas City #3"
    levels(nodata2$store)[levels(nodata2$store) == "61"] <- "Landmark Inn"
    levels(nodata2$store)[levels(nodata2$store) == "14"] <- "Hy-Vee Lawrence #1"
    levels(nodata2$store)[levels(nodata2$store) == "15"] <- "Hy-Vee Lawrence #2"
    levels(nodata2$store)[levels(nodata2$store) == "12"] <- "Hy-Vee Lee's Summit #1"
    levels(nodata2$store)[levels(nodata2$store) == "13"] <- "Hy-Vee Lee's Summit #2"
    levels(nodata2$store)[levels(nodata2$store) == "16"] <- "Hy-Vee Liberty"
    levels(nodata2$store)[levels(nodata2$store) == "17"] <- "Hy-Vee Manhattan"
    levels(nodata2$store)[levels(nodata2$store) == "37"] <- "Hy-Vee Maryville"
    levels(nodata2$store)[levels(nodata2$store) == "43"] <- "Price Chopper Mckeever's"
    levels(nodata2$store)[levels(nodata2$store) == "18"] <- "Hy-Vee Mission"
    levels(nodata2$store)[levels(nodata2$store) == "58"] <- "Newport Grill KC"
    levels(nodata2$store)[levels(nodata2$store) == "23"] <- "Hy-Vee Olathe #1"
    levels(nodata2$store)[levels(nodata2$store) == "24"] <- "Hy-Vee Olathe #2"
    levels(nodata2$store)[levels(nodata2$store) == "46"] <- "Old Shawnee Pizza"
    levels(nodata2$store)[levels(nodata2$store) == "41"] <- "One More Cup"
    levels(nodata2$store)[levels(nodata2$store) == "19"] <- "Hy-Vee Overland Park #1"
    levels(nodata2$store)[levels(nodata2$store) == "20"] <- "Hy-Vee Overland Park #2"
    levels(nodata2$store)[levels(nodata2$store) == "38"] <- "Parkville Coffeehouse"
    levels(nodata2$store)[levels(nodata2$store) == "50"] <- "Pizza Shoppe Belton"
    levels(nodata2$store)[levels(nodata2$store) == "34"] <- "Pizza Shoppe Kearney"
    levels(nodata2$store)[levels(nodata2$store) == "36"] <- "Pizza Shoppe Platte City"
    levels(nodata2$store)[levels(nodata2$store) == "33"] <- "Pizza Shoppe Savannah"
    levels(nodata2$store)[levels(nodata2$store) == "35"] <- "Pizza Shoppe Smithville"
    levels(nodata2$store)[levels(nodata2$store) == "32"] <- "Pizza Shoppe St Joseph"
    levels(nodata2$store)[levels(nodata2$store) == "47"] <- "Poppy's Bakery"
    levels(nodata2$store)[levels(nodata2$store) == "21"] <- "Hy-Vee Prairie Village"
    levels(nodata2$store)[levels(nodata2$store) == "31"] <- "Price Chopper Liberty"
    levels(nodata2$store)[levels(nodata2$store) == "29"] <- "Price Chopper Platte City"
    levels(nodata2$store)[levels(nodata2$store) == "57"] <- "Price Chopper Queen's Stanley KS"
    levels(nodata2$store)[levels(nodata2$store) == "28"] <- "Rays Green Hills"
    levels(nodata2$store)[levels(nodata2$store) == "22"] <- "Hy-Vee Raytown"
    levels(nodata2$store)[levels(nodata2$store) == "48"] <- "Hy-Vee Shawnee"
    levels(nodata2$store)[levels(nodata2$store) == "49"] <- "Shawnee Mission Medical Center"
    levels(nodata2$store)[levels(nodata2$store) == "3"] <- "testpass"
    levels(nodata2$store)[levels(nodata2$store) == "45"] <- "The Merc"
    levels(nodata2$store)[levels(nodata2$store) == "25"] <- "Hy-Vee Topeka"
    levels(nodata2$store)[levels(nodata2$store) == "54"] <- "Tornado Alley Trading Co"
    levels(nodata2$store)[levels(nodata2$store) == "42"] <- "Yia Yia's EuroBistro"

    rankstores1 <- nodata2[order(nodata2$total, nodata2$store), ]
    cat("\nBottom ten stores for specified dates:\n")
    print(rankstores1[1:10, ])

    cat("\nGross sales for bottom ten stores: \n$", sum(rankstores1[1:10, ]$total), "\n")

    } else if (store == "all"){
      #find the bottom ten stores
      storelist1 <- levels(factor(monthsales$user_id))

      storetotals1 <- lapply(storelist1, function(storename) {
        subset2 <- monthsales[monthsales$user_id == storename, ]
        return(sum(subset2$product_price))
      })

      framelen <- length(storetotals1)

      nodata2 <- data.frame(store=character(framelen), total=integer(framelen), percent=integer(framelen))
      nodata2$total <- as.numeric(storetotals1)
      nodata2$store <- as.character(storelist1)
      nodata2$percent <- as.numeric(nodata2$total / sum(monthsales$product_price) * 100)

      #change ids to store names
      nodata2$store <- as.factor(nodata2$store)
      levels(nodata2$store)[levels(nodata2$store) == "4"] <- "Hy-Vee St Joseph"
      levels(nodata2$store)[levels(nodata2$store) == "26"] <- "Hy-Vee Belton"
      levels(nodata2$store)[levels(nodata2$store) == "6"] <- "Hy-Vee Blue Springs"
      levels(nodata2$store)[levels(nodata2$store) == "30"] <- "Bulk-it"
      levels(nodata2$store)[levels(nodata2$store) == "56"] <- "Daisy House Atlantic"
      levels(nodata2$store)[levels(nodata2$store) == "59"] <- "Hy-Vee Gladstone"
      levels(nodata2$store)[levels(nodata2$store) == "39"] <- "Grandma Hoerner's Foods"
      levels(nodata2$store)[levels(nodata2$store) == "52"] <- "Green Acres"
      levels(nodata2$store)[levels(nodata2$store) == "44"] <- "Hen House Market"
      levels(nodata2$store)[levels(nodata2$store) == "40"] <- "Hy-Vee Lenexa"
      levels(nodata2$store)[levels(nodata2$store) == "10"] <- "Hy-Vee Independence #1"
      levels(nodata2$store)[levels(nodata2$store) == "11"] <- "Hy-Vee Independence #2"
      levels(nodata2$store)[levels(nodata2$store) == "9"] <- "Hy-Vee Kansas City #1"
      levels(nodata2$store)[levels(nodata2$store) == "7"] <- "Hy-Vee Kansas City #2"
      levels(nodata2$store)[levels(nodata2$store) == "8"] <- "Hy-Vee Kansas City #3"
      levels(nodata2$store)[levels(nodata2$store) == "61"] <- "Landmark Inn"
      levels(nodata2$store)[levels(nodata2$store) == "14"] <- "Hy-Vee Lawrence #1"
      levels(nodata2$store)[levels(nodata2$store) == "15"] <- "Hy-Vee Lawrence #2"
      levels(nodata2$store)[levels(nodata2$store) == "12"] <- "Hy-Vee Lee's Summit #1"
      levels(nodata2$store)[levels(nodata2$store) == "13"] <- "Hy-Vee Lee's Summit #2"
      levels(nodata2$store)[levels(nodata2$store) == "16"] <- "Hy-Vee Liberty"
      levels(nodata2$store)[levels(nodata2$store) == "17"] <- "Hy-Vee Manhattan"
      levels(nodata2$store)[levels(nodata2$store) == "37"] <- "Hy-Vee Maryville"
      levels(nodata2$store)[levels(nodata2$store) == "43"] <- "Price Chopper Mckeever's"
      levels(nodata2$store)[levels(nodata2$store) == "18"] <- "Hy-Vee Mission"
      levels(nodata2$store)[levels(nodata2$store) == "58"] <- "Newport Grill KC"
      levels(nodata2$store)[levels(nodata2$store) == "23"] <- "Hy-Vee Olathe #1"
      levels(nodata2$store)[levels(nodata2$store) == "24"] <- "Hy-Vee Olathe #2"
      levels(nodata2$store)[levels(nodata2$store) == "46"] <- "Old Shawnee Pizza"
      levels(nodata2$store)[levels(nodata2$store) == "41"] <- "One More Cup"
      levels(nodata2$store)[levels(nodata2$store) == "19"] <- "Hy-Vee Overland Park #1"
      levels(nodata2$store)[levels(nodata2$store) == "20"] <- "Hy-Vee Overland Park #2"
      levels(nodata2$store)[levels(nodata2$store) == "38"] <- "Parkville Coffeehouse"
      levels(nodata2$store)[levels(nodata2$store) == "50"] <- "Pizza Shoppe Belton"
      levels(nodata2$store)[levels(nodata2$store) == "34"] <- "Pizza Shoppe Kearney"
      levels(nodata2$store)[levels(nodata2$store) == "36"] <- "Pizza Shoppe Platte City"
      levels(nodata2$store)[levels(nodata2$store) == "33"] <- "Pizza Shoppe Savannah"
      levels(nodata2$store)[levels(nodata2$store) == "35"] <- "Pizza Shoppe Smithville"
      levels(nodata2$store)[levels(nodata2$store) == "32"] <- "Pizza Shoppe St Joseph"
      levels(nodata2$store)[levels(nodata2$store) == "47"] <- "Poppy's Bakery"
      levels(nodata2$store)[levels(nodata2$store) == "21"] <- "Hy-Vee Prairie Village"
      levels(nodata2$store)[levels(nodata2$store) == "31"] <- "Price Chopper Liberty"
      levels(nodata2$store)[levels(nodata2$store) == "29"] <- "Price Chopper Platte City"
      levels(nodata2$store)[levels(nodata2$store) == "57"] <- "Price Chopper Queen's Stanley KS"
      levels(nodata2$store)[levels(nodata2$store) == "28"] <- "Rays Green Hills"
      levels(nodata2$store)[levels(nodata2$store) == "22"] <- "Hy-Vee Raytown"
      levels(nodata2$store)[levels(nodata2$store) == "48"] <- "Hy-Vee Shawnee"
      levels(nodata2$store)[levels(nodata2$store) == "49"] <- "Shawnee Mission Medical Center"
      levels(nodata2$store)[levels(nodata2$store) == "3"] <- "testpass"
      levels(nodata2$store)[levels(nodata2$store) == "45"] <- "The Merc"
      levels(nodata2$store)[levels(nodata2$store) == "25"] <- "Hy-Vee Topeka"
      levels(nodata2$store)[levels(nodata2$store) == "54"] <- "Tornado Alley Trading Co"
      levels(nodata2$store)[levels(nodata2$store) == "42"] <- "Yia Yia's EuroBistro"

      rankstores1 <- nodata2[order(-nodata2$total, nodata2$store), ]
      cat("\nAll stores for specified dates:\n")
      print(rankstores1)

  } else if (store %in% monthsales$user_id) {
    #find the specified store
    as.numeric(store)
    monthproduct <- subset(monthsales, monthsales$user_id %in% store)
    cat("\nGross store sales: \n$", sum(monthproduct$product_price), "\n")
    cat("\nPercentage of YTD: \n", (sum(monthproduct$product_price)) / sum(monthsales$product_price) * 100, "%", "\n")

  } else if (!(store %in% monthsales$user_id)) {
    #throw an error
    stop("invalid store")
  }
}
