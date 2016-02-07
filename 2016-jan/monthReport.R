#file - a csv file
#month - a month number formatted as "01",...,"12", default is 01 for Jan
#year1 - the current year formatted as "16", default is 16 for 2016
#year2 - the previous year formatted as "15", default is 15 for 2015

report <- function(file, month= "01", year1="16", year2="15") {
  #read the file
  this_month <- read.csv(file, header=TRUE, sep ="\t")

  #change the dates to correct format
  this_month_dates <- this_month$placed_date
  this_month_dates <- as.Date(as.character(this_month_dates), "%Y-%m-%d")
  this_month$placed_date <- this_month_dates

  #get the months and years we want
  month_lastyear <- subset(this_month,
    format.Date(placed_date, "%m") == month &
    format.Date(placed_date, "%y") == year2)

  month_thisyear <- subset(this_month,
    format.Date(placed_date, "%m") == month &
    format.Date(placed_date, "%y") == year1)

  #get totals sold per product by month for each year
  lastyear_sums <- with(month_lastyear, tapply(product_price, product_name, sum))
  summary(lastyear_sums)

  thisyear_sums <- with(month_thisyear, tapply(product_price, product_name, sum))
  summary(thisyear_sums)

  #make into dataframe to generate plot
  d0 <- data.frame(product = names(lastyear_sums), total= lastyear_sums)
  #correcting NA's to be $0.00
  d0[is.na(d0)] <- 0
  d1 <- data.frame(product = names(thisyear_sums), total= thisyear_sums)
  #correcting NA's to be $0.00
  d1[is.na(d1)] <- 0
  #combine dataframes and correct the column names
  compare_months <- merge(d0, d1, by= "product")
  names(compare_months) <- c("product", "lastYear", "thisYear")

  #reshape data to make plots
  compare_months$thisYear <- as.numeric(compare_months$thisYear)
  compare_months$lastYear <- as.numeric(compare_months$lastYear)

  library(tidyr)
  long <- gather(compare_months, year, total, lastYear:thisYear)
  long <- long[order(long$product, long$year), ]

  library(ggplot2)

  #creating par changes for the plot
  default_par <- par()
  new_par <- par(mar=c(1, 1, 1, 1), pin=c(8, 11))
  par(new_par)

  #remove outlier for neat plot
  no_outlier <- long[-c(45, 46), ]

  #get the right number of colors needed
  colors <- sample(colors(), 87)

  #make comparsion plot without outlier
  s <- ggplot(no_outlier, aes(x=year, y=total, color=product, group=product)) +
    scale_fill_manual(values=colors) +
    geom_line() +
    geom_point(shape=1) +
    ggtitle("Total Month Gross Sales Compared without Cinnamon Rolls") +
    xlab("Year") + ylab("Total Month Sales by Product in Dollars") +
    scale_y_continuous(breaks=seq(0,800, 25), labels = scales::dollar)
  s <- s + theme(
    plot.title = element_text(color="black", face="bold",
    size=12, hjust=-.2,vjust=1),
    legend.text = element_text(size=3),
    legend.position="right",
    legend.key = element_rect(fill="white"),
    legend.background = element_rect(fill=NA),
    legend.title=element_blank()
    )
  s <- s + guides(col=guide_legend(ncol=4, byrow=TRUE, title.hjust = 0.4))
  #save as pdf
  pdf(paste("january_summary_byproduct", ".pdf", sep=""), useDingbats=FALSE)
  print(s)
  dev.off()

  #create plot for total comparsion for whole month totals
  par(default_par)
  g <- ggplot(data= long, aes(x=year, y=total, fill=year)) +
    geom_bar(stat= "identity") + guides(fill= FALSE) +
    xlab("Year") + ylab ("Total Month Sales in Dollars") +
    ggtitle("Total Month Gross Sales Compared") +
    scale_y_continuous(labels = scales::dollar)
  #save as pdf
  pdf(paste("january_summary_totals", ".pdf", sep=""), useDingbats=FALSE)
  print(g)
  dev.off()

  #create loop to make individual product plots
  product.graph <- function(long, ...) {
    # create list of products in data to loop over
    product_list <- unique(long$product)
    # create for loop to produce ggplot2 graphs
    for (i in seq_along(product_list)) {
      # create plot for each county in df
      plot <- ggplot(subset(long, long$product == product_list[i]),
        aes(x=year, y=total, fill=year)) +
        geom_bar(stat="identity") +
        facet_wrap(~ product, scales="free") +
        guides(fill=FALSE) + xlab("Year") +
        ylab("Product's monthly gross profit in dollars") +
        ggtitle(paste("January ",
        product_list[i],
        " sales in 2015 and 2016", sep = "")) +
        scale_y_continuous(labels = scales::dollar)
      # save plots as .pdf
      pdf(paste(product_list[i], ".pdf", sep=""), useDingbats=FALSE)
      print(plot)
      dev.off()
      # print plots to screen
    }
  }
  product.graph(long)

  #create bar charts of best products

  #order the products by highest selling this year
  compare_months <- compare_months[order(-compare_months$thisYear,
    compare_months$product), ]
  top_products <- compare_months[1:20, ]

  #reshape data to make plots
  compare_months$thisYear <- as.numeric(compare_months$thisYear)
  compare_months$lastYear <- as.numeric(compare_months$lastYear)

  top_products <- gather(top_products, year, total, lastYear:thisYear)
  top_products <- top_products[order(-top_products$total,
    top_products$product), ]

  #create plot for total comparsion for whole month totals
  new_par <- par(mar=c(1, 1, 1, 1), pin=c(11, 8))
  par(new_par)
  g <- ggplot(data= top_products, aes(x=year, y=total, fill=year)) +
    geom_bar(stat= "identity") + guides(fill= FALSE) +
    facet_wrap(~product, ncol=5, scales="free") +
    xlab("Year") + ylab ("Total Month Sales in Dollars") +
    ggtitle("Total Month Gross Sales for Bestselling Products Compared") +
    scale_y_continuous(labels = scales::dollar)
  g <- g + theme(
    plot.title = element_text(color="black", face="bold",
    size=12, hjust=-.2,vjust=1),
    legend.text = element_text(size=3),
    legend.title=element_blank(),
    axis.text=element_text(size=3),
    axis.title=element_text(size=3),
    strip.text.x = element_text(size=3)
    )
  #save as pdf
  pdf(paste("january_summary_bestproducts", ".pdf", sep=""), useDingbats=FALSE)
  print(g)
  dev.off()

  #create bar charts of worst products
  compare_months <- compare_months[order(compare_months$thisYear,
    compare_months$product), ]
  worst_products <- compare_months[1:20, ]

  #reshape data to make plots
  compare_months$thisYear <- as.numeric(compare_months$thisYear)
  compare_months$lastYear <- as.numeric(compare_months$lastYear)

  worst_products <- gather(worst_products, year, total, lastYear:thisYear)
  worst_products <- worst_products[order(-worst_products$total,
    worst_products$product), ]

  #create plot for total comparsion for whole month totals
  new_par <- par(mar=c(1, 1, 1, 1), pin=c(11, 8))
  par(new_par)
  g <- ggplot(data= worst_products, aes(x=year, y=total, fill=year)) +
    geom_bar(stat= "identity") + guides(fill= FALSE) +
    facet_wrap(~product, ncol=5, scales="free") +
    xlab("Year") + ylab ("Total Month Sales in Dollars") +
    ggtitle("Total Month Gross Sales for Worstselling Products Compared") +
    scale_y_continuous(labels = scales::dollar)
  g <- g + theme(
    plot.title = element_text(color="black", face="bold",
    size=12, hjust=-.2,vjust=1),
    legend.text = element_text(size=3),
    legend.title=element_blank(),
    axis.text=element_text(size=3),
    axis.title=element_text(size=3),
    strip.text.x = element_text(size=3)
    )
  #save as pdf
  pdf(paste("january_summary_worstproducts", ".pdf", sep=""), useDingbats=FALSE)
  print(g)
  dev.off()

  #create bar charts of best stores
  #create bar charts of worst stores
}



#   # ------- old code starts here ------
#   #change factor dates into date class
#   library(lubridate)
#   orders$newdates <- ymd(as.character(orders$placed_date))
#
#   #get the right columns we need
#   cleandata <- orders[, c(1, 4, 5, 7)]
#
#   #convert the dates to the right format
#   for (d in date < 10) {
#   gooddates <- lapply(date, function(n) sprintf("%02d", n))
#   }
#
#   #find the gross sales for the specified months
#   monthsales <- subset(cleandata, format.Date(newdates, "%m") %in% gooddates)
#   cat("\nGross sales: \n$", sum(monthsales$product_price), "\n")
#   cat("\nPercentage of YTD: \n", (sum(monthsales$product_price)) / sum(cleandata$product_price) * 100, "%",  "\n")
#
#   #validate product and order the rows by the specified order
#   if (product == "skip") {
#     cat("\n")
#   } else if (product == "best") {
#     #find the top twenty products
#     productlist <- levels(factor(monthsales$product_name))
#
#     totals <- lapply(productlist, function(prodname) {
#       subset1 <- monthsales[monthsales$product_name == prodname, ]
#       return(sum(subset1$product_price))
#     })
#
#     framelen <- length(totals)
#
#     nodata <- data.frame(product=character(framelen), total=integer(framelen), percent=integer(framelen))
#     nodata$total <- as.numeric(totals)
#     nodata$product <- as.character(productlist)
#     nodata$percent <- as.numeric(nodata$total / sum(monthsales$product_price) * 100)
#
#     rankproducts <- nodata[order(-nodata$total, nodata$product), ]
#     cat("\nTop twenty products sold for specified dates:\n")
#     print(rankproducts[1:20, ])
#
#     cat("\nGross sales for top twenty products: \n$", sum(rankproducts[1:20, ]$total), "\n")
#     cat("\nPercentage of sales for specified dates: \n", (sum(rankproducts[1:20, ]$total)) / sum(rankproducts$total) * 100, "%\n")
#
#   } else if (product == "worst"){
#     #find the bottom twenty products
#     productlist <- levels(factor(monthsales$product_name))
#
#     totals <- lapply(productlist, function(prodname) {
#       subset1 <- monthsales[monthsales$product_name == prodname, ]
#       return(sum(subset1$product_price))
#     })
#
#     framelen <- length(totals)
#
#     nodata <- data.frame(product=character(framelen), total=integer(framelen), percent=integer(framelen))
#     nodata$total <- as.numeric(totals)
#     nodata$product <- as.character(productlist)
#     nodata$percent <- as.numeric(nodata$total / sum(monthsales$product_price) * 100)
#
#     rankproducts <- nodata[order(nodata$total, nodata$product), ]
#     cat("\nBottom twenty products sold for specified dates:\n")
#     print(rankproducts[1:20, ])
#     cat("\nGross sales for bottom twenty products: \n$", sum(rankproducts[1:20, ]$total), "\n")
#
#
#   }  else if (product == "all"){
#     #find the all the products sold
#     productlist <- levels(factor(monthsales$product_name))
#
#     totals <- lapply(productlist, function(prodname) {
#       subset1 <- monthsales[monthsales$product_name == prodname, ]
#       return(sum(subset1$product_price))
#     })
#
#     framelen <- length(totals)
#
#     nodata <- data.frame(product=character(framelen), total=integer(framelen), percent=integer(framelen))
#     nodata$total <- as.numeric(totals)
#     nodata$product <- as.character(productlist)
#     nodata$percent <- as.numeric(nodata$total / sum(monthsales$product_price) * 100)
#
#     rankproducts <- nodata[order(-nodata$total, nodata$product), ]
#     cat("\nAll products sold for specified dates:\n")
#     print(rankproducts)
#
#   } else if (product %in% monthsales$product_name) {
#     #find the exact product
#     as.list(product)
#     monthproduct <- subset(monthsales, monthsales$product_name %in% product)
#     cat("\nGross sales for this product: \n$", sum(monthproduct$product_price), "\n")
#     cat("\nPercentage of YTD for this product: \n", (sum(monthproduct$product_price)) / sum(monthsales$product_price) * 100, "%\n")
#
#   } else if (!(product %in% monthsales$product_name)) {
#     #throw an error
#     stop("invalid product")
#   }
#
#   #validate store and order the rows by the specified store
#   if (store == "skip") {
#     cat("\n")
#   } else if (store == "best"){
#     #find the top ten stores
#     storelist <- levels(factor(monthsales$user_id))
#
#     storetotals <- lapply(storelist, function(storename) {
#       subset1 <- monthsales[monthsales$user_id == storename, ]
#       return(sum(subset1$product_price))
#     })
#
#     framelen <- length(storetotals)
#
#     nodata1 <- data.frame(store=character(framelen), total=integer(framelen), percent=integer(framelen))
#     nodata1$total <- as.numeric(storetotals)
#     nodata1$store <- as.character(storelist)
#     nodata1$percent <- as.numeric(nodata1$total / sum(monthsales$product_price) * 100)
#
#     #change ids to store names
#     nodata1$store <- as.factor(nodata1$store)
#     levels(nodata1$store)[levels(nodata1$store) == "4"] <- "Hy-Vee St Joseph"
#     levels(nodata1$store)[levels(nodata1$store) == "26"] <- "Hy-Vee Belton"
#     levels(nodata1$store)[levels(nodata1$store) == "6"] <- "Hy-Vee Blue Springs"
#     levels(nodata1$store)[levels(nodata1$store) == "30"] <- "Bulk-it"
#     levels(nodata1$store)[levels(nodata1$store) == "56"] <- "Daisy House Atlantic"
#     levels(nodata1$store)[levels(nodata1$store) == "59"] <- "Hy-Vee Gladstone"
#     levels(nodata1$store)[levels(nodata1$store) == "39"] <- "Grandma Hoerner's Foods"
#     levels(nodata1$store)[levels(nodata1$store) == "52"] <- "Green Acres"
#     levels(nodata1$store)[levels(nodata1$store) == "44"] <- "Hen House Market"
#     levels(nodata1$store)[levels(nodata1$store) == "40"] <- "Hy-Vee Lenexa"
#     levels(nodata1$store)[levels(nodata1$store) == "10"] <- "Hy-Vee Independence #1"
#     levels(nodata1$store)[levels(nodata1$store) == "11"] <- "Hy-Vee Independence #2"
#     levels(nodata1$store)[levels(nodata1$store) == "9"] <- "Hy-Vee Kansas City #1"
#     levels(nodata1$store)[levels(nodata1$store) == "7"] <- "Hy-Vee Kansas City #2"
#     levels(nodata1$store)[levels(nodata1$store) == "8"] <- "Hy-Vee Kansas City #3"
#     levels(nodata1$store)[levels(nodata1$store) == "61"] <- "Landmark Inn"
#     levels(nodata1$store)[levels(nodata1$store) == "14"] <- "Hy-Vee Lawrence #1"
#     levels(nodata1$store)[levels(nodata1$store) == "15"] <- "Hy-Vee Lawrence #2"
#     levels(nodata1$store)[levels(nodata1$store) == "12"] <- "Hy-Vee Lee's Summit #1"
#     levels(nodata1$store)[levels(nodata1$store) == "13"] <- "Hy-Vee Lee's Summit #2"
#     levels(nodata1$store)[levels(nodata1$store) == "16"] <- "Hy-Vee Liberty"
#     levels(nodata1$store)[levels(nodata1$store) == "17"] <- "Hy-Vee Manhattan"
#     levels(nodata1$store)[levels(nodata1$store) == "37"] <- "Hy-Vee Maryville"
#     levels(nodata1$store)[levels(nodata1$store) == "43"] <- "Price Chopper Mckeever's"
#     levels(nodata1$store)[levels(nodata1$store) == "18"] <- "Hy-Vee Mission"
#     levels(nodata1$store)[levels(nodata1$store) == "58"] <- "Newport Grill KC"
#     levels(nodata1$store)[levels(nodata1$store) == "23"] <- "Hy-Vee Olathe #1"
#     levels(nodata1$store)[levels(nodata1$store) == "24"] <- "Hy-Vee Olathe #2"
#     levels(nodata1$store)[levels(nodata1$store) == "46"] <- "Old Shawnee Pizza"
#     levels(nodata1$store)[levels(nodata1$store) == "41"] <- "One More Cup"
#     levels(nodata1$store)[levels(nodata1$store) == "19"] <- "Hy-Vee Overland Park #1"
#     levels(nodata1$store)[levels(nodata1$store) == "20"] <- "Hy-Vee Overland Park #2"
#     levels(nodata1$store)[levels(nodata1$store) == "38"] <- "Parkville Coffeehouse"
#     levels(nodata1$store)[levels(nodata1$store) == "50"] <- "Pizza Shoppe Belton"
#     levels(nodata1$store)[levels(nodata1$store) == "34"] <- "Pizza Shoppe Kearney"
#     levels(nodata1$store)[levels(nodata1$store) == "36"] <- "Pizza Shoppe Platte City"
#     levels(nodata1$store)[levels(nodata1$store) == "33"] <- "Pizza Shoppe Savannah"
#     levels(nodata1$store)[levels(nodata1$store) == "35"] <- "Pizza Shoppe Smithville"
#     levels(nodata1$store)[levels(nodata1$store) == "32"] <- "Pizza Shoppe St Joseph"
#     levels(nodata1$store)[levels(nodata1$store) == "47"] <- "Poppy's Bakery"
#     levels(nodata1$store)[levels(nodata1$store) == "21"] <- "Hy-Vee Prairie Village"
#     levels(nodata1$store)[levels(nodata1$store) == "31"] <- "Price Chopper Liberty"
#     levels(nodata1$store)[levels(nodata1$store) == "29"] <- "Price Chopper Platte City"
#     levels(nodata1$store)[levels(nodata1$store) == "57"] <- "Price Chopper Queen's Stanley KS"
#     levels(nodata1$store)[levels(nodata1$store) == "28"] <- "Rays Green Hills"
#     levels(nodata1$store)[levels(nodata1$store) == "22"] <- "Hy-Vee Raytown"
#     levels(nodata1$store)[levels(nodata1$store) == "48"] <- "Hy-Vee Shawnee"
#     levels(nodata1$store)[levels(nodata1$store) == "49"] <- "Shawnee Mission Medical Center"
#     levels(nodata1$store)[levels(nodata1$store) == "3"] <- "testpass"
#     levels(nodata1$store)[levels(nodata1$store) == "45"] <- "The Merc"
#     levels(nodata1$store)[levels(nodata1$store) == "25"] <- "Hy-Vee Topeka"
#     levels(nodata1$store)[levels(nodata1$store) == "54"] <- "Tornado Alley Trading Co"
#     levels(nodata1$store)[levels(nodata1$store) == "42"] <- "Yia Yia's EuroBistro"
#
#     rankstores <- nodata1[order(-nodata1$total, nodata1$store), ]
#     cat("\nTop ten stores for specified dates:\n")
#     print(rankstores[1:10, ])
#
#     cat("\nGross sales for top ten stores: \n$", sum(rankstores[1:10, ]$total), "\n")
#     cat("\nPercentage of sales for specified dates: \n", (sum(rankstores[1:10, ]$total) / sum(rankstores$total)) * 100, "%", "\n")
#
#   } else if (store == "worst"){
#     #find the bottom ten stores
#     storelist1 <- levels(factor(monthsales$user_id))
#
#     storetotals1 <- lapply(storelist1, function(storename) {
#       subset2 <- monthsales[monthsales$user_id == storename, ]
#       return(sum(subset2$product_price))
#     })
#
#     framelen <- length(storetotals1)
#
#     nodata2 <- data.frame(store=character(framelen), total=integer(framelen), percent=integer(framelen))
#     nodata2$total <- as.numeric(storetotals1)
#     nodata2$store <- as.character(storelist1)
#     nodata2$percent <- as.numeric(nodata2$total / sum(monthsales$product_price) * 100)
#
#     #change ids to store names
#     nodata2$store <- as.factor(nodata2$store)
#     levels(nodata2$store)[levels(nodata2$store) == "4"] <- "Hy-Vee St Joseph"
#     levels(nodata2$store)[levels(nodata2$store) == "26"] <- "Hy-Vee Belton"
#     levels(nodata2$store)[levels(nodata2$store) == "6"] <- "Hy-Vee Blue Springs"
#     levels(nodata2$store)[levels(nodata2$store) == "30"] <- "Bulk-it"
#     levels(nodata2$store)[levels(nodata2$store) == "56"] <- "Daisy House Atlantic"
#     levels(nodata2$store)[levels(nodata2$store) == "59"] <- "Hy-Vee Gladstone"
#     levels(nodata2$store)[levels(nodata2$store) == "39"] <- "Grandma Hoerner's Foods"
#     levels(nodata2$store)[levels(nodata2$store) == "52"] <- "Green Acres"
#     levels(nodata2$store)[levels(nodata2$store) == "44"] <- "Hen House Market"
#     levels(nodata2$store)[levels(nodata2$store) == "40"] <- "Hy-Vee Lenexa"
#     levels(nodata2$store)[levels(nodata2$store) == "10"] <- "Hy-Vee Independence #1"
#     levels(nodata2$store)[levels(nodata2$store) == "11"] <- "Hy-Vee Independence #2"
#     levels(nodata2$store)[levels(nodata2$store) == "9"] <- "Hy-Vee Kansas City #1"
#     levels(nodata2$store)[levels(nodata2$store) == "7"] <- "Hy-Vee Kansas City #2"
#     levels(nodata2$store)[levels(nodata2$store) == "8"] <- "Hy-Vee Kansas City #3"
#     levels(nodata2$store)[levels(nodata2$store) == "61"] <- "Landmark Inn"
#     levels(nodata2$store)[levels(nodata2$store) == "14"] <- "Hy-Vee Lawrence #1"
#     levels(nodata2$store)[levels(nodata2$store) == "15"] <- "Hy-Vee Lawrence #2"
#     levels(nodata2$store)[levels(nodata2$store) == "12"] <- "Hy-Vee Lee's Summit #1"
#     levels(nodata2$store)[levels(nodata2$store) == "13"] <- "Hy-Vee Lee's Summit #2"
#     levels(nodata2$store)[levels(nodata2$store) == "16"] <- "Hy-Vee Liberty"
#     levels(nodata2$store)[levels(nodata2$store) == "17"] <- "Hy-Vee Manhattan"
#     levels(nodata2$store)[levels(nodata2$store) == "37"] <- "Hy-Vee Maryville"
#     levels(nodata2$store)[levels(nodata2$store) == "43"] <- "Price Chopper Mckeever's"
#     levels(nodata2$store)[levels(nodata2$store) == "18"] <- "Hy-Vee Mission"
#     levels(nodata2$store)[levels(nodata2$store) == "58"] <- "Newport Grill KC"
#     levels(nodata2$store)[levels(nodata2$store) == "23"] <- "Hy-Vee Olathe #1"
#     levels(nodata2$store)[levels(nodata2$store) == "24"] <- "Hy-Vee Olathe #2"
#     levels(nodata2$store)[levels(nodata2$store) == "46"] <- "Old Shawnee Pizza"
#     levels(nodata2$store)[levels(nodata2$store) == "41"] <- "One More Cup"
#     levels(nodata2$store)[levels(nodata2$store) == "19"] <- "Hy-Vee Overland Park #1"
#     levels(nodata2$store)[levels(nodata2$store) == "20"] <- "Hy-Vee Overland Park #2"
#     levels(nodata2$store)[levels(nodata2$store) == "38"] <- "Parkville Coffeehouse"
#     levels(nodata2$store)[levels(nodata2$store) == "50"] <- "Pizza Shoppe Belton"
#     levels(nodata2$store)[levels(nodata2$store) == "34"] <- "Pizza Shoppe Kearney"
#     levels(nodata2$store)[levels(nodata2$store) == "36"] <- "Pizza Shoppe Platte City"
#     levels(nodata2$store)[levels(nodata2$store) == "33"] <- "Pizza Shoppe Savannah"
#     levels(nodata2$store)[levels(nodata2$store) == "35"] <- "Pizza Shoppe Smithville"
#     levels(nodata2$store)[levels(nodata2$store) == "32"] <- "Pizza Shoppe St Joseph"
#     levels(nodata2$store)[levels(nodata2$store) == "47"] <- "Poppy's Bakery"
#     levels(nodata2$store)[levels(nodata2$store) == "21"] <- "Hy-Vee Prairie Village"
#     levels(nodata2$store)[levels(nodata2$store) == "31"] <- "Price Chopper Liberty"
#     levels(nodata2$store)[levels(nodata2$store) == "29"] <- "Price Chopper Platte City"
#     levels(nodata2$store)[levels(nodata2$store) == "57"] <- "Price Chopper Queen's Stanley KS"
#     levels(nodata2$store)[levels(nodata2$store) == "28"] <- "Rays Green Hills"
#     levels(nodata2$store)[levels(nodata2$store) == "22"] <- "Hy-Vee Raytown"
#     levels(nodata2$store)[levels(nodata2$store) == "48"] <- "Hy-Vee Shawnee"
#     levels(nodata2$store)[levels(nodata2$store) == "49"] <- "Shawnee Mission Medical Center"
#     levels(nodata2$store)[levels(nodata2$store) == "3"] <- "testpass"
#     levels(nodata2$store)[levels(nodata2$store) == "45"] <- "The Merc"
#     levels(nodata2$store)[levels(nodata2$store) == "25"] <- "Hy-Vee Topeka"
#     levels(nodata2$store)[levels(nodata2$store) == "54"] <- "Tornado Alley Trading Co"
#     levels(nodata2$store)[levels(nodata2$store) == "42"] <- "Yia Yia's EuroBistro"
#
#     rankstores1 <- nodata2[order(nodata2$total, nodata2$store), ]
#     cat("\nBottom ten stores for specified dates:\n")
#     print(rankstores1[1:10, ])
#
#     cat("\nGross sales for bottom ten stores: \n$", sum(rankstores1[1:10, ]$total), "\n")
#
#     } else if (store == "all"){
#       #find the bottom ten stores
#       storelist1 <- levels(factor(monthsales$user_id))
#
#       storetotals1 <- lapply(storelist1, function(storename) {
#         subset2 <- monthsales[monthsales$user_id == storename, ]
#         return(sum(subset2$product_price))
#       })
#
#       framelen <- length(storetotals1)
#
#       nodata2 <- data.frame(store=character(framelen), total=integer(framelen), percent=integer(framelen))
#       nodata2$total <- as.numeric(storetotals1)
#       nodata2$store <- as.character(storelist1)
#       nodata2$percent <- as.numeric(nodata2$total / sum(monthsales$product_price) * 100)
#
#       #change ids to store names
#       nodata2$store <- as.factor(nodata2$store)
#       levels(nodata2$store)[levels(nodata2$store) == "4"] <- "Hy-Vee St Joseph"
#       levels(nodata2$store)[levels(nodata2$store) == "26"] <- "Hy-Vee Belton"
#       levels(nodata2$store)[levels(nodata2$store) == "6"] <- "Hy-Vee Blue Springs"
#       levels(nodata2$store)[levels(nodata2$store) == "30"] <- "Bulk-it"
#       levels(nodata2$store)[levels(nodata2$store) == "56"] <- "Daisy House Atlantic"
#       levels(nodata2$store)[levels(nodata2$store) == "59"] <- "Hy-Vee Gladstone"
#       levels(nodata2$store)[levels(nodata2$store) == "39"] <- "Grandma Hoerner's Foods"
#       levels(nodata2$store)[levels(nodata2$store) == "52"] <- "Green Acres"
#       levels(nodata2$store)[levels(nodata2$store) == "44"] <- "Hen House Market"
#       levels(nodata2$store)[levels(nodata2$store) == "40"] <- "Hy-Vee Lenexa"
#       levels(nodata2$store)[levels(nodata2$store) == "10"] <- "Hy-Vee Independence #1"
#       levels(nodata2$store)[levels(nodata2$store) == "11"] <- "Hy-Vee Independence #2"
#       levels(nodata2$store)[levels(nodata2$store) == "9"] <- "Hy-Vee Kansas City #1"
#       levels(nodata2$store)[levels(nodata2$store) == "7"] <- "Hy-Vee Kansas City #2"
#       levels(nodata2$store)[levels(nodata2$store) == "8"] <- "Hy-Vee Kansas City #3"
#       levels(nodata2$store)[levels(nodata2$store) == "61"] <- "Landmark Inn"
#       levels(nodata2$store)[levels(nodata2$store) == "14"] <- "Hy-Vee Lawrence #1"
#       levels(nodata2$store)[levels(nodata2$store) == "15"] <- "Hy-Vee Lawrence #2"
#       levels(nodata2$store)[levels(nodata2$store) == "12"] <- "Hy-Vee Lee's Summit #1"
#       levels(nodata2$store)[levels(nodata2$store) == "13"] <- "Hy-Vee Lee's Summit #2"
#       levels(nodata2$store)[levels(nodata2$store) == "16"] <- "Hy-Vee Liberty"
#       levels(nodata2$store)[levels(nodata2$store) == "17"] <- "Hy-Vee Manhattan"
#       levels(nodata2$store)[levels(nodata2$store) == "37"] <- "Hy-Vee Maryville"
#       levels(nodata2$store)[levels(nodata2$store) == "43"] <- "Price Chopper Mckeever's"
#       levels(nodata2$store)[levels(nodata2$store) == "18"] <- "Hy-Vee Mission"
#       levels(nodata2$store)[levels(nodata2$store) == "58"] <- "Newport Grill KC"
#       levels(nodata2$store)[levels(nodata2$store) == "23"] <- "Hy-Vee Olathe #1"
#       levels(nodata2$store)[levels(nodata2$store) == "24"] <- "Hy-Vee Olathe #2"
#       levels(nodata2$store)[levels(nodata2$store) == "46"] <- "Old Shawnee Pizza"
#       levels(nodata2$store)[levels(nodata2$store) == "41"] <- "One More Cup"
#       levels(nodata2$store)[levels(nodata2$store) == "19"] <- "Hy-Vee Overland Park #1"
#       levels(nodata2$store)[levels(nodata2$store) == "20"] <- "Hy-Vee Overland Park #2"
#       levels(nodata2$store)[levels(nodata2$store) == "38"] <- "Parkville Coffeehouse"
#       levels(nodata2$store)[levels(nodata2$store) == "50"] <- "Pizza Shoppe Belton"
#       levels(nodata2$store)[levels(nodata2$store) == "34"] <- "Pizza Shoppe Kearney"
#       levels(nodata2$store)[levels(nodata2$store) == "36"] <- "Pizza Shoppe Platte City"
#       levels(nodata2$store)[levels(nodata2$store) == "33"] <- "Pizza Shoppe Savannah"
#       levels(nodata2$store)[levels(nodata2$store) == "35"] <- "Pizza Shoppe Smithville"
#       levels(nodata2$store)[levels(nodata2$store) == "32"] <- "Pizza Shoppe St Joseph"
#       levels(nodata2$store)[levels(nodata2$store) == "47"] <- "Poppy's Bakery"
#       levels(nodata2$store)[levels(nodata2$store) == "21"] <- "Hy-Vee Prairie Village"
#       levels(nodata2$store)[levels(nodata2$store) == "31"] <- "Price Chopper Liberty"
#       levels(nodata2$store)[levels(nodata2$store) == "29"] <- "Price Chopper Platte City"
#       levels(nodata2$store)[levels(nodata2$store) == "57"] <- "Price Chopper Queen's Stanley KS"
#       levels(nodata2$store)[levels(nodata2$store) == "28"] <- "Rays Green Hills"
#       levels(nodata2$store)[levels(nodata2$store) == "22"] <- "Hy-Vee Raytown"
#       levels(nodata2$store)[levels(nodata2$store) == "48"] <- "Hy-Vee Shawnee"
#       levels(nodata2$store)[levels(nodata2$store) == "49"] <- "Shawnee Mission Medical Center"
#       levels(nodata2$store)[levels(nodata2$store) == "3"] <- "testpass"
#       levels(nodata2$store)[levels(nodata2$store) == "45"] <- "The Merc"
#       levels(nodata2$store)[levels(nodata2$store) == "25"] <- "Hy-Vee Topeka"
#       levels(nodata2$store)[levels(nodata2$store) == "54"] <- "Tornado Alley Trading Co"
#       levels(nodata2$store)[levels(nodata2$store) == "42"] <- "Yia Yia's EuroBistro"
#
#       rankstores1 <- nodata2[order(-nodata2$total, nodata2$store), ]
#       cat("\nAll stores for specified dates:\n")
#       print(rankstores1)
#
#   } else if (store %in% monthsales$user_id) {
#     #find the specified store
#     as.numeric(store)
#     monthproduct <- subset(monthsales, monthsales$user_id %in% store)
#     cat("\nGross store sales: \n$", sum(monthproduct$product_price), "\n")
#     cat("\nPercentage of YTD: \n", (sum(monthproduct$product_price)) / sum(monthsales$product_price) * 100, "%", "\n")
#
#   } else if (!(store %in% monthsales$user_id)) {
#     #throw an error
#     stop("invalid store")
#   }
# }
