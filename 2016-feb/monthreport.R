#file - a csv file
#month1 - the current month number formatted as "01",...,"12", default is 01 for Jan
#month2 - the previous month number formatted as "01",...,"12", default is 01 for Jan
#year1 - the current year formatted as "16", default is 16 for 2016
#year2 - the previous year formatted as "15", default is 15 for 2015

report <- function(file, month1= "01", month2= "01", year1="16", year2="15") {
  #read the file
  this_month <- read.csv(file, header=TRUE, sep ="\t")

  #change the dates to correct format
  this_month_dates <- this_month$placed_date
  this_month_dates <- as.Date(as.character(this_month_dates), "%Y-%m-%d")
  this_month$placed_date <- this_month_dates

  #get the months and years we want
  month_lastyear <- subset(this_month,
    format.Date(placed_date, "%m") == month2 &
    format.Date(placed_date, "%y") == year2)

  month_thisyear <- subset(this_month,
    format.Date(placed_date, "%m") == month1 &
    format.Date(placed_date, "%y") == year1)

  #get month totals for each year and percetages of year to date
  cat("\nTotal gross month sales for current year: \n$",
    sum(month_thisyear$product_price), "\n")
  cat("\nTotal gross month sales for previous year: \n$",
    sum(month_lastyear$product_price), "\n")

  #get only the years with all months to calculate percentages
  ytd_thisyear <- subset(this_month,
    format.Date(placed_date, "%y") == year1)

  ytd_lastyear <- subset(this_month,
    format.Date(placed_date, "%y") == year2)

  cat("\nTotal gross month sales for current year as percentage of current year YTD: \n",
    (sum(month_thisyear$product_price)) / sum(ytd_thisyear$product_price) * 100,
    "%",  "\n")

  cat("\nTotal gross month sales for previous year as percentage of previous year YTD: \n",
    (sum(month_lastyear$product_price)) / sum(ytd_lastyear$product_price) * 100,
    "%",  "\n")

  #get totals sold per product by month for each year
  lastyear_sums <- with(month_lastyear,
    tapply(product_price, product_name, sum))
  cat("\nTotal gross sales summaries in dollars for this month last year:\n")
  print(summary(lastyear_sums))

  thisyear_sums <- with(month_thisyear,
    tapply(product_price, product_name, sum))
  cat("\nTotal gross sales summaries in dollars for this month this year:\n")
  print(summary(thisyear_sums))

  #make into dataframe to generate plot
  d0 <- data.frame(product = names(lastyear_sums), total= lastyear_sums)
  #correcting NA's to be $0.00
  d0[is.na(d0)] <- 0
  d1 <- data.frame(product = names(thisyear_sums), total= thisyear_sums)
  #correcting NA's to be $0.00
  d1[is.na(d1)] <- 0
  #combine dataframes and correct the column names
  compare_months <- merge(d0, d1, by= "product")
  names(compare_months) <- c("product", "lastyear", "currentyear")

  cat("\nTotal month gross sales in dollars for each product compared:\n")
  compare_months_ordered <- compare_months[order(-compare_months$currentyear),]
  print(compare_months_ordered)

  #reshape data to make plots
  compare_months$currentyear <- as.numeric(compare_months$currentyear)
  compare_months$lastyear <- as.numeric(compare_months$lastyear)

  library(tidyr)
  long <- gather(compare_months, year, total, lastyear:currentyear)
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
  pdf(paste("february_summary_byproduct", ".pdf", sep=""), useDingbats=FALSE)
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
  pdf(paste("february_summary_totals", ".pdf", sep=""), useDingbats=FALSE)
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
        ggtitle(paste("february ",
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
  compare_months <- compare_months[order(-compare_months$currentyear,
    compare_months$product), ]
  top_products <- compare_months[1:20, ]
  cat("\nTotal gross month sales for top 20 products for current year: \n",
    "$", sum(top_products$currentyear), "\n")
  cat("\nTotal gross month sales for top 20 products for current year",
    "\n", " as percentage of current year YTD: \n",
    (sum(top_products$currentyear)) / sum(ytd_thisyear$product_price) * 100,
    "%",  "\n")
  cat("\nTotal month gross sales for top 20 products compared:\n")
  print(top_products)

  #reshape data to make plots
  compare_months$currentyear <- as.numeric(compare_months$currentyear)
  compare_months$lastyear <- as.numeric(compare_months$lastyear)

  top_products <- gather(top_products, year, total, lastyear:currentyear)
  top_products <- top_products[order(-top_products$total,
    top_products$product), ]

  #create plot for total comparsion for whole month totals
  par(new_par)
  g <- ggplot(data= top_products, aes(x=year, y=total, fill=year)) +
    geom_bar(stat= "identity") + guides(fill= FALSE) +
    facet_wrap(~product, ncol=5, scales="free") +
    xlab("Year") + ylab ("Total Month Sales in Dollars") +
    ggtitle("Total Month Gross Sales for Bestselling Products This Year Compared") +
    scale_y_continuous(labels = scales::dollar)
  g <- g + theme(
    plot.title = element_text(color="black", face="bold",
    size=12, hjust=.2,vjust=1),
    legend.text = element_text(size=3),
    legend.title=element_blank(),
    axis.text=element_text(size=3),
    axis.title=element_text(size=3),
    strip.text.x = element_text(size=3)
    )
  #save as pdf
  pdf(paste("february_summary_bestproducts", ".pdf", sep=""), useDingbats=FALSE)
  print(g)
  dev.off()

  #create bar charts of worst products
  compare_months <- compare_months[order(compare_months$currentyear,
    compare_months$product), ]
  worst_products <- compare_months[1:20, ]
  cat("\nTotal gross month sales for bottom 20 products for current year: \n",
    "$", sum(worst_products$currentyear), "\n")
  cat("\nTotal gross month sales for bottom 20 products for current year",
    "\n", " as percentage of current year YTD: \n",
    (sum(worst_products$currentyear)) / sum(ytd_thisyear$product_price) * 100,
    "%",  "\n")
  cat("\nTotal month gross sales for bottom 20 products compared:\n")
  print(worst_products)

  #reshape data to make plots
  compare_months$currentyear <- as.numeric(compare_months$currentyear)
  compare_months$lastyear <- as.numeric(compare_months$lastyear)

  worst_products <- gather(worst_products, year, total, lastyear:currentyear)
  worst_products <- worst_products[order(-worst_products$total,
    worst_products$product), ]

  #create plot for total comparsion for whole month totals
  par(new_par)
  g <- ggplot(data= worst_products, aes(x=year, y=total, fill=year)) +
    geom_bar(stat= "identity") + guides(fill= FALSE) +
    facet_wrap(~product, ncol=5, scales="free") +
    xlab("Year") + ylab ("Total Month Sales in Dollars") +
    ggtitle("Total Month Gross Sales for Worstselling Products This Year Compared") +
    scale_y_continuous(labels = scales::dollar)
  g <- g + theme(
    plot.title = element_text(color="black", face="bold",
    size=12, hjust=.2,vjust=1),
    legend.text = element_text(size=3),
    legend.title=element_blank(),
    axis.text=element_text(size=3),
    axis.title=element_text(size=3),
    strip.text.x = element_text(size=3)
    )
  #save as pdf
  pdf(paste("february_summary_worstproducts", ".pdf", sep=""), useDingbats=FALSE)
  print(g)
  dev.off()

  #create bar charts of best stores

  #get totals sold per store by month for each year
  lastyear_storesums <- with(month_lastyear,
    tapply(product_price, name, sum))

  thisyear_storesums <- with(month_thisyear,
    tapply(product_price, name, sum))

  #make into dataframe to generate plot
  d2 <- data.frame(store = names(lastyear_storesums), total= lastyear_storesums)
  d3 <- data.frame(store = names(thisyear_storesums), total= thisyear_storesums)

  #combine dataframes and correct the column names
  compare_storemonths <- merge(d2, d3, by="store", all=TRUE)
  compare_storemonths[is.na(compare_storemonths)] <- 0

  names(compare_storemonths) <- c("store", "lastyear", "currentyear")
  compare_storemonths$currentyear <- as.numeric(compare_storemonths$currentyear)
  compare_storemonths$lastyear <- as.numeric(compare_storemonths$lastyear)

  #order the stores by highest selling this year
  compare_storemonths <- compare_storemonths[order(
    -compare_storemonths$currentyear, compare_storemonths$store), ]

  cat("\nTotal month gross sales in dollars for each store compared:\n")
  compare_stores_ordered <- compare_storemonths[order(
    -compare_storemonths$currentyear),]
  print(compare_stores_ordered)

  top_stores <- compare_stores_ordered[1:20, ]
  cat("\nTotal gross month sales for top 20 stores for current year: \n",
    "$", sum(top_stores$currentyear), "\n")
  cat("\nTotal gross month sales for top 20 stores for current year",
    "\n", " as percentage of current year YTD: \n",
    (sum(top_stores$currentyear)) / sum(ytd_thisyear$product_price) * 100,
    "%",  "\n")
  cat("\nTotal month gross sales for top 20 stores compared:\n")
  print(top_stores)

  #reshape data to make plots
  top_stores <- gather(top_stores, year, total, lastyear:currentyear)
  top_stores <- top_stores[order(-top_stores$total, top_stores$store), ]

  #create plot for total comparsion for store whole month totals
  par(new_par)
  t <- ggplot(data= top_stores, aes(x=year, y=total, fill=year)) +
    geom_bar(stat= "identity") + guides(fill= FALSE) +
    facet_wrap(~store, ncol=5, scales="free") +
    xlab("Year") + ylab ("Total Month Sales in Dollars") +
    ggtitle("Total Month Gross Sales for Bestselling Stores This Year Compared") +
    scale_y_continuous(labels = scales::dollar)
  t <- t + theme(
    plot.title = element_text(color="black", face="bold",
    size=12, hjust=.2,vjust=1),
    legend.text = element_text(size=3),
    legend.title=element_blank(),
    axis.text=element_text(size=3),
    axis.title=element_text(size=3),
    strip.text.x = element_text(size=3)
    )
  #save as pdf
  pdf(paste("february_summary_beststores", ".pdf", sep=""), useDingbats=FALSE)
  print(t)
  dev.off()

  #create bar charts of worst stores
  compare_storemonths <- compare_storemonths[order(
    compare_storemonths$currentyear, compare_storemonths$store), ]
  worst_stores <- compare_storemonths[1:20, ]
  cat("\nTotal gross month sales in dollars for bottom 20 stores for current year: \n",
    "$", sum(worst_stores$currentyear), "\n")
  cat("\nTotal gross month sales in dollars for bottom 20 stores for current year",
    "\n", " as percentage of current year YTD: \n",
    (sum(worst_stores$currentyear)) / sum(ytd_thisyear$product_price) * 100,
    "%",  "\n")
  cat("\nTotal month gross sales in dollars for bottom 20 stores compared:\n")
  print(worst_stores)

  #reshape data to make plots
  worst_stores <- gather(worst_stores, year, total, lastyear:currentyear)
  worst_stores <- worst_stores[order(worst_stores$total, top_stores$store), ]

  #create plot for total comparsion for store whole month totals
  par(new_par)
  t <- ggplot(data= worst_stores, aes(x=year, y=total, fill=year)) +
    geom_bar(stat= "identity") + guides(fill= FALSE) +
    facet_wrap(~store, ncol=5, scales="free") +
    xlab("Year") + ylab ("Total Month Sales in Dollars") +
    ggtitle("Total Month Gross Sales for Wostselling Stores This Year Compared") +
    scale_y_continuous(labels = scales::dollar)
  t <- t + theme(
    plot.title = element_text(color="black", face="bold",
    size=12, hjust=.2,vjust=1),
    legend.text = element_text(size=3),
    legend.title=element_blank(),
    axis.text=element_text(size=3),
    axis.title=element_text(size=3),
    strip.text.x = element_text(size=3)
    )
  #save as pdf
  pdf(paste("february_summary_worststores", ".pdf", sep=""), useDingbats=FALSE)
  print(t)
  dev.off()

  #comparsion of only stores that made profit > 0 in both years for this month
  compare_storesprofit <- merge(d2, d3, by="store", all = FALSE)
  names(compare_storesprofit) <- c("store", "lastyear", "currentyear")
  compare_storesprofit <- subset(compare_storesprofit, lastyear > 0 & currentyear > 0)
  compare_storesprofit <- compare_storesprofit[order(
    -compare_storesprofit$currentyear), ]

  cat("\nTotal gross sales for only stores that made a profit greater than $0\n",
    "in both years for this month:\n")
  print(compare_storesprofit)

  #reshape data to make plots
  both_profit <- gather(compare_storesprofit, year, total, lastyear:currentyear)
  both_profit <- both_profit[order(-both_profit$store, both_profit$year), ]

  #create plot for comparsion of these stores
  par(default_par)
  y <- ggplot(data= both_profit, aes(x=year, y=total, fill=year)) +
    geom_bar(stat= "identity") + guides(fill= FALSE) +
    facet_wrap(~store, ncol=5, scales="free") +
    xlab("Year") + ylab ("Total Month Sales in Dollars") +
    ggtitle("Total Month Gross Sales in Dollars for Stores That Made Profit in Both Years Compared") +
    scale_y_continuous(labels = scales::dollar)
  y <- y + theme(
    plot.title = element_text(color="black", face="bold",
    size=12, hjust=.2,vjust=1),
    legend.text = element_text(size=3),
    legend.title=element_blank(),
    axis.text=element_text(size=3),
    axis.title=element_text(size=3),
    strip.text.x = element_text(size=3)
    )
  #save as pdf
  pdf(paste("february_summary_profitstores", ".pdf", sep=""), useDingbats=FALSE)
  print(y)
  dev.off()
}
