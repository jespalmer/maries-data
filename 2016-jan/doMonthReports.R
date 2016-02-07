source("monthReport.R")

header <- function(title){
  cat("\n", "---------------------------------------------------------------",
  "\n")
  cat(title)
  cat("\n", "---------------------------------------------------------------",
  "\n")
}

#main report title and date
header("2016 Monthly Wholesale Summary ran on Feb 6, 2016")

#Monthly summary
header("2016 Wholesale Totals | January")
report("fixed_orders.csv", date=1)
#Q1 top products as percentage of YTD
header("2016 Wholesale Totals | January | Bestselling Products")
report("fixed_orders.csv", date=1, product="best", store="skip")
#Q1 top stores as percentage of YTD
header("2016 Wholesale Totals | January | Bestselling Stores")
report("fixed_orders.csv", date=1, product="skip", store="best")
#Q1 bottom products as percentage of YTD
header("2016 Wholesale Totals | January | Worstselling Products")
report("fixed_orders.csv", date=1, product="worst", store="skip")
#Q1 bottom stores as percentage of YTD
header("2016 Wholesale Totals | January | Worstselling Stores")
report("fixed_orders.csv", date=1, product="skip", store="worst")
