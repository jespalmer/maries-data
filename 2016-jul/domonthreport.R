source("monthreport.R")

header <- function(title){
  cat("\n", "---------------------------------------------------------------",
  "\n")
  cat(title)
  cat("\n", "---------------------------------------------------------------",
  "\n")
}

#main report title and date
header("2016 Monthly Wholesale Summary ran on August 7, 2016")

#Monthly summary
header("2016 Wholesale Totals | July 2016 compared to July 2015")
report("fixed_orders.csv", month1= "07", month2= "07", year1="16", year2="15")
