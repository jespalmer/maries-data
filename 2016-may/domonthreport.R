source("monthreport.R")

header <- function(title){
  cat("\n", "---------------------------------------------------------------",
  "\n")
  cat(title)
  cat("\n", "---------------------------------------------------------------",
  "\n")
}

#main report title and date
header("2016 Monthly Wholesale Summary ran on June 9, 2016")

#Monthly summary
header("2016 Wholesale Totals | May 2016 compared to April 2016")
report("fixed_orders.csv", month1= "05", month2= "04", year1="16", year2="16")
