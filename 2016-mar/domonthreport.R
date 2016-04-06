source("monthreport.R")

header <- function(title){
  cat("\n", "---------------------------------------------------------------",
  "\n")
  cat(title)
  cat("\n", "---------------------------------------------------------------",
  "\n")
}

#main report title and date
header("2016 Monthly Wholesale Summary ran on Mar 1, 2016")

#Monthly summary
header("2016 Wholesale Totals | Mar 2016 compared to Feb 2016")
report("fixed_orders.csv", month1= "03", month2= "02", year1="16", year2="16")
