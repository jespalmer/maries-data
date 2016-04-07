source("monthreport.R")

header <- function(title){
  cat("\n", "---------------------------------------------------------------",
  "\n")
  cat(title)
  cat("\n", "---------------------------------------------------------------",
  "\n")
}

#main report title and date
header("2016 Monthly Wholesale Summary ran on April 7, 2016")

#Monthly summary
header("2016 Wholesale Totals | Mar 2015 compared to Mar 2016")
report("fixed_orders.csv", month1= "03", month2= "03", year1="16", year2="15")
