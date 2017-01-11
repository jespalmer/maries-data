source("monthreport.R")

header <- function(title) {
    cat("\n", "---------------------------------------------------------------",
        "\n")
    cat(title)
    cat("\n", "---------------------------------------------------------------",
        "\n")
}

# main report title and date
header("2016 Gross Yearly Wholesale Summary ran on January 11, 2016")

# Monthly summary
header("2016 Wholesale Year Totals | All 2016 Compared to All 2015")
report("fixed_orders.csv", month1 = "12", month2 = "12", year1 = "16", year2 = "15")
