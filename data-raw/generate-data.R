dls <- read.csv("data-raw/dls.csv", header=T, encoding="UTF-8", stringsAsFactors = FALSE)
var_table <- read.csv("data-raw/summary_tables_to_variable_names.csv", stringsAsFactors = FALSE)


dls$summary_name <- paste(ifelse(dls$Matrix == "urine", "Urinary", ifelse(dls$Matrix == "blood", "Blood", "Serum")), dls$SummaryName)

var_table$dl_name <- var_table$chemical
var_table$dl_name <- gsub(" \\([0-9]{4} ?- ?[0-9]{4}\\)", "", var_table$dl_name)


lookup_table <- merge(var_table, dls, by.x = "dl_name", by.y="summary_name", all.x = TRUE)

devtools::use_data(lookup_table, internal=TRUE, overwrite=TRUE)
