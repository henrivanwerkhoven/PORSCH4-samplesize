# markdown document
output_file <- "%s/output/SDD_pancreatitis_associations_other_outcomes_%.0fd_%s" %>% sprintf(getwd(), futime, Sys.Date())
render("scripts/SDD_output_associations.Rmd", output_file=output_file, output_format = "html_document")
rm(output_file)
