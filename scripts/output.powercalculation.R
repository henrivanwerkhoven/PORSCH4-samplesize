# markdown document
output_file <- "%s/output/PORSCH_AB_power_simulations_%s_%.0fd_%s" %>% 
  sprintf(getwd(), 
          "patient_days_total_allresections", 
          futime, 
          Sys.Date())
render("scripts/PORSCH-AB_output_powercalc.Rmd", output_file=output_file, output_format = "html_document")
rm(output_file)
