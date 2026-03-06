library(tidyverse)
library(devtools)

load_all()

background_image <- png::readPNG("inst/extdata/heads.png")

# Read csv data
REDCap_data <- read_csv("R_sandbox/REDCap_export/REDCap_export.csv")


pd_json2pd(
  f_name = list.files(
    "R_sandbox/REDCap_export/REDCap_export",
    pattern = "json$"
  )
)
