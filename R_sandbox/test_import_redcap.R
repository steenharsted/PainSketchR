library(tidyverse)
library(devtools)

load_all()

background_image <- png::readPNG("inst/extdata/heads.png")

# Read csv data
REDCap_data <- read_csv("R_sandbox/REDCap_export/REDCap_export.csv")

json_files <- list.files(
  "R_sandbox/REDCap_export/REDCap_export",
  pattern = "json$",
  full.names = TRUE
)

pd_data <- pd_json2pd(
  f_name = json_files
)

# Get record_id
pd_data <- pd_data |>
  mutate(
    record_id = str_extract(f, "[0-9]+_") |> str_remove("_$") |> as.numeric(),
    .before = 1
  )

# Get redcap_event_name
pd_data <- pd_data |>
  mutate(
    redcap_event_name = str_extract(f, "[0-9]_.+?[0-1]_") |>
      str_remove("^[0-9]_") |>
      str_remove("_$"),
    .after = record_id
  )

data_merged <- REDCap_data |>
  full_join(pd_data, by = c("record_id", "redcap_event_name"))

data_merged |>
  filter(
    redcap_event_name == "baseline_arm_1",
    tpl == "heads.png"
  ) |>
  filter_out(is.na(headache_type)) |>

  mutate(
    id = record_id
  ) |>
  pd_create_heatmap(
    # id = "record_id",  BUG HERE IF WE DONT RENAME!!!!
    variables = "headache_type",
    n_groups = 1
  )


#
test <- jsonlite::fromJSON(
  "R_sandbox/REDCap_export/REDCap_export/1_baseline_arm_1_ps_drawing_2.json"
)


# canvasWidth og canvasHeight behøver vi ikke for hvert stroke under $s (er gemt i w og h niveauet højere)
# id gemmes forkert - gemmes altid som 'tegning_id'
# Hvad er $s dpr?
# Hvad er $coord ?
# Hvad er $tpl_md5 ?
# Hvad er $crc ?
# det ligner at p mangler stroke id... eller gemmes hvert stroke som en linje?
test
