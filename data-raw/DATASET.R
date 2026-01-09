## code to prepare `DATASET` dataset goes here

# Generate the MiRD full body template
# This is based on csv files in folder regions_of_full_body_tempalte
pd_demo_body_template <- tibble::tibble(id=as.character(), i=as.integer(), x=as.integer(), y=as.integer())
for(c in fs::dir_ls("data-raw/regions_of_full_body_template/")) {
  d <- read.csv(c, sep=";", header = FALSE)
  pd_demo_body_template <- rbind(pd_demo_body_template, tibble::tibble(id=fs::path_ext_remove(fs::path_file(c)), i=1, x=d[,1], y=d[,2]))
}
pd_demo_body_template <- pd_demo_body_template |> dplyr::mutate(i=as.integer(i), x=as.integer(x), y=as.integer(y))
save(pd_demo_body_template, file="data-raw/pd_demo_body_template.rda")

# Generate a merger of all the body templates to create two full body outlines - front and back
# pd_demo_body_template <- pd_demo_body_template |> 
#   dplyr::mutate(name=id) |>
#   dplyr::group_by(id) |> 
#   dplyr::mutate(i=dplyr::cur_group_id(), id="body") |> 
#   dplyr::ungroup() 
# strokes_i <- unique(pd_demo_body_template$i)
# n_strokes <- length(strokes_i) # How many of them there are (this may change in the while loop)
# p1 <- 1 # pointer 1
# p2 <- 2 # pointer 2
# while (p1 < n_strokes) {
#     while (p2 <=n_strokes) {
#       a <- pd_demo_body_template |> dplyr::filter(i==strokes_i[p1])
#       b <- pd_demo_body_template |> dplyr::filter(i==strokes_i[p2])
#       union <- pd_polyclip(a, b, op = "union")
#       if(length(unique(union$i))==1) {
#         print(paste0("Area ", unique(a$name), " and ", unique(b$name), " has ",length(unique(union$i)), " strokes."))
#         # There is contact!
#         pd_demo_body_template <- pd_demo_body_template |>
#           dplyr::filter(i != strokes_i[p1] & i!= strokes_i[p2]) |>
#           dplyr::bind_rows(dplyr::tibble(i=strokes_i[p1], x=union$x, y=union$y)) 
#         strokes_i <- strokes_i[-p2] # Remove p2 
#         n_strokes <- n_strokes-1
#         p2 <- p1+1
#       }
#       p2 <- p2+1
#     }
#   p1<-p1+1
#   p2<-p1+1
# }
# pd_demo_body_outline_template <- pd_demo_body_template |> dplyr::mutate(i=as.integer(i), x=as.integer(x), y=as.integer(y))
# save(pd_demo_body_outline_template, file="data-raw/pd_demo_body_outline_template.rda")

# Generate a demo data set of pain drawings
# This is based on real-world data collection (mird)
pd_demo_data <- read.csv("data-raw/demo_data.csv") |> 
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, ",\\),\\(,", ";")) |> 
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, "\\(,", "")) |>
  dplyr::mutate(paindrawing_LBP = stringr::str_replace_all(paindrawing_LBP, ",\\)", "")) |>
  dplyr::mutate(paindrawing_LBP = stringr::str_split(paindrawing_LBP, ";")) |> 
  dplyr::mutate(paindrawing_LBP = paindrawing_LBP |> purrr::map(\(x) {
    stringr::str_split(x,",") |> purrr::imap_dfr(\(xy,i) {
      as.integer(xy) |> matrix(ncol=2, byrow=TRUE) |> as.data.frame() |> purrr::set_names(c("x","y")) |> dplyr::mutate(i=i)  
    })
  })) |> 
  tidyr::unnest(paindrawing_LBP) |> dplyr::select(-X) |> dplyr::relocate(i, .after=id)

save(pd_demo_data, file="data-raw/pd_demo_data.rda")
  