pd_mird2df <- function(df, .id, .st) {
  # The purpose of this function is simply to convert the custom format of
  # the MiRD data base to a regular data frame structure

  # We expect a data frame with an id column and a column of paindrawing data
  # and we expect those two column names to be provided (.id, .st)
  # pain drawing data is a string, e.g.:    (,301,187,),(,285,186,285,187,285,188,)
  # this example represents one string with two strokes of 1 and 3 points, respectively

  require(stringr)
  require(purrr)

  # sample data for code testing:
  #   df <- data.frame(id=letters[3:6], paindrawing=c("", NA, "(,301,187,),(,285,186,275,197,265,168,)", "(,112,122,124,143,)"))
  #   id <- "id"
  #   st <- "paindrawing"


  # regexp explanation:
  #   (?<=\\(,)                   = preceded by (,
  #   ([:digit:]+,[:digit:]+)+    = 1 or more of : 1+ digits , 1+ digits
  #   (?=,\\))                    = followed by ,)

  # Return a list with an element for each string in st which is a vector of strings
  # Each element in the result is a data frame with three columns: stroke, x, y

  ids <- df |> pull( {{.id}} )
  df |>
    pull({{ .st }}) |>
    imap(\(pd,i) {
      if (is.na(pd)) {
        data.frame()
      } else if (pd=="") {
        data.frame()
      } else {
        pd |>
          str_extract_all("(?<=\\(,)([:digit:]+,[:digit:]+)+(?=,\\))") |>
          map(\(x) str_split(x,",")) |> # now a list of lists of char vectors
          map_depth(2, \(df) {as.integer(df) |> matrix(ncol=2, byrow=TRUE) |> as.data.frame() |> set_names(c("x","y")) |> mutate(i=i)} ) |>
          map(\(x) {list_rbind(x, names_to = "stroke")}) |>
          list_rbind()
      }
    }) |>
    list_rbind() |>
    mutate(i = ids[i] ) |>
    rename(id = i) |>
    relocate(id, .before=1) 
    #mutate() stroke and id to factors

}