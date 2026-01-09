#' Subset pain drawings to the template outline using parallel processing
#'
#' This function relies on parallel processing using sockets to ensure Windows
#' compatability. For faster processing on Linux and MacOS operating systems
#' used the function ... 
#' 
#' @param A A valid pain drawing data structure -- a data frame with cols id,i,x,y
#' @param B A valid pain drawing data structure -- a data frame with cols id,i,x,y
#'
#' @returns A valid pain drawing data structure
#'
#' @export
#' @examples

pd_geom_reduce_to_intersection <- function(A, B, parallel=TRUE, n_workers=0) {
  # A and B are pain drawings : tibbles with cols id,i,x,y
  # The purpose is to reduce the polygons of A and B into the subsets
  # of each polygons
  # intersection with one or more polygons from A
  # A typical use-case would be: Which subsets of anatomical regions in B 
  # intersect with subsets of A.

  # Determine the number of cores/workers to use with a safety net!
  n_cores <- parallelly::availableCores()
  if (n_workers == 0 || (n_workers > n_cores-2) || (n_cores-n_workers < 1)) {
    n_workers <- n_cores -2
  } else {
    if (n_workers<0) {
      n_workers <- n_cores + n_workers # n_workers is negative!
    } else {
      # n_workers > 0 and < n_cores-2 .. just leave as is
    }
  }

  # Set parallel processing as multicore, except if OS is windows
  if (.Platform$OS.type=="windows") {
    future::plan(multisession, workers = n_workers)
  } else {
    future::plan(multicore, workers = n_workers)
  }
  
  all_combos <- dplyr::cross_join(pd |> dplyr::distinct(id, i), template |> dplyr::distinct(id, i), suffix=c(".A",".B"))
  
  result <- tibble::tibble(id=character(), i=integer(), x=integer(), y=integer())
  


  foreach (c = 1:nrow(combos)) %dopar% {
    a <- pd |> dplyr::filter(id == combos[[c,'id.x']] & i == combos[[c,'i.x']])
    b <- template |> dplyr::filter(id == combos[[c,'id.y']] & i == combos[[c,'i.y']])
    
    intersection <- pd_polyclip(a, b, op = "intersection") # Note this may contain multiple subsets
    
    if(tibble::is_tibble(intersection) && nrow(intersection)>0) {
      new_id <- paste0(combos[[c,'id.x']],"_",combos[[c,'id.y']],"_",combos[[c,'i.x']],"_",combos[[c,'i.y']])
      result <- dplyr::bind_rows(result, tibble::tibble(id = new_id, intersection))
      #print(str(result))
      
      #print(paste0(length(unique(intersection$i))," overlap(s) between ",combos[[c,'i.x']]," and ",combos[[c,'i.y']]," with ",nrow(intersection)," vertices."))
      
    }
  }
  future::plan(sequential)
  result
}

