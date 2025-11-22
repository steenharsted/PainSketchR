all_triangular_number_combinations <- function(l, i=1, m=data.frame(matrix(ncol=2, nrow=0, dimnames = list(NULL, c("a","b"))))) {
  # This functions is self-referencing and recursive
  # It returns all unique combinations of the triangular numbers of l, from
  # 1 to l in an ordered fashion: e.g. if l=4, it returns:
  #
  # [,1] [,2]
  # [1,]    1    2
  # [2,]    1    3
  # [3,]    1    4
  # [4,]    2    3
  # [5,]    2    4
  # [6,]    3    4

  if (i<l) {
    m <- rbind(m,
               data.frame(matrix(c(rep(i, l-i), (i+1):l), ncol=2, dimnames = list(NULL, c("a","b")))))
    all_triangular_number_combinations(l, i+1, m)
  } else {
    m
  }
}