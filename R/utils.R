# stupid function to cope with dplyr's non standard evaluation
filter_with_list <- function(x, l){
  id <- vector("list", length(l))
  keep <- lapply(seq_along(l),
               function(i) unlist(x$fac[, names(l)[i]]) %in% l[[i]]) %>%
    as.data.frame() %>%
    apply(1, all)
  #return(keep)
  # return(keep)
 Momocs:::subsetize(x, keep)
}

# cosmetics. because selectInput accepts characters only, not functions
palette_deliver <- function(input){
  switch(input,
         "pal_qual"=Momocs::pal_qual,
         "pal_seq"=Momocs::pal_seq,
         "pal_div"=Momocs::pal_div)
}
