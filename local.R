
# # very local solution so far
# data <- list(
#   bot       = bot %>% efourier(5),
#   chaff     = chaff %>% fgProcrustes(),
#   charring  = charring %>% efourier(5),
#   flower    = flower,
#   hearts    = hearts %>% fgProcrustes() %>% efourier,
#   molars    = molars %>% fgProcrustes(),
#   mosquito  = mosquito %>% fgProcrustes(),
#   oak       = oak %>% fgProcrustes(),
#   olea      = olea %>% opoly(5, nb.pts=60),
#   shapes    = shapes %>% efourier(5),
#   trilo     = trilo %>% efourier(5),
#   wings     = wings %>% fgProcrustes()
# )
# save(data, file="data.rda")

# domestic functions -------

# stupid function to cope with dplyr's non standard evaluation
filter_x_with_list <- function(x, l){
  id <- vector("list", length(l))
  for (i in seq_along(l))
    id[[i]] <- x$fac[, names(l)[i]] %in% l[[i]]
  keep <- apply(as.data.frame(id), 1, all)
  subset(x, keep)
}

# cosmetics. because selectInput accepts characters only, not functions
palette_deliver <- function(input){
  switch(input,
         "col_spring"=col_spring,
         "col_summer"=col_summer,
         "col_autumn"=col_autumn,
         "col_qual"=col_qual,
         "col_solarized"=col_solarized)
}

# returns all Coe in global environment
all_Coe_in_GlobalEnv <- function(){
  data_Coe <- sapply(ls(), function(x) if(is.Coe(get(x))) get(x) )
  data_Coe[sapply(data_Coe, function(x) !is.null(x))]
}
