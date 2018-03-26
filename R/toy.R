# Toy is obtained with the code below
# library(Momocs)
#
# toy <- list(
#   bot       = Momocs::bot %>% Momocs::efourier(5),
#   chaff     = chaff %>% fgProcrustes(),
#   charring  = charring %>% efourier(5),
#   flower    = Momocs::flower,
#   hearts    = hearts %>% fgProcrustes() %>% efourier,
#   molars    = molars %>% fgProcrustes(),
#   mosquito  = mosquito %>% fgProcrustes(),
#   oak       = oak %>% fgProcrustes(),
#   olea      = Momocs::olea %>% Momocs::opoly(5, nb.pts=60),
#   shapes    = shapes %>% efourier(5),
#   trilo     = trilo %>% efourier(5),
#   wings     = wings %>% fgProcrustes()
# )
# class(toy) %<>% c(., "toy")
# devtools::use_data(toy, overwrite = TRUE)

#' Toy dataset from Momocs
#'
#' @docType data
#' @name toy
#' @rdname data_toy
#' @format
#' From Momocs:
#'
#' * bot
#' * chaff
#' * charring
#' * flower
#' * hearts
#' * molars
#' * mosquito
#' * oak
#' * olea
#' * shapes
#' * trilo
#' * wings
#' @source [Momocs]
#' @family datasets
NULL

