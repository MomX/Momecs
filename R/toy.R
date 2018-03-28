# Toy is obtained with the code below
# library(Momocs)
#
# toy <- list(
#   bot       = bot %>% efourier(5),
#   chaff     = chaff %>% fgProcrustes(),
#   charring  = charring %>% coo_bookstein %>% coo_slide(ldk=1) %>% chop(~view) %>% efourier(8, norm=FALSE) %>% combine,
#   flower    = Momocs::flower,
#   hearts    = hearts %>% fgProcrustes() %>% coo_slide(ldk=2) %>% efourier(6, norm=FALSE),
#   molars    = molars %>% fgProcrustes() %>% coo_slide(ldk=1) %>% efourier(8, norm=FALSE),
#   mosquito  = mosquito %>% coo_slidedirection("right") %>% efourier(6, norm=FALSE),
#   oak       = oak %>% fgProcrustes(),
#   olea      = olea %>% filter(var != "Cypre") %>% chop(~view) %>% opoly(6) %>% combine,
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

