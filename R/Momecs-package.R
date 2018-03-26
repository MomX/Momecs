
##### Package documentation and NAMESPACE import

#' Momocs
#'
#' The goal of Momecs is to ease exploration and hypotheses testing
#' on multivariate datasets, notably for morphometrics data.
#' It includes the most common approaches, namely:
#' PCA (Principal Component analysis), LDA (Linear Discriminant Analysis),
#' and friends. It essentially wraps on existing functions.
#'
#' To cite Momecs in publications: not yet published.
#'
#' @seealso Have a look to:
#'
#'  * Homepage: (http://momx.github.io/Momecs/)
#'  * Report issues and/or ask for features: (https://github.com/MomX/Momecs/issues)
#'  * Other MomX packages (https://MomX.github.io/MomX)
#'  * Contact: `bonhomme.vincent@gmail.com`
#'
#' @references
#' Not yet published.
#'
#' @import shiny
#' @import shinydashboard
#' @import Momocs
#'
#' @docType package
#' @name Momecs-package
NULL

# Namespace --------
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' @importFrom magrittr %$%
#' @export
magrittr::`%$%`

#' @importFrom magrittr %T>%
#' @export
magrittr::`%T>%`


# Avoid no visible binding...
globalVariables("toy")

# Welcome message
.onAttach <- function(lib, pkg) {
  packageStartupMessage('This is Momecs ',
                        utils::packageDescription('Momecs', field='Version'),
                        appendLF = TRUE)
}
