
module_ui_filter <- function(id, data) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  # each column in fac becomes a selectInput
  lapply(colnames(data$fac), function(i) {
    selectInput(inputId = ns(paste0('fac_', i)),
                label = i,
                choices =  levels(data$fac[, i]),
                selected = levels(data$fac[, i]),
                selectize=FALSE,
                multiple = TRUE)
  })

}

# server
module_server_filter <- function(input, output, session, data){
  # stupid function to cope with dplyr's non standard evaluation
  filter_x_with_list <- function(x, l){
    id <- vector("list", length(l))
    for (i in seq_along(l))
      id[[i]] <- x$fac[, names(l)[i]] %in% l[[i]]
    keep <- apply(as.data.frame(id), 1, all)
    subset(x, keep)
  }

  # a reactive for filtered data that retrieves values for all selectInputs and use
  # them for filtering with filter_x_with_list
  data_filtered <- reactive({
    res <- lapply(colnames(data$fac), function(i) input[[paste0('fac_', i)]])
    names(res) <- colnames(data$fac)
    filter_x_with_list(data, res)
  })

  return(data_filtered)
}
