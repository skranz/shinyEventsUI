#' @export
htmlBar = function(..., objs=list(...), td.style = "padding-right: 5px;") {
  objs = lapply(objs, tags$td, style=td.style)
  tags$table(tags$tr(objs))
}
