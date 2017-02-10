examples.widgets = function() {
  app = eventsApp()
  app$ui = bootstrapPage(
    slimCollapsePanel(open=TRUE,"Panel",
      p("I am open!")
    )
  )
  viewApp(app)
}

smallButton = function(id, label,class.add="",class="btn btn-default action-button btn-xs",style="",...) {
  tags$button(id=id, style=style, type="button", class=paste(class,class.add),...,label)
}


slimCollapsePanel = function (title, ..., value = title, bsStyle = NULL, heading.style=paste0("padding-top: ", padding,"; padding-bottom: ",padding,";"), open=FALSE, padding="3px")
{
    content <- list(...)
    id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(1,
        1, 1e+06))))
    if (is.null(value)) {
        value = title
    }
    if (is.null(bsStyle)) {
        bsStyle = "default"
    }
    bsTag <- shiny::tags$div(class = paste0("panel panel-", bsStyle),
        value = value, shiny::tags$div(class = "panel-heading", style=heading.style,role = "tab", id = paste0("heading_", id), shiny::tags$h4(class = "panel-title",
                shiny::tags$a(`data-toggle` = "collapse", href = paste0("#",
                  id), title))), shiny::tags$div(id = id, class = paste0("panel-collapse collapse ", if(open) "in" else ""),
            role = "tabpanel", shiny::tags$div(class = "panel-body",
                content)))
    #htmltools::attachDependencies(bsTag, shinyBSDep)
}
