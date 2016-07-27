example.table.filter = function() {
  library(SeminarMatching)

  script.dir = "D:/libraries/shinyEventsUI/shinyEventsUI/inst/www"
  app = eventsApp()
  n = 10
  df = data.frame(a = sample(1:2,n, replace = TRUE), x = runif(n))
  html = html.table(df,id="mytab")

  app$ui = bootstrapPage(
    HTML(html),
    add.table.filter("mytab", filter.type="select", num.cols=5)
  )
  viewApp(app)
}

table.filter.header = function() {
  addShinyEventsUIRessourcePath()
  singleton(tags$head(tags$script(src="shinyEventsUI/tablefilter_all_min.js")))

}

add.table.filter = function(table.id, filter.type = "select", num.cols=10, add.header=TRUE) {
  restore.point("add.table.filter")

  inner = paste0("col_",seq_len(num.cols)-1,': "', filter.type,'"', collapse=",\n" )
  js = paste0('
  var myfilter = {
',inner,',
  };
  var tf = setFilterGrid("',table.id,'",myfilter);
  ')
  if (add.header) {
    return(tagList(
      table.filter.header(),
      tags$script(HTML(js))
    ))
  }
  tags$script(HTML(js))
}
