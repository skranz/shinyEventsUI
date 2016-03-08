

example = function() {
  library(shinyEvents)

  app = eventsApp()
  selectors = list(
    sections = list(
      label = "Section",
      div = "1",
      choices = list("Section 1"=1,"Section 2"=2),
      children = list("1"="subsections1"),
      contents.id = list("2"="div2")
    ),
    subsections1 = list(
      label = "Subsection",
      div = "2",
      choices = list("1.1"=1,"1.2"=2),
      contents = list("div1_1","div1_2")
    )
  )

  selUI = nestedSelector(id="sections",selectors=selectors, label="Choose section")

  ui = fluidPage(
    h3("Selector Test"),
    selUI$ui,
    div(id="div1_1", p("I am div1_1!")),
    hidden_div(id="div1_2", p("I am div1_2!")),
    hidden_div(id="div2", p("I am div2!"))
  )

  app$ui = ui
  viewApp(app)
}

#' A div that is by default not shown
#' @export
hidden_div = function(id,...,style="") {
  restore.point("hidden_div")

  style = paste0(style," display: none;")
  div(id=id,style=style,...)
}

#' Nested select menus that show associated div elements
#' @export
nestedSelector = function(id,selectors, label="", show.first=TRUE, input.type=c("radioBtnGroup","select")[1]) {
  restore.point("nestedSelector")

  nf = function(cid) {
    if (is.null(cid)) return(NULL)
    res =  paste0(id,"__",cid)
    if (is.list(cid)) {
      res = as.list(res)
      names(res) = names(cid)
    }
    res
  }
  nali = make.nali(id,selectors)

  child.li = lapply(selectors, function(sel) {
    res = nf(sel$children)
    if (is.null(names(res)) & is.list(res)) names(res) = sel$choices
    res
  })
  names(child.li) = nf(names(selectors))
  child.li = child.li[!sapply(child.li, is.null)]
  child.js = paste0("\n  var ", nali$selector_child," = ",toJSON(child.li,auto_unbox=TRUE),";")

  div.li = lapply(selectors, function(sel) {
    res = sel$contents.id
    if (is.null(names(res)) & is.list(res)) names(res) = sel$choices
    res
  })
  names(div.li) = nf(names(selectors))

  div.li = div.li[!sapply(child.li, is.null)]
  div.js = paste0("\n  var ", nali$selector_div," = ",toJSON(div.li, auto_unbox=TRUE),";")

  select.ui.li = lapply(seq_along(selectors), function(i) {
    make.selector.select.ui(i,id,selectors,show.first, input.type)
  })
  ui.bar = select.ui.li
  names(select.ui.li) = nf(names(selectors))

  spec.js = paste0(
    child.js,
    div.js,
    selector.specific.js(id=id,selectors=selectors,nali=nali),
    collapse="\n"
  )

  # Add call to show_selector for first selector
  if (show.first) {
    code = paste0('show_selector("', nali$sel.ids[[1]], '",',nali$selector_child,',', nali$selector_div,',', nali$shown_seldiv,');')
    spec.js = paste0(spec.js,"\n",code)
  }

  addShinyRessourcePath()
  head.tags = tagList(
    singleton(tags$head(tags$script(src="jquery/jquery.min.js"))),
    singleton(tags$head(tags$script(src="shinyNestedSelector/nestedsel.js")))
  )

  ui = tagList(
    head.tags,
    ui.bar,
    tags$script(HTML(spec.js))
  )

  res = list(ui=ui, select.ui.li=select.ui.li, head.tags=head.tags, id=id, selectors=selectors)
}


make.selector.select.ui = function(i,id, selectors, show.first, input.type="radioBtnGroup") {
  restore.point("make.selector.select.ui")
  nf = function(cid) {
    if (is.null(cid)) return(NULL)
    res =  paste0(id,"__",cid)
    if (is.list(cid)) {
      res = as.list(res)
      names(res) = names(cid)
    }
    res
  }


  name = names(selectors)[[i]]
  sel = selectors[[name]]
  if (length(sel$choices)==0) return(NULL)
  sel_id = nf(name)
  label = sel$label
  if (is.null(label)) label = ""

  style = if ((i==1 & show.first)) "" else "display: none;"
  if (input.type == "select") {
    options = paste0(collapse="\n",paste0(
      "<option value='",sel$choices,"'>",names(sel$choices),"</option>"))

    html = paste0("<select id='",sel_id,"' class='",nali$sel.class,"'  style='",style,"'>\n",options,"\n</select>")
  } else {
    html =radioBtnGroupHTML(id=sel_id,labels = names(sel$choices),values = unlist(sel$choices),style = style)
  }
  HTML(html)

}

selector.specific.js = function(id,selectors, nali = make.nali(id,selectors)) {
  js = paste0('
  var ',nali$shown_seldiv,' = [];

  $(".',nali$sel.class,'").on("change", function() {
    hide_seldiv_onchange(this.id, ',nali$shown_seldiv,');
    show_selector(this.id, ',nali$selector_child,',', nali$selector_div,',', nali$shown_seldiv,');
  });
  ')
  js
}


make.nali = function(id, selectors) {
  restore.point("make.nali")

  sel.ids = paste0(id,"__", names(selectors))
  names(sel.ids) = names(selectors)
  list(
    sel.ids = sel.ids,
    sel.class = paste0(id,"__selector_class"),
    selector_child = paste0(id,"__selector_child"),
    selector_div = paste0(id,"__selector_div"),
    shown_seldiv = paste0(id,"__shown_seldiv")
  )
}


addShinyRessourcePath = function() {

  www.dir = system.file('www', package='shinyEventsUI')
  # init ressource paths
  shiny::addResourcePath(
    prefix = 'shinyEventsUI',
    directoryPath = www.dir
  )

}
