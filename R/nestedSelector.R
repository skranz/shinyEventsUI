# TO DO: Add update function for nestedSelector

example = function() {
  library(shinyEvents)




  app = eventsApp()
  selectors = list(
    sections = list(
      label = "Section",
      div = "1",
      choices = list("Section 1"=1,"Section 2"=2),
      children = list("1"="subsections1"),
      contents = list("2"="div2")
    ),
    subsections1 = list(
      label = "Subsection",
      div = "2",
      choices = list("1.1"=1,"1.2"=2),
      contents = list("div1_1","div1_2")
    )
  )

  selUI = nestedSelector(id="sections",selectors=selectors, label="Choose section", btn.size="sm")

  nestedSelectorHandler(id="sections", function(...) {
    args = list(...)
    restore.point("myNestedSelectorHandler")
    cat("\nin nestedSelectorHandler")
  })

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
nestedSelector = function(id,selectors, label="", show.first=TRUE, input.type=c("radioBtnGroup","select")[1], btn.size="sm", selector.par=list()) {
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
    #restore.point("djkfjhdfhkd")
    if (is.null(sel$children)) return(NULL)
    res = nf(as.list(sel$children))
    if (is.null(names(res)) & is.list(res)) names(res) = sel$choices
    res
  })
  names(child.li) = nf(names(selectors))
  child.li = child.li[!sapply(child.li, is.null)]
  child.js = toJSON(child.li,auto_unbox=TRUE)

  div.li = lapply(selectors, function(sel) {
    if (is.null(sel$contents)) return(NULL);
    res = as.list(sel$contents)
    if (is.null(names(res))) names(res) = sel$choices
    res
  })
  names(div.li) = nf(names(selectors))

  div.li = div.li[!sapply(div.li, is.null)]
  div.js = toJSON(div.li, auto_unbox=TRUE)

  select.ui.li = lapply(seq_along(selectors), function(i) {
    make.selector.select.ui(i=i,id=id,selectors=selectors,show.first=show.first, input.type=input.type, nali=nali, selector.par=selector.par, btn.size=btn.size)
  })
  ui.bar = select.ui.li
  names(select.ui.li) = nf(names(selectors))

  spec.js = selector.specific.js(id=id,child.js=child.js,div.js=div.js,nali=nali, show.first=show.first)


  addShinyRessourcePath()
  head.tags = tagList(
    #singleton(tags$head(tags$script(src="jquery/jquery.min.js"))),
    singleton(tags$head(tags$script(src="shinyEventsUI/nestedsel.js")))
  )

  ui = tagList(
    head.tags,
    ui.bar,
    bottomScript(HTML(spec.js)),
    bottomScript(HTML(radioBtnGroupScript()))
  )

  res = list(ui=ui, select.ui.li=select.ui.li, head.tags=head.tags, id=id, selectors=selectors)
}

#' Add a change handler to a nested selector
#' @return value a list with the values of all shown selector parts
#' @export
nestedSelectorHandler = function(id, fun,...,app=getApp()) {
  eventHandler(eventId="nestedSelectorHandlerEvent", id=id,fun=nestedSelectorHandlerInterface,handler=fun,...,jscript="")
}

nestedSelectorHandlerInterface = function(eventId,id,shown_sel, values, shown_contents, handler,...) {
  restore.point("nestedSelectorHandlerInterface")
  value = values
  nc = nchar(id)+2
  names(value) = substring(unlist(shown_sel),nc+1)
  handler(eventId=eventId, id=id, value=value, shown_contents=unlist(shown_contents),...)

}

make.selector.select.ui = function(i,id, selectors, show.first, input.type="radioBtnGroup", nali, selector.par=selector.par, btn.size="sm") {
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
    html =radioBtnGroupHTML(id=sel_id,labels = names(sel$choices),values = unlist(sel$choices),div.style = style, div.extra.class=paste0(nali$sel.class), btn.size=btn.size)
  }
  HTML(html)

}

selector.specific.js = function(id,child.js,div.js, nali, show.first=TRUE) {
  restore.point("selector.specific.js")

  js = paste0('
  var ',nali$so,' = new nestedSelectorObject(',child.js,',',div.js,');

  $(".',nali$sel.class,'").on("change", function() {
    //alert("onchange");
    selectorPartOnChange(this,',nali$so,',"',id,'");
  });
  ')
  # Add call to show_selector for first selector
  if (show.first) {
    js = paste0(js,'\nshow_selector("', nali$sel.ids[[1]], '",',nali$so,');')
  }
  js
}


make.nali = function(id, selectors) {
  restore.point("make.nali")

  sel.ids = paste0(id,"__", names(selectors))
  names(sel.ids) = names(selectors)
  list(
    sel.ids = sel.ids,
    sel.class = paste0(id,"__selector_class"),
    so = paste0(id,"__selector_object")
  )
}

.onLoad = function(...) {
  addShinyEventsUIRessourcePath()
}

addShinyEventsUIRessourcePath = addShinyRessourcePath = function() {

  www.dir = system.file('www', package='shinyEventsUI')
  # init ressource paths
  shiny::addResourcePath(
    prefix = 'shinyEventsUI',
    directoryPath = www.dir
  )

}
