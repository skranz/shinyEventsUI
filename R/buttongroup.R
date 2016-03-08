examples.radioBtnGroup = function() {
  app = eventsApp()

  app$ui = fluidPage(
    buttonHandlerEvents(),
    p("A radio button group"),
    actionButton("mybtn","Set value = 2"),
    radioBtnGroup(id="myradio",c("A","B","C")),
    textOutput("mytext")
  )
  buttonHandler("mybtn", function(...) {
    cat("\nupdate radio value...\n")
    updateRadioBtnGroup(id="myradio",value=2)
  })
  radioBtnGroupHandler("myradio", function(value,...) {
    args = list(...)
    restore.point("myradioHandler")
    cat("\nradioBtnGroupHandler...", value)
    setText("mytext",value)

  })
  viewApp(app)
}

#' A button group that acts like a radio button group
#'
#' use radioBtnGroupgHandler to add a handler for a value change
#' @export
radioBtnGroup = function(id, labels,values=seq_along(labels), handler=NULL,...) {
  restore.point("radioBtnGroup")

  addShinyRessourcePath()
  inds = seq_along(values)

  html = radioBtnGroupHTML(id,labels,values,...)
  script = radioBtnGroupScript()
  registerEventIdHandler("radioBtnGroupChange")

  addShinyRessourcePath()

  ui = tagList(
    HTML(html),
    tags$script(HTML(script))
  )

  if (!is.null(handler)) {
    radioBtnGroupHandler(id,handler,...)
  }
  ui
}

# more efficient version of button handler via global eventId handler
radioBtnGroupHandler = function(id, fun,..., eventId="radioBtnGroupChange", app=getApp()) {
  restore.point("buttonHandler")
  eventHandler(eventId=eventId,id=id,fun=fun,...,app=app)
}


updateRadioBtnGroup = function(session=app$session,id,value, app=getApp()) {
  session$sendCustomMessage(type = 'updateRadioBtnGroup',message = list(id=id, value=value))
}

radioBtnGroupHTML = function(id, labels, values=seq_along(labels), init.value=values[1], style="", class="",...) {

  restore.point("radioBtnGroupHTML")

  n = length(values)

  if (is.null(init.value)) {
    class = paste0("btn btn-default ", class)
    div.value.str = paste0('data-value = ""')
  } else {
    init.ind = which(values==init.value)
    tclass = rep("btn btn-default",n)
    tclass[init.ind] = "btn btn-primary"
    class = paste(tclass," ",class)
    div.value.str = paste0('data-value = "',init.value,'"')
  }
  #<button id="myid__btn1" type="button" class="btn" value = "1">Left</button>
  btn.html = paste0(collapse="\n",
'<button id="', id,'__btn__',values,'" type="button" class="',class,'" value = "',values,'" style="',style,'">',labels,'</button>')


  html = paste0(
    '<div id="',id,'" class="btn-group asradio-btn-group" data-toggle="buttons" ', div.value.str, '>\n',
    btn.html,
    '\n</div>'
  )
  html
}

radioBtnGroupScript = function(...) {
  script = '
$(".asradio-btn-group .btn").click(function() {
  // change value of container div
  // when button is pressed
  // and trigger change event
  var groupid = this.parentNode.id;
  var oldval = $("#"+groupid).attr("data-value");
  var newval = $(this).val();
  //alert("clicked "+newval);

  if (oldval !== newval) {
    // update value of container div
    $("#"+groupid).attr("data-value",newval);

    // change button class to primary
    var oldid = "#"+groupid+"__btn__"+oldval;
    $(this).removeClass( "btn-default" ).addClass( "btn-primary" );
    $(oldid).removeClass( "btn-primary" ).addClass( "btn-default" );
    //alert(oldid)

    // send shiny change event
    Shiny.onInputChange("radioBtnGroupChange", {eventId:"radioBtnGroupChange",id: groupid, value: newval});
    return;
  }
});

Shiny.addCustomMessageHandler("updateRadioBtnGroup",
  function(message) {
    //alert("Hi");
    var id = message.id;
    var value = message.value;
    var btnid = "#"+id+"__btn__"+value;
    $(btnid).trigger("click");

    //alert(btnid);
  }
);

  '
  script
}
