examples.radioBtnGroup = function() {
  app = eventsApp()

  app$ui = fluidPage(
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
#' use radioBtnGroupHandler to add a handler for a value change
#' @export
radioBtnGroup = function(id, labels,values=seq_along(labels), handler=NULL,...) {
  restore.point("radioBtnGroup")

  addShinyRessourcePath()
  inds = seq_along(values)

  html = radioBtnGroupHTML(id,labels,values,...)
  ui = HTML(html)

  if (!is.null(handler)) {
    radioBtnGroupHandler(id,handler,...)
  }
  ui
}

#' Add change hangdler to a radio button group
#' @return value the value of the selected button
#' @export
radioBtnGroupHandler = function(id, fun,..., eventId="radioBtnGroupChange", app=getApp()) {
  restore.point("buttonHandler")
  eventHandler(eventId=eventId,id=id,fun=fun,...,jscript=radioBtnGroupScript(),
app=app)
}


updateRadioBtnGroup = function(session=app$session,id,value, app=getApp()) {
  session$sendCustomMessage(type = 'updateRadioBtnGroup',message = list(id=id, value=value))
}

radioBtnGroupHTML = function(id, labels, values=seq_along(labels), init.value=values[1], div.style="", div.extra.class=NULL, btn.style="", register.with.shiny=TRUE,...) {

  restore.point("radioBtnGroupHTML")

  n = length(values)

  no.shiny = if(!register.with.shiny) {
    "data-noshiny = true;"
  } else {
    ""
  }

  div.class = "btn-group asradio-btn-group"
  if (!is.null(div.extra.class)) div.class = paste(div.class, div.extra.class)


  if (is.null(init.value)) {
    div.value.str = paste0('data-value = ""')
    btn.class="btn btn-default"
  } else {
    init.ind = which(values==init.value)
    btn.class = rep("btn btn-default",n)
    btn.class[init.ind] = "btn btn-primary"
    div.value.str = paste0('data-value = "',init.value,'"')
  }

  #<button id="myid__btn1" type="button" class="btn" value = "1">Left</button>
  btn.html = paste0(collapse="\n",
'<button id="', id,'__btn__',values,'" type="button" class="',btn.class,'" value = "',values,'" style="',btn.style,'">',labels,'</button>')


  html = paste0(
    '<div id="',id,'" class="', div.class,'" data-toggle="buttons" ', div.value.str, " ", no.shiny,' style="', div.style,'">\n',
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
  //alert(".asradio-btn-group .btn.click: oldval = "+oldval+" newval = "+newval);

  if (oldval !== newval) {
    // update value of container div
    $("#"+groupid).attr("data-value",newval);


    // change button class to primary
    var oldid = "#"+groupid+"__btn__"+oldval;
    $(this).removeClass( "btn-default" ).addClass( "btn-primary" );
    $(oldid).removeClass( "btn-primary" ).addClass( "btn-default" );
    //alert(oldid)
    $("#"+groupid).trigger("change");

    // send shiny change event
    var noshiny = $("#"+groupid).attr("data-noshiny");
    //alert("noshiny = "+noshiny);
    if (noshiny !== true) {
      Shiny.onInputChange("radioBtnGroupChange", {eventId:"radioBtnGroupChange",id: groupid, value: newval});
    }
    return;
  }
});

Shiny.addCustomMessageHandler("updateRadioBtnGroup",
  function(message) {
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
