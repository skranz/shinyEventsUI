
examples.radioBtnGroup = function() {
  app = eventsApp()

  app$ui = fluidPage(
    p("A radio button group"),
    actionButton("mybtn","Set value = 2"),
    htmlBar(
      radioBtnGroup(id="myradio",c("A","B","C"),values = c("A","B","C"),  panes=list(c("divA","divA2"),"divB","divC")),
      h4("test")
    ),
    textOutput("mytext"),
    div(id="divA", "divA"),
    div(id="divA2","divA2"),
    hidden_div(id="divB", "divB"),
    hidden_div(id="divC", "divC")
  )
  buttonHandler("mybtn", function(...) {
    cat("\nupdate radio value...\n")
    updateRadioBtnGroup(id="myradio",value="B")
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
radioBtnGroup = function(id, labels,values=seq_along(labels), handler=NULL, div.style="", div.extra.class="", btn.size = "xs",btn.style="", panes=show.hide.containers, show.hide.containers=NULL,...) {
  restore.point("radioBtnGroup")

  addShinyRessourcePath()
  inds = seq_along(values)

  ui = radioBtnGroupInner(id,labels,values,div.style=div.style, div.extra.class=div.extra.class, btn.style=btn.style,...)

  if (!is.null(handler)) {
    radioBtnGroupHandler(id,handler,...)
  }
  if (!is.null(panes)) {
    js = radioBtnGroupShowHideJS(id = id, values=values, panes)
    js2 = HTML(radioBtnGroupScript())
    ui = tagList(ui, bottomScript(HTML(js)), singletonBottomScript(js2))
  }

  ui
}

#' Add change hangdler to a radio button group
#' @return value the value of the selected button
#' @export
radioBtnGroupHandler = function(id, fun,..., eventId="radioBtnGroupChange", app=getApp()) {
  restore.point("radioBtnGroupHandler")
  eventHandler(eventId=eventId,id=id,fun=fun,...,
app=app)
}

#' Change selected button of a radion button group
#'
#' use radioBtnGroupHandler to add a handler for a value change
#' @export
updateRadioBtnGroup = function(session=app$session,id,value, app=getApp()) {
  session$sendCustomMessage(type = 'updateRadioBtnGroup',message = list(id=id, value=value))
}


radioBtnGroupInner = function(id, labels, values=seq_along(labels), init.value=values[1], div.style="", div.extra.class="", btn.size="sm", btn.style="", register.with.shiny=TRUE,...) {

  restore.point("radioBtnGroupInner")

  n = length(values)

  div.class = "btn-group asradio-btn-group"
  if (!is.null(div.extra.class))
    div.class = paste(div.class, div.extra.class, paste0("btn-group-",btn.size))

  if (is.null(init.value)) {
    init.value = ""
    btn.class="btn btn-default"
  } else {
    init.ind = which(values==init.value)
    btn.class = rep("btn btn-default",n)
    btn.class[init.ind] = "btn btn-primary"
  }

  #<button id="myid__btn1" type="button" class="btn" value = "1">Left</button>
  btn.html = paste0(collapse="\n",
'<button id="', id,'__btn__',values,'" type="button" class="',btn.class,'" value = "',values,'" style="',btn.style,'">',labels,'</button>')


  div(id=id, class=div.class, style=div.style, `data-toggle` = "buttons", `data-value`=init.value, `data-noshiny` = !register.with.shiny,
    HTML(btn.html)
  )
}


radioBtnGroupHTML = function(id, labels, values=seq_along(labels), init.value=values[1], div.style="", div.extra.class="", btn.size="sm", btn.style="", register.with.shiny=TRUE,...) {

  restore.point("radioBtnGroupHTML")

  n = length(values)

  no.shiny = if(!register.with.shiny) {
    "data-noshiny = true;"
  } else {
    ""
  }

  div.class = "btn-group asradio-btn-group"
  if (!is.null(div.extra.class)) div.class = paste(div.class, div.extra.class, paste0("btn-group-",btn.size))


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

radioBtnGroupShowHideJS = function(id, values, container.id) {
  restore.point("radioBtnGroupShowHideJS")

  divs = as.list(container.id)
  names(divs) = values


  divs = toJSON(divs)
  paste0('
  $("#',id,'").change(function(e){
    var value = $(this).attr("data-value");
    var divs = ',divs,';
    for (var key in divs) {
      var div = divs[key];
      if (key != value) {
        if (div.constructor === Array) {
          for (var i = 0; i < div.length; i++) {
            $("#"+div[i]).hide();
          }
        } else {
          $("#"+div).hide();
        }
      } else {
        if (div.constructor === Array) {
          for (var i = 0; i < div.length; i++) {
            $("#"+div[i]).show().css("visibility","visible");
          }
        } else {
          $("#"+div).show().css("visibility","visible");
        }

      }
    }
    //alert("Change value = "+ value);
  });
  ')
}
