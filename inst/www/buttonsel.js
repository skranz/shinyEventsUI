buttonSelectorObject = function(selector_child, selector_div) {
  var me = this;
  me.selector_div = selector_div;
  me.shown_div = [];
  return(me);
};

// show a select selector, the content, and the child selectors
function show_selector(id,so) {
  //alert("show_selector(id=" + id + ")");
  // for select input
  var value = getSelectorPartValue($("#"+id));

  $("#"+id).show();
  //alert("show_selector: "+id+" value "+value);

  // show divs
  var divs = so.selector_div[id];
  if (typeof divs !== "undefined") {
    var divid = divs[value];
    //alert("show div "+ divid);
    if (typeof divid !== "undefined") {
      if (divid instanceof Array) {
        for (var i = 0; i < divid.length; i++) {
          so.shown_div.push(divid[i]);
          $("#"+divid[i]).show();
        }
      } else {
        //alert("div = "+divid);
        so.shown_div.push(divid);
        $("#"+divid).show();
      }

    }
  }
}

// hide shown selectors and divs because
// value changes
function hide_selector_div_onchange(id,  so) {
  // hide later shown elements
  while(so.shown_div.length>0) {
    hide_id = so.shown_div.pop();
    $("#"+hide_id).hide();
  }
}


function buttonSelectorOnChange(el,so, selector_id) {
  //alert("selectorOnChange");
  hide_selector_div_onchange(el.id,so);
  show_simple_selector(el.id,so);

  value = $(el).attr("data-value");
  Shiny.onInputChange("buttonSelectorHandlerEvent", {eventId:"buttonSelectorHandlerEvent",id: selector_id, value: value, shown_contents: so.shown_div});

  return;
}
