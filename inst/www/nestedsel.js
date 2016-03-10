nestedSelectorObject = function(selector_child, selector_div) {
  var me = this;
  me.selector_child = selector_child;
  me.selector_div = selector_div;
  me.shown_seldiv = [];
  me.shown_is_sel = [];
  return(me);
};

// show a select selector, the content, and the child selectors
function show_selector(id,so) {
  //alert("show_selector(id=" + id + ")");
  // for select input
  var value = getSelectorPartValue($("#"+id));

  // show selector itself
  so.shown_seldiv.push(id);
  so.shown_is_sel.push(true);
  $("#"+id).show();
  //alert("show_selector: "+id+" value "+value);

  // show divs
  var divs = so.selector_div[id];
  if (typeof divs !== "undefined") {
    var divid = divs[value];
    if (typeof divid !== "undefined") {
      if (divid instanceof Array) {
        for (var i = 0; i < divid.length; i++) {
          so.shown_seldiv.push(divid[i]);
          so.shown_is_sel.push(false);

          $("#"+divid[i]).show();
        }
      } else {
        //alert("div = "+divid);
        so.shown_seldiv.push(divid);
        so.shown_is_sel.push(false);

        $("#"+divid).show();
      }

    }
  }

  // show children
  var children = so.selector_child[id];
  if (typeof children !== "undefined") {
    var childid = children[value];
    //alert("childid: " + childid + " value = "+value);
    if (typeof childid !== "undefined") {
      show_selector(childid,so);
    }
  }
}

// hide shown selectors and divs because
// value changes
function hide_seldiv_onchange(id,  so) {
  // hide later shown elements
  while(so.shown_seldiv.length>0) {
    hide_id = so.shown_seldiv.pop();
    so.shown_is_sel.pop();
    if (hide_id == id) {
      break;
    }
    $("#"+hide_id).hide();
  }
}

function getSelectorPartValue(el) {
  // for select input
  var value = $(el).val();
  // for button group
  if (value === "")
    value = $(el).attr("data-value");
  return(value);
}

function selectorPartOnChange(el,so, selector_id) {
  //alert("selectorOnChange");
  hide_seldiv_onchange(el.id,so);
  show_selector(el.id,so);


  var ids = [];
  var values = [];
  for (var i=0; i < so.shown_seldiv.length; i++) {
    if (so.shown_is_sel[i]) {
      ids.push(so.shown_seldiv[i]);
      values.push(getSelectorPartValue($("#"+so.shown_seldiv[i])));
    }
  }
  Shiny.onInputChange("nestedSelectorHandlerEvent", {eventId:"nestedSelectorHandlerEvent",id: selector_id, shown_sel: ids, values: values});

  return;
}
