
// show a select selector, the content, and the child selectors
function show_selector(id, selector_child, selector_div, shown_seldiv) {
  //alert("show_selector(id=" + id + ")");

  var value = $("#"+id).val();


  // show selector itself
  shown_seldiv.push(id);
  $("#"+id).show();
  //alert(id+" "+value);

  // show divs
  var divs = selector_div[id];
  if (typeof divs !== "undefined") {
    var divid = divs[value];
    if (typeof divid !== "undefined") {
      if (divid instanceof Array) {
        for (var i = 0; i < divid.length; i++) {
          shown_seldiv.push(divid[i]);
          $("#"+divid[i]).show();
        }
      } else {
        //alert("div = "+divid);
        shown_seldiv.push(divid);
        $("#"+divid).show();
      }

    }
  }

  // show children
  var children = selector_child[id];
  if (typeof children !== "undefined") {
    var childid = children[value];
    if (typeof childid !== "undefined") {
      show_selector(childid,selector_child, selector_div, shown_seldiv);
    }
  }
}

// hide shown selectors and divs because
// value changes
function hide_seldiv_onchange(id,  shown_seldiv) {
  // hide later shown elements
  while(shown_seldiv.length>0) {
    hide_id = shown_seldiv.pop();
    if (hide_id == id) {
      shown_seldiv.push(hide_id);
      break;
    }
    $("#"+hide_id).hide();
  }
}
