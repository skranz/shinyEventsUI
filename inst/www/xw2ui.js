var xw2ui = {
  destroy_tab_content: function(divId) {
    $("#"+divId).remove();
  },

  tabs_add: function(id, tabs, select) {
    select = (typeof select !== 'undefined') ?  select : true;
    var x = w2ui[id];
    x.add(tabs);
    for (i=0; i < tabs.length; i++) {
      x.tabs_id.push(tabs[i].id);
    }
    var tabs_divs = {};
    for (i=0; i < x.tabs.length; i++) {
      tabs_divs[x.tabs_id[i]] = x.tabs[i].div_id;
    }
    x.show_hide = new AutoShowHide(tabs_divs);
    if (select & tabs.length >0) {
      x.select(tabs[0].id);
      x.show_hide.show(tabs[0].id);
    }
  },

  xw2tabs: function(x, registerShinyEvents) {
    var registerShinyEvents = (typeof registerShinyEvents !== 'undefined') ?  registerShinyEvents : false;

    var id = x.name;
    var tabs_id = [];
    var tabs_divs = {};

    for (i=0; i < x.tabs.length; i++) {
      tabs_id.push(x.tabs[i].id);
    }
    var divs_id = [];
    for (i=0; i < x.tabs.length; i++) {
      tabs_divs[tabs_id[i]] = x.tabs[i].div_id;
    }
    x.tabs_id = tabs_id;
    x.show_hide = new AutoShowHide(tabs_divs);

    $("#"+id).w2tabs(x);
    x.show_hide.show(x.active);
    w2ui[id].on("click", function(e) {
      //alert(id +": "+ e.target);
      var x = w2ui[id];
      x.show_hide.show(e.target);
      //if (registerShinyEvents) {
        Shiny.onInputChange("click", {eventId: "click", id: id, class: "xw2tab", tabId: e.target, nonce: Math.random()});
      //}

    });

    w2ui[id].on("close", function(e) {
      var tabId = e.target;
      var x = w2ui[id];
      var tabInd = x.tabs_id.indexOf(tabId);
      var divId = x.tabs[tabInd].div_id;
      var keep_content = x.tabs[tabInd].keep_closed_content === true;
      var activeId = x.active;
      var newTabId = tabId;
      var newTabInd = tabInd-1;
      if (activeId == tabId) {
        if (tabInd < x.tabs_id.length-1) {
          newTabInd = tabInd +1;
        }
        newTabId = null;
        if (newTabInd > -1) {
          newTabId = x.tabs_id[newTabInd];
          x.click(newTabId);
        }
      }
      x.tabs_id.splice(tabInd,1);

      if (registerShinyEvents) {
        Shiny.onInputChange("close", {eventId: "close", id: id, class: "xw2tab", tabId: e.target, divId: divId, nonce: Math.random()});
      }

      // click event for new active tab
      if (activeId == tabId) {
        if (newTabInd > -1) {
          x.click(x.tabs_id[newTabId]);
        }
      }
      // remove tab from x.show_hide
      x.show_hide.remove(tabId, true);

      // destroy tab content
      if (!keep_content) {
        xw2ui.destroy_tab_content(divId);
      }

    });


  }
};
