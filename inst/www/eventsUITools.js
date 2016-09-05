var AutoShowHide = function(x) {
  this.x = x;
};

AutoShowHide.prototype.add = function(id, container) {
  this.x[id] = container;
};

AutoShowHide.prototype.remove = function(id, hide) {
  var hide = (typeof hide !== 'undefined') ?  hide : true;

  if (hide) {
    sel = "#"+this.x[id];
    $(sel).css({display: "none", visibility: "hidden"});
  }
  delete this.x[id];
};

AutoShowHide.prototype.show = function(id) {
  var x = this.x;
  Object.keys(x).forEach(function(key) {
    if (key !== id) {
      $("#"+x[key]).css({display: "none", visibility: "hidden"});
    }
  });
  $("#"+x[id]).css({display: "block", visibility: "visible"});

};

