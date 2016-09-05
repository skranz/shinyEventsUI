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
    sel = "#"+x[key];
    if (key == id) {
      $(sel).css({display: "block", visibility: "visible"});
    } else {
      $(sel).css({display: "none", visibility: "hidden"});
    }
  });
};
