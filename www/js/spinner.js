var inputSpinnerBinding = new Shiny.InputBinding();
$.extend(inputSpinnerBinding, {
  find: function(scope) {
    return $(scope).find(".spinnernumber");
  },
  getValue: function(el) {
    return (parseFloat($(el).val()).toFixed(2));
  },
  setValue: function(el, value) {
    $(el).attr("value", value);
  },
  subscribe: function(el, callback) {
    $(el).on("change.inputSpinnerBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".inputSpinnerBinding");
  }
});

Shiny.inputBindings.register(inputSpinnerBinding);

function textlower() {
  el = $("#spinner .spinnernumber");
  newval = Math.min(1, Math.max(0, (parseFloat(el.val())-1/20).toFixed(2)));
  $(el).attr("value", newval);
  el.trigger("change");
}


function textraise() {
  el = $("#spinner .spinnernumber");
  newval = Math.min(1, Math.max(0, (parseFloat(el.val())+1/20).toFixed(2)));
  $(el).attr("value", newval);
  el.trigger("change");
}
