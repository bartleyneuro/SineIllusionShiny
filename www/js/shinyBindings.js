
(function(){
  // Calling `jQuery.fingerprint()` will return an MD5 hash, i.e. said
  // fingerprint.

  $.fingerprint = function() {

    // This function, `_raw()`, uses several browser details which are
    // available to JS here to build a string, namely...
    //
    // * the user agent
    // * screen size
    // * color depth
    // * the timezone offset
    // * sessionStorage support
    // * localStorage support
    // * the list of all installed plugins (we're using their names,
    //    descriptions, mime types and file name extensions here)
    function _raw() {
      // That string is the return value.
      return [
        navigator.userAgent,
        [ screen.height, screen.width, screen.colorDepth ].join("x"),
        ( new Date() ).getTimezoneOffset(),
        !!window.sessionStorage,
        !!window.localStorage,
        $.map( navigator.plugins, function(p) {
          return [
            p.name,
            p.description,
            $.map( p, function(mt) {
              return [ mt.type, mt.suffixes ].join("~");
            }).join(",")
          ].join("::");
        }).join(";")
      ].join("###");
    }

    // `_md5()` computes a MD5 hash using [md5-js](http://github.com/wbond/md5-js/).
    function _md5() {
      if ( typeof window.md5 === "function" ) {
        // The return value is the hashed fingerprint string.
        return md5( _raw() );
      }
      else {
        // If `window.md5()` isn't available, an error is thrown.
        throw "md5 unavailable, please get it from http://github.com/wbond/md5-js/";
      }
    }

    // And, since I'm lazy, calling `$.fingerprint()` will return the hash
    // right away, without the need for any other calls.
    return _md5();
  }

  var uid=$.fingerprint();
  
  function setuid() {
    el = $.find('.userid')
    $(el).value = $.fingerprint();
    $(el).trigger("change");
  }
  
  var uidOutputBinding = new Shiny.OutputBinding();
  $.extend(uidOutputBinding, {
    find: function(scope) {
      return $.find('.userid');
    },
    renderError: function(el,error) {
      console.log("Foe");
    },
    renderValue: function(el,data) {
      updateView(data);
      console.log("Friend");
      
    }
  });
  Shiny.outputBindings.register(uidOutputBinding);
  
  var uidInputBinding = new Shiny.InputBinding();
  $.extend(uidInputBinding, {
    find: function(scope) {
      return $.find('.userid');
    },
    getValue: function(el) {
      return $(el).value;
    },
    setValue: function(el, values) {
      $(el).value = $.fingerprint();
    },
    subscribe: function(el, callback) {
      $(el).on("change.uidInputBinding", function(e) {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off(".uidInputBinding");
    }
  });
  Shiny.inputBindings.register(uidInputBinding);
  
  setuid();
})()