(function(document) {
"use strict";

var showTerms = true;
// Bind an event listener on login form to make an ajax call
document.addEventListener("DOMContentLoaded", function(event) {
      if(document.getElementById("popup").innerHTML === 'show'){
      document.getElementById("popup").innerHTML = "shown";
        var dialog = new Coral.Dialog().set({
                id: 'agreeTerms',
                header: {
                  innerHTML:  Granite.I18n.get('BNP Paribas Terms and Conditions')
                },
                content: {
                  innerHTML: '<p>'+ Granite.I18n.get('You are trying to access a restrited content, Unless you accept this message you will not be allowed to log in') + '<p> <coral-checkbox value="" id="agree">Show Terms and Conditions</coral-checkbox><div id="terms"></div>'
                },
                footer: {
                  innerHTML: '<button id="acceptButton" is="coral-button" variant="primary">' + Granite.I18n.get('accept') + '</button>'
                },
                backdrop: "static"
              });
              document.body.appendChild(dialog);
              dialog.show();

              dialog.on('change', '#agree', function() {
                  if(showTerms){
                    var alert = document.createElement('coral-alert');
                    alert.header.innerHTML = Granite.I18n.get('Below are the terms and conditions:');
                    alert.content.innerHTML = Granite.I18n.get('terms and conditions for log in');
                    dialog.content.appendChild(alert);
                  }
                  showTerms=false;
              });

              dialog.on('click', '#acceptButton', function() {
                dialog.hide();
              });
      }

});

 })(document);