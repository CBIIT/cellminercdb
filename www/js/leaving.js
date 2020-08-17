$(document).ready(function() {
  
// alert("Hello");

//$(document).on("click", "a", function () {    
$(document).on("click", ".dm", function () { 
//  $("a").on("click", function () {

     var parser = document.createElement("a");
      parser.href = $(this).attr("href");
//      console.log(parser.hostname);
          if (parser.hostname != '127.0.0.1' && parser.hostname != 'localhost' && parser.hostname.indexOf(".gov") == -1) { return confirm("You are leaving the NIH/NCI CellMinerCDB website." + "\nThis external link provides additional information that is consistent with the intended purporse of this site. NIH can not attest to the accuracy of non-federal site." + "\nLinking to a non-federal site does not constitute an endorsement by NIH or any of its employees of the sponsors or the information and products presented on the site. You will be subject to the destination site's privacy policy when you follow the link.")}
         });

});

