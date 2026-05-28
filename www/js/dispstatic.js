$(document).ready(function() {
  
 // alert("Disp Static");

// $(".navbar-nav").append('<li><a href="cell_lines/index.html" target="_blank">Cell line Summaries</a></li>');

// $("#helpCells").onChange="{if (this.selectedIndex != 0) window.open(this.options[this.selectedIndex].value,'_blank')}"

$(document).on('change','#helpCells', function() {
   alert($("#helpCells option:selected").val());
//   console.log($(this).selectedIndex);
//  if ($(this).selectedIndex != 0) window.open($(this).options[$(this).selectedIndex].value,'_blank');
  
//       var parser = document.createElement("a");
//      parser.href = $(this).attr("href");
//      console.log(parser.hostname);
  
} );

//$('#helpCells').change(function() {
//  alert( $(this).find(":selected").val() );
   // if ($(this).selectedIndex != 0) window.open($(this).options[$(this).selectedIndex].value,'_blank');
//});

});

