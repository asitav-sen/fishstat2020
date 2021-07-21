$(document).ready(function() {
  $(".navbar .container-fluid .navbar-nav .dropdown .dropdown-menu").each(function(index) {
    const num = index + 1;
    $( this ).addClass("dropdown-menu-" + num);
  });
  
  $(".dropdown-menu-1").append('<li><a href="https://jaljeevika.org" target="_blank">Meet the Sponsor</a></li>');
  $(".dropdown-menu-2").append('<li><a href="https://asitavsen.com" target="_blank">Talk to the Consultant</a></li>');
  $(".dropdown-menu-3").append('<li><a href="https://asitavsen.com/files/india-fisheries-report/" target="_blank">Previous Report</a></li>');
});