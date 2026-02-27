
$(document).on('shiny:connected', function(event) {
  
  $('#to').on('blur', function() {
       if ($('#to').val()-$('#from').val()>9 | $('#to').val()-$('#from').val()<0) {
          $('#valTo').modal('show');
          $('#to').val(parseFloat($('#from').val())+5);
          console.log($('#to').val(parseFloat($('#from').val())+5))
       }
  });
  $('#newCases').on('blur', function() {
       if ($('#newCases').val()>150 | $('#newCases').val()<0) {
          $('#valNewCases').modal('show');
          $('#newCases').val('87');
       }
  });
  $('#prop508').on('blur', function() {
       if ($('#prop508').val()>100 | $('#prop508').val()<0) {
          $('#valPct').modal('show');
          $('#prop508').val('96.7');
       }
  });
  $('#breaks').on('blur', function() {
       if ($('#breaks').val()>($('#to').val()-$('#from').val()) | $('#breaks').val()<=0) {
          $('#valBreaks').modal('show');
          $('#breaks').val('2');
       }
  });
  $('#nSim').on('blur', function() {
       if ($('#nSim').val()>50 | $('#nSim').val()<5) {
          $('#valIters').modal('show');
          $('#nSim').val('5');
       }
  });
  
  // Initially hide the card if 'Hide Card' is selected by default
  if ($('input[name=rb_initial_population]:checked').val() === 'canada') {
      $('#initial_data_card').hide()
  }
  
  $('input[name=rb_initial_population]').on('change', function() {
       if ($(this).val() ==='canada') {
          $('#initial_data_card').hide()
       } else {
          $('#initial_data_card').show()
       }
  });
  
/*  $('#runSim').on('click', function() {

    const tabPaneDivs = $('div.tab-pane');
    output=tabPaneDivs.eq(0)
    rates=tabPaneDivs.eq(1)

    if (rates.hasClass('active')) {
      output.addClass('active show')
      rates.removeClass('active show')
    }
    
    const tabLinks = $('a.nav-link');
    outputLink=tabLinks.eq(0)
    ratesLink=tabLinks.eq(1)
    
    if (ratesLink.hasClass('active')) {
      outputLink.addClass('active')
      ratesLink.removeClass('active')
    }
    
  });
*/

});