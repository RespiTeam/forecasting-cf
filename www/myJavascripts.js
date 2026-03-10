
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
  
  // Hidding the button to download simulation data
  /*
  const resultsTag = document.getElementById('simResults-times');
  const myButton = document.getElementById('runSim');
  const myLink = document.getElementById('simResults-download_data_btn');
  
  myButton.addEventListener("click", (event) => {
    if (resultsTag.innerText.trim() !== '') {
        myButton.style.display = 'inline-block';
        console.log(resultsTag.innerText)
        console.log('Not empty')
    } else {
        myButton.style.display = 'none';
        console.log(resultsTag.innerText)
        console.log('empty')
    }
  
  })
  */

});

// document.addEventListener("DOMContentLoaded", function() {

// })


