document.addEventListener('DOMContentLoaded', function() {
    // Get tabs elements
    const tabs = document.querySelectorAll('.nav-link');
  
    // Get divs with a specific pattern (class 'body-title')
    const divs = document.querySelectorAll('.body-title');

    // Get the div whose background color will change
    const colorChangingDiv = document.getElementById('footer-div');
  
    // Loop through tabs
    tabs.forEach(function(tab) {
      // Add event listener for tab click
      tab.addEventListener('click', function() {
        // Check if tab is active
        if (tab.classList.contains('active')) {
          // Change text color of divs based on the active tab
          if (tab.id === 'tabset-1-1-tab') {
            divs.forEach(function(div) {
              div.style.color = '#184e77'; // Change text color to red for Tab 1
            });
            colorChangingDiv.style.backgroundColor = '#184e77'; // Change to desired color for Tab 1

            // Change value in the app
            var active_tab = {id: "species", nonce: Math.random()};
            Shiny.onInputChange("jsValue", active_tab)

          } else if (tab.id === 'tabset-1-2-tab') {
            divs.forEach(function(div) {
              div.style.color = '#1e6091'; // Change text color to blue for Tab 2
            });
            colorChangingDiv.style.backgroundColor = '#1e6091'; // Change to desired color for Tab 2

            // Change value in the app
            var active_tab = {id: "thermal", nonce: Math.random()};
            Shiny.onInputChange("jsValue", active_tab)

          } else if (tab.id === 'tabset-1-3-tab') {
            divs.forEach(function(div) {
              div.style.color = '#1a759f'; 
            });
            colorChangingDiv.style.backgroundColor = '#1a759f'; 
            
            // Change value in the app
            var active_tab = {id: "habitat", nonce: Math.random()};
            Shiny.onInputChange("jsValue", active_tab)
            
          } else if (tab.id === 'tabset-1-4-tab') {
            // divs.forEach(function(div) {
            //   div.style.color = '#168aad'; 
            // });
            // colorChangingDiv.style.backgroundColor = '#168aad'; 
            
            // // Change value in the app
            // var active_tab = {id: "diversity", nonce: Math.random()};
            // Shiny.onInputChange("jsValue", active_tab)

            // Update to remove diversity tab
            divs.forEach(function(div) {
              div.style.color = '#168aad'; 
            });
            colorChangingDiv.style.backgroundColor = '#168aad'; 
            
            // Change value in the app
            var active_tab = {id: "atlas", nonce: Math.random()};
            Shiny.onInputChange("jsValue", active_tab)
          } else if (tab.id === 'tabset-1-5-tab') { // ignored
            divs.forEach(function(div) {
              div.style.color = '#2da4c9'; 
            });
            colorChangingDiv.style.backgroundColor = '#2da4c9'; 
            
            // Change value in the app
            var active_tab = {id: "atlas", nonce: Math.random()};
            Shiny.onInputChange("jsValue", active_tab)
          }
        }
      });
    });
  });
  
  Shiny.addCustomMessageHandler('showContext', hideGraphDiv);
  Shiny.addCustomMessageHandler('removeContext', hideGraphDivOn);
  
  function hideGraphDiv(message) {
    var graphDiv = document.getElementById('graphDiv');
      if (graphDiv.style.display === 'none' || graphDiv.style.display === '') {
        graphDiv.style.display = 'block';
      } else {
        graphDiv.style.display = 'none';
      }
  };
  
  function hideGraphDivOn(message) {
    var graphDiv = document.getElementById('graphDiv');
      if (graphDiv.style.display === 'block') {
        graphDiv.style.display = 'none';
      }
  };
  
  // Remove mask control when updating object
  Shiny.addCustomMessageHandler('removeEye', removeEyeFun);
  function removeEyeFun(message) {
    var divToRemove = document.querySelector('.leaflet-bar.easy-button-container.leaflet-control');
    if (divToRemove) {
       // Remove the div from the DOM
       divToRemove.remove();
    }
  }

// Add handler for execution after map
Shiny.addCustomMessageHandler('additionalInfoTrigger', function(value) {
  var message = {id: "id1", nonce: Math.random()};
  Shiny.onInputChange("additionalInfo", message)
});
  
  //TEMPORARY!!!
  Shiny.addCustomMessageHandler('backToTab', backTotabtemp);
  function backTotabtemp(message) {
    
    var tab1 = document.getElementById('tabset-1-1-tab');
    var tab2 = document.getElementById('tabset-1-2-tab');
    var tab3 = document.getElementById('tabset-1-3-tab');
    var tab4 = document.getElementById('tabset-1-4-tab');
    
    // Set the first tab as active
    tab1.classList.add('active');
    tab1.setAttribute('aria-selected', 'true');

    // Remove active state from the second tab
    //tab2.classList.remove('active');
    //tab2.setAttribute('aria-selected', 'false');
    tab3.classList.remove('active');
    tab3.setAttribute('aria-selected', 'false');
    tab4.classList.remove('active');
    tab4.setAttribute('aria-selected', 'false');
    
  };