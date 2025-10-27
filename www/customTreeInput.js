$(document).ready(function() {
  // Use event delegation for dynamically created elements
  $(document).on('shiny:connected', function() {
    initializeTreeInputs();
  });
  
  function initializeTreeInputs() {
    $('.custom-tree-input').each(function() {
      var $container = $(this);
      var inputId = $container.attr('id');
      
      // Skip if already initialized
      if ($container.data('initialized')) {
        return;
      }
      $container.data('initialized', true);
      
      // Initialize state
      function updateState() {
        var state = {};
        $container.find('.tree-node').each(function() {
          var parent = $(this).find('.node-label').data('parent');
          var checked = $(this).find('input[type="checkbox"]:checked');
          // In the original code, it only sets the value if there's a selection. 
          // If you intended to collect all checked items, you'd use a different logic.
          // Keeping the original logic for state reporting.
          if (checked.length > 0) {
            // Since only one is allowed to be checked, .val() gets the value of the first checked element
            state[parent] = checked.val(); 
          } else {
            state[parent] = null;
          }
        });
        Shiny.setInputValue(inputId, state);
      }
      
      // Update visual state of parent labels
      function updateParentLabels() {
        $container.find('.tree-node').each(function() {
          var nodeLabel = $(this).find('.node-label');
          var hasSelection = $(this).find('input[type="checkbox"]:checked').length > 0;
          if (hasSelection) {
            nodeLabel.addClass('has-selection');
          } else {
            nodeLabel.removeClass('has-selection');
          }
        });
      }
      
      // Handle checkbox changes - enforce only one checked per parent
      $container.find('input[type="checkbox"]').on('change', function() {
        if ($(this).is(':checked')) {
          // Uncheck all other checkboxes in the same parent
          var parentGroup = $(this).attr('name');
          $container.find('input[name="' + parentGroup + '"]').not(this).prop('checked', false);
        }
        updateParentLabels();
        updateState();
      });
      
      // Handle parent label clicks (to deselect AND toggle collapse)
      $container.find('.node-label').on('click', function() {
        var $label = $(this);
        var $treeNode = $label.closest('.tree-node');
        
        // --- NEW COLLAPSE/EXPAND LOGIC ---
        var $children = $treeNode.find('.node-children');
        
        // Toggle the visibility of the children and the indicator icon class
        $children.toggleClass('collapsed');
        $label.toggleClass('expanded');
        // ---------------------------------

        var checked = $treeNode.find('input[type="checkbox"]:checked');
        
        // Only deselect if there's a selection
        if (checked.length > 0) {
          checked.prop('checked', false);
          updateParentLabels();
          updateState();
        }
      });

      // Since we added 'collapsed' to node-children in R, we need to ensure 
      // the corresponding 'expanded' class is NOT on the label initially.
      // It is not added by default, so no extra initialization is strictly needed.
      
      // Initialize
      updateParentLabels();
      updateState();
    });
  }
});
