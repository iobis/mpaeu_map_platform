$(document).ready(function() {
  $(document).on('shiny:connected', function() {
    initializeLayerStyleInputs();
  });
  
  function initializeLayerStyleInputs() {
    $('.layer-style-input').each(function() {
      var $container = $(this);
      var inputId = $container.attr('id');
      
      // Skip if already initialized
      if ($container.data('initialized')) {
        return;
      }
      $container.data('initialized', true);
      
      // Initialize palette previews
      function updatePalettePreview($select) {
        var $option = $select.find('option:selected');
        var colors = $option.data('colors');
        var layer = $select.data('layer');
        var $preview = $container.find('.palette-preview[data-layer="' + layer + '"]');
        
        if (colors) {
          var colorArray = colors.split(',');
          $preview.empty();
          colorArray.forEach(function(color) {
            $preview.append(
              $('<div class="palette-preview-color"></div>').css('background-color', color)
            );
          });
        }
      }
      
      // Initialize all palette previews
      $container.find('.palette-selector').each(function() {
        updatePalettePreview($(this));
      });
      
      // Update preview when palette changes
      $container.find('.palette-selector').on('change', function() {
        updatePalettePreview($(this));
      });
      
      // Update alpha value display when slider changes
      $container.find('.alpha-slider').on('input', function() {
        var $slider = $(this);
        var value = parseFloat($slider.val());
        $slider.siblings('.alpha-value').text(value.toFixed(2));
      });
      
      // Handle apply button click
      $container.find('.apply-style-btn').on('click', function() {
        var styles = {};
        
        $container.find('.layer-style-item').each(function() {
          var layer = $(this).data('layer');
          var alpha = parseFloat($(this).find('.alpha-slider').val());
          var palette = $(this).find('.palette-selector').val();
          
          styles[layer] = {
            alpha: alpha,
            palette: palette
          };
        });
        
        // Send to Shiny
        Shiny.setInputValue(inputId, styles, {priority: 'event'});
      });
    });
  }
});