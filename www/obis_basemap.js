// Custom Leaflet basemap using OBIS's free vector tiles (land + coastlines)
// instead of Carto's basemaps, which are no longer free.
// Registered as a LeafletWidget method so it can be invoked from R via
// leaflet::invokeMethod(map, NULL, "addObisBasemap", layerId, pane).
// Requires Leaflet.VectorGrid (www/libs/leaflet.vectorgrid.bundled.min.js)
// to be loaded before this script.
LeafletWidget.methods.addObisBasemap = function(layerId, pane) {
  var map = this;

  var baseOptions = {
    interactive: false,
    maxNativeZoom: 14,
    rendererFactory: L.canvas.tile
  };
  if (pane) {
    baseOptions.pane = pane;
  }

  var land = L.vectorGrid.protobuf(
    'https://tiles.obis.org/land_tiles/{z}/{x}/{y}.pbf',
    Object.assign({}, baseOptions, {
      vectorTileLayerStyles: {
        land: { fill: true, fillColor: '#f8fafc', fillOpacity: 1, stroke: false }
      }
    })
  );

  var coastlines = L.vectorGrid.protobuf(
    'https://tiles.obis.org/coastlines_tiles/{z}/{x}/{y}.pbf',
    Object.assign({}, baseOptions, {
      vectorTileLayerStyles: {
        coastlines: { weight: 0.4, color: '#334155', opacity: 0.85, fill: false }
      }
    })
  );

  var group = L.layerGroup([land, coastlines]);
  if (pane) {
    group.options.pane = pane;
  }

  map.layerManager.addLayer(group, 'tile', layerId);
};
