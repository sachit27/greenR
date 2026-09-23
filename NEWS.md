# greenR 0.0.1.8

* Calculate the street green index with local metric `sf` distances, removing
  the runtime dependency on the DuckDB spatial extension. Empty optional green
  area and tree layers now contribute zero instead of causing an error.
* Fetch the selected OpenStreetMap features in one Overpass request, support
  configurable endpoints and caching, and validate geographic bounding boxes.
* Correct several existing mapping, accessibility, and spatial-processing
  edge cases; add offline regression tests and update documentation.
