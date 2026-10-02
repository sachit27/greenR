# Static cartography uses available vector context, avoiding tile downloads at
# draw time (which can fail after the output device has already been opened).
.greenr_map_context <- function(data) {
  layers <- list()
  if (inherits(data$boundary, "sf")) {
    tile <- .greenr_fetch_basemap(data$boundary)
    if (!is.null(tile)) layers <- list(ggspatial::layer_spatial(tile, alpha = 0.9),
      ggplot2::labs(caption = .greenr_basemap()$credit))
  }
  if (inherits(data$boundary, "sf")) {
    layers <- c(layers, list(ggplot2::geom_sf(data = sf::st_transform(data$boundary, 3857),
      fill = NA, colour = "#8b969d", linewidth = 0.25, inherit.aes = FALSE)))
  }
  context <- data$osm_layers$layers
  for (nm in c("water", "buildings", "roads")) {
    x <- context[[nm]]
    if (!inherits(x, "sf") || !nrow(x)) next
    x <- sf::st_transform(x, 3857)
    layers <- c(layers, list(ggplot2::geom_sf(data = x,
      fill = switch(nm, water = "#c7dfe7", buildings = "#d9dedc", roads = NA),
      colour = if (nm == "roads") "#aab3b7" else NA,
      linewidth = 0.15, inherit.aes = FALSE)))
  }
  layers
}

# Render to a sibling temporary file. Failed rendering cannot leave a blank PNG
# or overwrite an existing successful image.
.greenr_save_plot <- function(filename, plot, ...) {
  if (is.null(plot)) stop("Cannot save a NULL plot.", call. = FALSE)
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("greenr-render-", tmpdir = dirname(filename),
                  fileext = paste0(".", tools::file_ext(filename)))
  on.exit(unlink(tmp), add = TRUE)
  ggplot2::ggsave(tmp, plot = plot, ...)
  if (!file.exists(tmp) || file.info(tmp)$size == 0)
    stop("Plot rendering produced no output.", call. = FALSE)
  if (!file.rename(tmp, filename)) {
    if (!file.copy(tmp, filename, overwrite = TRUE))
      stop("Unable to save rendered plot.", call. = FALSE)
  }
  invisible(filename)
}

# One configuration for static, Leaflet and MapLibre maps. CARTO keys are read
# from the environment and are never written to package source or logs.
.greenr_basemap <- function(dark = FALSE) {
  provider <- getOption("greenR.basemap", if (nzchar(Sys.getenv("CARTO_API_KEY"))) "carto" else "esri")
  if (!provider %in% c("carto", "esri", "osm", "none"))
    stop("greenR.basemap must be carto, esri, osm or none.", call. = FALSE)
  if (provider == "none") return(NULL)
  if (provider == "carto") {
    key <- Sys.getenv("CARTO_API_KEY")
    if (!nzchar(key)) stop("Set CARTO_API_KEY in your user .Renviron before selecting CARTO.", call. = FALSE)
    style <- if (dark) "dark_all" else "light_all"
    return(list(name = paste0("greenR_carto_",style),
      url = paste0("https://basemaps.cartocdn.com/",style,"/{z}/{x}/{y}.png?key=",utils::URLencode(key,reserved=TRUE)),
      attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap contributors</a> &copy; <a href="https://carto.com/attributions">CARTO</a>',
      credit = "Basemap: OpenStreetMap contributors, CARTO", maxzoom=20))
  }
  if (provider == "osm") return(list(name="greenR_osm",url="https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution='&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap contributors</a>',
    credit="Basemap: OpenStreetMap contributors", maxzoom=19))
  list(name="greenR_esri_gray",url="https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}.jpg",
    attribution="Tiles &copy; Esri, HERE, Garmin, OpenStreetMap contributors, and the GIS user community",
    credit="Basemap: Esri, HERE, Garmin, OpenStreetMap contributors, and the GIS user community",maxzoom=16)
}

.greenr_basemap_cache <- new.env(parent=emptyenv())
.greenr_fetch_basemap <- function(boundary, zoom = 14) {
  spec <- .greenr_basemap()
  if (is.null(spec)) return(NULL)
  if (identical(spec$name,"greenR_osm"))
    stop("Use CARTO or Esri for static exports; OSM public tiles are offered here for live interactive viewing only.",call.=FALSE)
  b<-sf::st_transform(boundary,3857)
  bb<-sf::st_bbox(b)
  z<-min(zoom,spec$maxzoom)
  while (z>0) {
    tile_span<-40075016.6856/2^z
    count<-(ceiling((bb['xmax']-bb['xmin'])/tile_span)+2)*(ceiling((bb['ymax']-bb['ymin'])/tile_span)+2)
    if(count<=64)break
    z<-z-1
  }
  # In-memory key intentionally omits credentials; authenticated tiles are never
  # confused with the old unauthenticated CARTO cache.
  cache_key<-paste(spec$name,z,paste(round(bb,2),collapse=":"),sep=":")
  if(exists(cache_key,.greenr_basemap_cache,inherits=FALSE))return(get(cache_key,.greenr_basemap_cache))
  provider<-maptiles::create_provider(spec$name,spec$url,sub="",citation=spec$credit)
  cache_dir<-getOption("greenR.basemap_cache",file.path(tempdir(),"greenR-authenticated-basemaps-v1"))
  dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
  tile<-tryCatch(maptiles::get_tiles(b,provider=provider,zoom=z,crop=TRUE,
    cachedir=cache_dir,retina=FALSE,verbose=FALSE),error=function(e){
      warning("Basemap download failed. Rendering with available local geometry; check provider settings and connectivity.",call.=FALSE)
      NULL
    })
  if(!is.null(tile))assign(cache_key,tile,.greenr_basemap_cache)
  tile
}

.greenr_add_tiles <- function(map, provider = "default", ...) {
  if (provider %in% c("default","OpenStreetMap","Esri.WorldGrayCanvas","CartoDB.Positron","CartoDB.DarkMatter")) {
    spec<-.greenr_basemap(dark=provider %in% c("CartoDB.DarkMatter","Esri.WorldGrayCanvas"))
    if(is.null(spec))return(map)
    return(leaflet::addTiles(map,urlTemplate=spec$url,attribution=spec$attribution,
      options=leaflet::tileOptions(maxZoom=spec$maxzoom),...))
  }
  leaflet::addProviderTiles(map,provider,...)
}

.greenr_style_json <- function(dark=FALSE) {
  spec<-.greenr_basemap(dark)
  if(is.null(spec))return('{"version":8,"sources":{},"layers":[{"id":"background","type":"background","paint":{"background-color":"#edf1ef"}}]}')
  jsonlite::toJSON(list(version=8,sources=list(base=list(type="raster",tiles=list(spec$url),tileSize=256,maxzoom=spec$maxzoom,attribution=spec$attribution)),
    layers=list(list(id="base",type="raster",source="base"))),auto_unbox=TRUE)
}
