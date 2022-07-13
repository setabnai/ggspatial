
#' Add background OSM tiles
#'
#' Uses [rosm::osm.image()] to add background tiles. If you are publishing
#' a map using these tiles, make sure to use the proper attribution
#' (e.g., "Copyright OpenStreetMap contributors" when using an
#' OpenStreetMap-based tile set).
#'
#' @param type The map type (one of that returned by [rosm::osm.types])
#' @param zoom The zoom level (overrides zoomin)
#' @param zoomin Delta on default zoom. The default value is designed
#'   to download fewer tiles than you probably want. Use `-1` or `0` to
#'   increase the resolution.
#' @param forcedownload Re-download cached tiles?
#' @param cachedir Specify cache directory
#' @param progress Use `progress = "none"` to suppress progress and zoom output
#' @param quiet Use `quiet = FALSE` to see which URLs are downloaded
#' @param interpolate Passed to [grid::rasterGrob()]
#' @param alpha Use to make this layer semi-transparent
#' @param data,mapping Specify data and mapping to use this geom with facets
#' @param brightness (numeric) inclusive range (-255 to 255), negative darkens,
#'   positive lightens
#' @param contrast (numeric) inclusive range (-100 to 100), negative reduces,
#'   positive increases
#' @param gamma (numeric) inclusive range (0.1 to 10), 1 is identity
#' @param grayscale (logical) `TRUE` to enable grayscale shading
#' @param grayscale_mode (character) method to enable graysacle shading.
#'   Includes luminosity (luma601), average (both RGB space) and lightness and
#'   saturation (hsv space).  Ignored if `grayscale` is `FALSE`.
#' @param invert (logical) `TRUE` to invert colours
#'
#' @return A ggplot2 layer
#' @export
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' load_longlake_data(which = "longlake_waterdf")
#'
#' ggplot() +
#'   annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
#'   geom_sf(data = longlake_waterdf, fill = NA, col = "grey50")
#' }
#'
annotation_map_tile <- function(type = "osm", zoom = NULL, zoomin = -2,
                                forcedownload = FALSE, cachedir = NULL,
                                progress = c("text", "none"), quiet = TRUE,
                                interpolate = TRUE, data = NULL, mapping = NULL,
                                alpha = 1,
                                brightness = 0, contrast = 0, gamma = 1,
                                grayscale = FALSE,
                                grayscale_method = c("luminosity", "average", "lightness", "saturation"),
                                invert = FALSE) {

  progress <- match.arg(progress)
  if (grayscale) {
    grayscale_method <- match.arg(grayscale_method, several.ok = FALSE)
  } else {
    grayscale_method <- "none"
  }

  if(!is.null(zoom)) {
    zoomin <- 0
  }

  if(is.null(data)) {
    if(is.null(zoom)) {
      data <- data.frame(type = type, zoomin = zoomin, stringsAsFactors = FALSE)
      mapping <- ggplot2::aes(type = type, zoomin = zoomin)
    } else {
      data <- data.frame(type = type, zoom = zoom, zoomin = zoomin, stringsAsFactors = FALSE)
      mapping <- ggplot2::aes(type = type, zoom = zoom, zoomin = zoomin)
    }
  }

  c(
    ggplot2::layer(
      data = data,
      mapping = mapping,
      geom = GeomMapTile,
      stat = "identity",
      position = "identity",
      params = list(
        forcedownload = forcedownload,
        cachedir = cachedir,
        progress = progress,
        quiet = quiet,
        interpolate = interpolate,
        alpha = alpha,
        brightness = brightness,
        contrast = contrast,
        gamma = gamma,
        grayscale = grayscale,
        grayscale_method = grayscale_method,
        invert = invert
      ),
      inherit.aes = FALSE,
      show.legend = FALSE
    ),
    # use an emtpy geom_sf() with same CRS as the raster to mimic behaviour of
    # using the first layer's CRS as the base CRS for coord_sf().
    ggplot2::geom_sf(
      data = sf::st_sfc(sf::st_point(), crs = sf::st_crs(3857)),
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
}

#' @export
#' @rdname annotation_map_tile
GeomMapTile <- ggplot2::ggproto(
  "GeomMapTile",
  ggplot2::Geom,

  extra_params = "",

  handle_na = function(data, params) {
    data
  },

  default_aes = ggplot2::aes(
    type = "osm",
    zoomin = 0,
    zoom = NULL
  ),

  draw_panel = function(
    data, panel_params, coordinates,
    forcedownload = FALSE, cachedir = NULL,
    progress = c("none", "text"), quiet = TRUE, interpolate = TRUE, alpha = 1,
    brightness = 0, contrast = 0, gamma = 1,
    grayscale = FALSE,
    grayscale_method = c("luminosity", "average", "lightness", "saturation"),
    invert = FALSE
  ) {
    progress <- match.arg(progress)
    if (grayscale) {
      grayscale_method <- match.arg(grayscale_method, several.ok = FALSE)
    } else {
      grayscale_method <- "none"
    }

    coord_crs <- sf::st_crs(panel_params$crs)
    if(!is.null(coord_crs)) {
      sp_bbox <- project_extent(
        xmin = panel_params$x_range[1],
        ymin = panel_params$y_range[1],
        xmax = panel_params$x_range[2],
        ymax = panel_params$y_range[2],
        from_crs = coord_crs,
        to_crs = 4326,
        format = "sp"
      )

      # check for a bounding box that is too small (<5 m)
      # (may cause rosm_raster() to crash RStudio)
      if (.geodist(sp_bbox[, 1], sp_bbox[, 2]) < 5) {
        warning("annotation_map_tile(): bounding box is too small", call. = FALSE)
        return(ggplot2::zeroGrob())
      }


    } else {
      stop("geom_map_tile() requires coord_sf().", call. = FALSE)
    }

    if(coord_crs != sf::st_crs(3857)) {

      # have to use raster to reproject...
      # because this involves a call to projectRaster(), it has to be wrapped in
      # suppressWarnings to avoid the "no non-missing arguments to min; returning Inf" error
      # Suppress "Discarded datum ... in CRS definition" warning (not important at the scale
      # of an OSM map)
      raster <- suppressWarnings(
        rosm_raster(
          x = sp_bbox,
          zoomin = data[["zoomin"]][1],
          zoom = data[["zoom"]][1],
          type = as.character(data[["type"]][1]),
          forcedownload = forcedownload,
          cachedir = cachedir,
          progress = progress,
          quiet = quiet
        )
      )

      # raster::projectRaster has very odd behaviour...it outputs the warning
      # "no non-missing arguments to max; returning -Inf"
      # but only when run with a calling handler
      raster_proj <- suppressWarnings(
          raster::projectRaster(
          raster,
          crs = raster::crs(sf::st_crs(coord_crs)$proj4string)
        )
      )

      ext <- raster::extent(raster_proj)
      corners <- data.frame(x = c(ext@xmin, ext@xmax), y = c(ext@ymin, ext@ymax))
      img <- raster_as_array(raster_proj, alpha = alpha)

    } else {

      # Can use osm.image, which is much faster (and this is the most common case)
      # Suppress "Discarded datum ... in CRS definition" warning (not important at the scale
      # of an OSM map)
      img <- suppressWarnings(
        rosm_image(
          x = sp_bbox,
          zoomin = data[["zoomin"]][1],
          zoom = data[["zoom"]][1],
          type = as.character(data[["type"]][1]),
          forcedownload = forcedownload,
          cachedir = cachedir,
          progress = progress,
          quiet = quiet
        )
      )

      bbox_img <- attr(img, "bbox")
      corners <- data.frame(t(bbox_img))

      # apply the specified alpha
      if(alpha != 1) {
        # it could be that there is already a band 4, or it may not exist yet
        if(dim(img)[3] == 4) {
          tband <- img[ , , 4, drop = FALSE]
        } else {
          tband <- array(1, dim(img)[1:2])
        }

        # multiply the alpha band by the requested alpha (clamping to 0,1)
        tband <- tband * max(0, min(1, alpha))

        # bind it to the original raster
        img <- abind::abind(img[, , 1:3, drop = FALSE], tband)
      }
    }

    # Brightness, Ccontrast and Gamma
    #
    #
    # brightness_value: -255:255
    #
    # The value of brightness will usually be in the range of -255 to +255 for a
    # 24 bit palette.
    #
    # Negative values will darken the image and, conversely, positive values
    # will brighten the image.
    #
    #
    # contrast_value: -100,100
    #
    # The value of contrast will be in the range of -100 to +100
    #
    # Negative values will decrease the amount of contrast and conversely
    # positive values will increase the amount of contrast.
    #

    #browser()

    # apply brightness, contrast and gamma
    if (!(brightness == 0 && contrast == 0 && gamma == 1)) {
      y2 <- apply(
        img[, , 1:3], 3,
        adjustColorComponent,
        brightness_value = brightness / 255.0,
        contrast_factor = ((contrast + 100.0) / 100.0) ^ 2,
        gamma_factor = 1.0 / gamma
      )
      img[, , 1:3] <- array(y2, dim = c(dim(img)[c(1, 2)], 3))
    }

    # invert colours

    if (invert) {
      img[, , 1:3] <- 1 - img[, , 1:3]
    }

    # Convert to grayscale
    #
    # ref: https://en.wikipedia.org/wiki/Grayscale#Converting_color_to_grayscale

    if (grayscale) {

      if (grayscale_method == "luminosity") {

        y <- matrix(img[, , 1:3], ncol = 3)
        y <- y %*% c(0.2126, 0.7152, 0.0722)
        y <- array(y, dim = dim(img)[1:2])

      } else if (grayscale_method == "average") {

        y <- matrix(img[, , 1:3], ncol = 3)
        y <- y %*% c(1, 1, 1) / 3
        y <- array(y, dim = dim(img)[1:2])

      } else if (grayscale_method == "lightness") {

        y <- t(matrix(img[, , 1:3], ncol = 3))
        y <- rgb2hsv(y, maxColorValue = 1L)
        y <- array(y[3, ], dim = dim(img)[1:2])

      } else if (grayscale_method == "saturation") {

        y <- t(matrix(img[, , 1:3], ncol = 3))
        y <- rgb2hsv(y, maxColorValue = 1L)
        y <- array(1 - y[2, ], dim = dim(img)[1:2])

      } else {
        stop("Unknown greyscale_method specified:", grayscale_method)
      }

      img[, , 1:3] <- y
    }

    # transform corners to viewport cs
    corners_trans <- coordinates$transform(corners, panel_params)
    x_rng <- range(corners_trans$x, na.rm = TRUE)
    y_rng <- range(corners_trans$y, na.rm = TRUE)

    # return raster grob of the img
    grid::rasterGrob(
      img,
      x_rng[1], y_rng[1],
      diff(x_rng), diff(y_rng), default.units = "native",
      just = c("left","bottom"), interpolate = interpolate
    )
  }
)


# the CMD check doesn't detect the call to
# rosm in the ggproto object and complains
rosm_image <- function(...) {
  rosm::osm.image(...)
}

rosm_raster <- function(...) {
  rosm::osm.raster(...)
}


#' keep values between limits
#'
#' ... and set NAs to 0
#'
#' @param x (numeric vector)
#' @param lower (numeric)
#' @param upper (numeric)
#' @param na_value (numeric)
#'
#' @return numeric vector
#'
#' @noRd
#'
clamp <- function(x, lower, upper, na_value = 0) {

  # reset any NAs to finite
  x[is.na(x)] <- na_value

  # enforce limits
  x[x > upper] <- upper
  x[x < lower] <- lower

  x
}


#' adjust image for contrast, brightness and gamma
#'
#' Method as QGIS raster code
#'
#' source:
#' https://github.com/qgis/QGIS/blob/005a0ad0937b24e5a999a2962f473c2fc88032a2/src/core/raster/qgsbrightnesscontrastfilter.cpp
#'
#' @param img (raster)
#' @param a (numeric) alpha component ... per-pixel ... ignored
#' @param brightness_value (numeric) brightness, [-255 to 255]
#' @param contrast_value (numeric) contrast, [-100 to 100]
#' @param gamma_value (numeric) gamma, [0.1 to 10]
#'
#' @return (raster) adjusted img
#'
#' @noRd
#'
adjustColorComponents <- function(
  img, a = 1, brightness_value = 0, contrast_value = 0, gamma_value = 1
) {

  # brightness_value: [-255, 255] -> [ -1,  1]
  # contrast_factor:  [-100, 100] -> [  0,  4]
  # gamma_factor:     [ 0.1,  10] -> [0.1, 10]

  brightness_value <- brightness_value / 255.0
  contrast_factor <- ((contrast_value + 100.0) / 100.0) ^ 2
  gamma_factor <- 1.0 / gamma_value

  apply(
    img, c(1, 2), adjustColorComponent
    , a = a
    , brightness_value = brightness_value
    , contrast_factor = contrast_factor
    , gamma_factor = gamma_factor
  )
}


#' adjust image for contrast, brightness and gamma
#'
#' Method as QGIS raster code
#'
#' source:
#' https://github.com/qgis/QGIS/blob/005a0ad0937b24e5a999a2962f473c2fc88032a2/src/core/raster/qgsbrightnesscontrastfilter.cpp
#'
#' @param rgb (numeric, possible vector) [0 to 1]
#' @param a (numeric) alpha component ... per-pixel ... ignored
#' @param brightness_value (numeric) brightness, [-1 to 1]
#' @param contrast_factor (numeric) contrast factor, [0 to 4]
#' @param gamma_factor (numeric) gamma factor, [0.1 to 10]
#'
#' @return (raster) adjusted img
#'
#' @noRd
#'
adjustColorComponent <- function(
  rgb, a = 1, brightness_value = 0, contrast_factor = 1, gamma_factor = 1
) {

  if (contrast_factor != 1)
    rgb <- ((rgb - 0.5) * contrast_factor) + 0.5

  if (brightness_value != 0)
    rgb <- rgb + brightness_value

  if (gamma_factor != 1)
    rgb <- rgb ^ gamma_factor

  rgb <- clamp(rgb, 0, 1)

  rgb
}
