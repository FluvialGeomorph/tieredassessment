#' Prepare Results slider state for update
#'
#' Captures the current slider values, computes the slider range used during
#' Results initialization, and clamps each value into that range.
#'
#' @param xs_pts A data frame-like object containing at least `Seq` and
#'   `Detrend_DEM_Z` columns.
#' @param pick_xs The selected cross-section identifier.
#' @param channel_elevation Current channel elevation slider value.
#' @param floodplain_elevation Current floodplain elevation slider value.
#'
#' @return A list with:
#'   - `rem_min`
#'   - `rem_max`
#'   - `channel_elevation_value`
#'   - `floodplain_elevation_value`
#' @noRd
prepare_results_slider_state <- function(
  xs_pts,
  pick_xs,
  channel_elevation,
  floodplain_elevation
) {
  stopifnot(!is.null(xs_pts))
  stopifnot("Seq" %in% names(xs_pts))
  stopifnot("Detrend_DEM_Z" %in% names(xs_pts))

  detrended_elevations <- xs_pts[["Detrend_DEM_Z"]]
  if (!is.numeric(detrended_elevations)) {
    stop(
      "`Detrend_DEM_Z` must be a numeric vector.",
      call. = FALSE
    )
  }

  selected_xs <- detrended_elevations[
    xs_pts[["Seq"]] == as.numeric(pick_xs)
  ]
  selected_xs <- selected_xs[is.finite(selected_xs)]
  if (length(selected_xs) == 0L) {
    stop(
      "Selected cross section has no finite `Detrend_DEM_Z` values.",
      call. = FALSE
    )
  }

  rem_min <- max(round(min(selected_xs), 1) + 0.1, 100)
  rem_max <- round(max(selected_xs), 0) - 1
  if (!is.finite(rem_min) ||
      !is.finite(rem_max) ||
      rem_min >= rem_max) {
    stop(
      "Selected cross section does not provide a valid Results slider range.",
      call. = FALSE
    )
  }

  list(
    rem_min = rem_min,
    rem_max = rem_max,
    channel_elevation_value = clamp_results_slider_value(
      channel_elevation,
      lower = rem_min,
      upper = rem_max,
      input_name = "channel_elevation"
    ),
    floodplain_elevation_value = clamp_results_slider_value(
      floodplain_elevation,
      lower = rem_min,
      upper = rem_max,
      input_name = "floodplain_elevation"
    )
  )
}

#' Clamp a Results slider value
#'
#' @param value Current slider value.
#' @param lower Computed lower slider bound.
#' @param upper Computed upper slider bound.
#' @param input_name Input name used in validation errors.
#'
#' @return One finite numeric value within `lower` and `upper`.
#' @noRd
clamp_results_slider_value <- function(
  value,
  lower,
  upper,
  input_name
) {
  if (!is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value)) {
    stop(
      "`", input_name, "` must be one finite numeric value.",
      call. = FALSE
    )
  }

  max(lower, min(upper, as.numeric(value)))
}

#' Prepare the Results cross-section selection
#'
#' Rebuilds the selector choices from cross sections with enough finite
#' detrended elevations to initialize the Results sliders, and preserves the
#' current selection when it is still usable.
#'
#' @param xs_pts A data frame-like object containing a `Seq` column.
#' @param pick_xs The currently selected cross-section identifier.
#'
#' @return A list containing numeric `choices`, one numeric `selected` value,
#'   and numeric `unavailable` identifiers omitted from the selector.
#' @noRd
prepare_results_cross_section_selection <- function(xs_pts, pick_xs) {
  stopifnot(!is.null(xs_pts))
  stopifnot("Seq" %in% names(xs_pts))
  stopifnot("Detrend_DEM_Z" %in% names(xs_pts))

  sequence_values <- suppressWarnings(as.numeric(xs_pts[["Seq"]]))
  detrended_elevations <- xs_pts[["Detrend_DEM_Z"]]
  if (!is.numeric(detrended_elevations)) {
    stop(
      "`Detrend_DEM_Z` must be a numeric vector.",
      call. = FALSE
    )
  }

  candidates <- sort(unique(sequence_values[is.finite(sequence_values)]))
  usable <- vapply(candidates, function(candidate) {
    values <- detrended_elevations[sequence_values == candidate]
    values <- values[is.finite(values)]
    if (length(values) == 0L) {
      return(FALSE)
    }

    rem_min <- max(round(min(values), 1) + 0.1, 100)
    rem_max <- round(max(values), 0) - 1
    is.finite(rem_min) && is.finite(rem_max) && rem_min < rem_max
  }, logical(1))

  choices <- candidates[usable]
  unavailable <- candidates[!usable]
  if (length(choices) == 0L) {
    stop(
      paste(
        "No cross section contains enough finite detrended terrain",
        "elevations to initialize Results. Adjust or redraw the cross",
        "sections and try again."
      ),
      call. = FALSE
    )
  }

  current <- suppressWarnings(as.numeric(pick_xs))
  selected <- if (
    length(current) == 1L &&
      is.finite(current) &&
      current %in% choices
  ) {
    current
  } else {
    choices[[1]]
  }

  list(
    choices = choices,
    selected = selected,
    unavailable = unavailable
  )
}

#' Prepare Results workflow state
#'
#' This helper captures the workflow values needed to transition the app into
#' the Results state in a testable way.
#'
#' @param xs_pts Data frame-like object with `Seq` and `Detrend_DEM_Z`.
#' @param pick_xs Selected cross-section identifier.
#' @param channel_elevation Current channel elevation slider value.
#' @param floodplain_elevation Current floodplain elevation slider value.
#'
#' @return A list containing:
#'   - `slider_state`: output from `prepare_results_slider_state()`
#'   - `cross_section_choices`: choices rebuilt from the current geometry
#'   - `pick_xs`: the preserved or fallback selection
#'   - `unavailable_cross_sections`: identifiers omitted from Results
#'   - `results_loaded`: always `TRUE` for a successful transition
#' @noRd
prepare_results_workflow_state <- function(
  xs_pts,
  pick_xs,
  channel_elevation,
  floodplain_elevation
) {
  cross_section_state <- prepare_results_cross_section_selection(
    xs_pts = xs_pts,
    pick_xs = pick_xs
  )

  slider_state <- prepare_results_slider_state(
    xs_pts = xs_pts,
    pick_xs = cross_section_state$selected,
    channel_elevation = channel_elevation,
    floodplain_elevation = floodplain_elevation
  )

  list(
    slider_state = slider_state,
    cross_section_choices = cross_section_state$choices,
    pick_xs = cross_section_state$selected,
    unavailable_cross_sections = cross_section_state$unavailable,
    results_loaded = TRUE
  )
}

#' Read the Results gate from a deferred Shiny callback
#'
#' Deferred callbacks such as `session$onFlushed()` do not run inside a
#' reactive consumer. Isolating the read supplies the required context without
#' creating a reactive dependency.
#'
#' @param results_loaded A Shiny reactive-value getter.
#'
#' @return One logical value.
#' @noRd
read_deferred_results_gate <- function(results_loaded) {
  if (!is.function(results_loaded)) {
    stop("results_loaded must be a reactive-value getter")
  }

  isTRUE(shiny::isolate(results_loaded()))
}

#' Run the Results workflow transition
#'
#' Prepares workflow state for Results, updates relevant slider inputs, and marks
#' Results readiness.
#'
#' @param session Shiny session object.
#' @param input Shiny input object.
#' @param xs_pts Data frame of cross-section points used to compute slider bounds.
#' @param set_results_loaded Optional function used to set the Results gate state.
#'   - When `NULL` (default), the function uses the internal `results_loaded`
#'     setter in the existing server flow.
#'   - When supplied, it must be a function accepting one logical argument and is
#'     used as an injectable seam for testing.
#'
#' @return A workflow state list with:
#' \describe{
#'   \item{slider_state}{List containing captured slider values and computed bounds.}
#'   \item{results_loaded}{Logical readiness flag for Results workflow completion.}
#' }
#'
#' @keywords internal
run_results_workflow_transition <- function(session,
                                            input,
                                            xs_pts,
                                            set_results_loaded = NULL) {
  workflow_state <- prepare_results_workflow_state(
    xs_pts = xs_pts,
    pick_xs = input$pick_xs,
    channel_elevation = input$channel_elevation,
    floodplain_elevation = input$floodplain_elevation
  )

  slider_state <- workflow_state$slider_state

  # Rebuild choices on every transition so edited geometry is represented.
  updateSelectInput(
    session = session,
    inputId = "pick_xs",
    choices = workflow_state$cross_section_choices,
    selected = workflow_state$pick_xs
  )

  # Values are clamped to the new cross-section-specific bounds before update.
  updateSliderInput(
    session = session,
    inputId = "channel_elevation",
    value = slider_state$channel_elevation_value,
    min = slider_state$rem_min,
    max = slider_state$rem_max
  )

  updateSliderInput(
    session = session,
    inputId = "floodplain_elevation",
    value = slider_state$floodplain_elevation_value,
    min = slider_state$rem_min,
    max = slider_state$rem_max
  )

  # Injectable seam for gate-setting (testability)
  if (is.function(set_results_loaded)) {
    set_results_loaded(workflow_state$results_loaded)
  } else {
    # Existing server behavior fallback
    results_loaded(workflow_state$results_loaded)
  }

  workflow_state
}

#' Prepare Draw XS workflow state
#'
#' Creates an explicit readiness contract for Draw XS transition logic.
#'
#' @param xs_pts Data frame of cross-section points.
#' @param pick_xs Selected cross-section index/value.
#'
#' @return A list containing:
#' \describe{
#'   \item{draw_xs_loaded}{Logical readiness flag for Draw XS workflow.}
#'   \item{pick_xs}{Normalized selected cross-section value.}
#' }
#' @keywords internal
prepare_draw_xs_workflow_state <- function(xs_pts, pick_xs) {
  ready <- !is.null(xs_pts) &&
    is.data.frame(xs_pts) &&
    nrow(xs_pts) > 0 &&
    !is.null(pick_xs) &&
    length(pick_xs) == 1 &&
    !is.na(pick_xs)

  list(
    draw_xs_loaded = isTRUE(ready),
    pick_xs = pick_xs
  )
}

#' Run Draw XS workflow transition
#'
#' Computes Draw XS workflow readiness and optionally propagates readiness
#' via an injectable gate setter for deterministic testing.
#'
#' @param session Shiny session object.
#' @param input Shiny input object.
#' @param xs_pts Data frame of cross-section points.
#' @param set_draw_xs_loaded Optional function accepting one logical argument.
#'   When NULL, default behavior is no-op unless app server wiring provides
#'   gate handling externally.
#'
#' @return A workflow state list with Draw XS readiness contract.
#' @keywords internal
run_draw_xs_workflow_transition <- function(session,
                                            input,
                                            xs_pts,
                                            set_draw_xs_loaded = NULL) {
  workflow_state <- prepare_draw_xs_workflow_state(
    xs_pts = xs_pts,
    pick_xs = input$pick_xs
  )

  if (is.function(set_draw_xs_loaded)) {
    set_draw_xs_loaded(workflow_state$draw_xs_loaded)
  }

  workflow_state
}
