library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(DT)
library(jsonlite)

# ---- HCR function (verbatim from source app) -----------------------------
hcr_asymptotic_hillary_step_manual <- function(sbsbf0, params) {
  sbsbf0_min      <- unname(params["sbsbf0_min"])
  sbsbf0_step_min <- unname(params["sbsbf0_step_min"])
  sbsbf0_step_max <- unname(params["sbsbf0_step_max"])
  sbsbf0_max      <- unname(params["sbsbf0_max"])
  out_min         <- unname(params["out_min"])
  step_height     <- unname(params["step_height"])
  out_max         <- unname(params["out_max"])
  curve           <- unname(params["curve"])

  # left asymptotic branch
  z1 <- exp(-curve * sbsbf0_min)
  z2 <- exp(-curve * sbsbf0_step_min)
  p  <- (step_height / z2) - (out_min / z1)
  q  <- (1 - z2) / z2
  r  <- (1 - z1) / z1
  a  <- p / (q - r)
  b  <- (out_min - a * (1 - z1)) / z1

  y_left <- a - (a - b) * exp(-curve * sbsbf0)
  y_left <- pmin(pmax(out_min, y_left), step_height)

  # right threshold branch
  grad      <- (out_max - step_height) / (sbsbf0_max - sbsbf0_step_max)
  intercept <- step_height - grad * sbsbf0_step_max

  y_right <- grad * sbsbf0 + intercept
  y_right <- pmin(pmax(step_height, y_right), out_max)

  # combine
  y <- y_left
  y[sbsbf0 > sbsbf0_step_min] <- y_right[sbsbf0 > sbsbf0_step_min]
  y
}

# ---- Defaults ------------------------------------------------------------
default_params <- c(
  sbsbf0_min      = 0.37,
  sbsbf0_step_min = 0.94,
  sbsbf0_step_max = 1.29,
  sbsbf0_max      = 1.59,
  out_min         = 0.20,
  step_height     = 1.08526,
  out_max         = 1.29,
  curve           = 10
)
default_hcr_x <- 0.94

# ---- Color palette for guide lines ---------------------------------------
break_colors <- list(
  sbsbf0_min      = "#1f77b4",  # blue
  sbsbf0_step_min = "#2ca02c",  # green
  sbsbf0_step_max = "#9467bd",  # purple
  sbsbf0_max      = "#8c564b"   # brown
)
level_colors <- list(
  out_min     = "#e377c2",  # pink
  step_height = "#17becf",  # cyan
  out_max     = "#bcbd22"   # olive
)
selected_color <- "#ff7f0e"   # warm orange accent for the selected point

# ---- Tooltip helper ------------------------------------------------------
label_with_tip <- function(label_html, tip, swatch = NULL) {
  swatch_tag <- if (!is.null(swatch)) {
    tags$span(
      style = sprintf(
        "display:inline-block;width:10px;height:10px;border-radius:2px;
         background:%s;margin-right:6px;vertical-align:middle;", swatch
      )
    )
  } else NULL

  tags$span(
    swatch_tag,
    HTML(label_html),
    tags$span(
      `data-bs-toggle` = "tooltip",
      `data-bs-placement` = "right",
      `aria-label` = "Help",
      role = "img",
      title = tip,
      style = "margin-left: 6px; color: #2a6ebb; cursor: help;",
      HTML("&#9432;")
    )
  )
}

# ---- JS: Bootstrap 5 tooltips + clipboard handler ------------------------
# The clipboard handler reports success/failure back to R via
# input$clipboard_status so we can show an accurate toast.
tooltip_and_clipboard_js <- "
$(function () {
  function refreshTips() {
    document.querySelectorAll('[data-bs-toggle=\"tooltip\"]').forEach(function (el) {
      if (!el._tip && window.bootstrap && window.bootstrap.Tooltip) {
        el._tip = new bootstrap.Tooltip(el, {container: 'body'});
      }
    });
  }
  refreshTips();
  $(document).on('shiny:value shiny:inputchanged', refreshTips);
});

function _setStatus(ok, label) {
  if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue('clipboard_status',
      {ok: ok, label: label, t: Date.now()},
      {priority: 'event'});
  }
}

function _legacyCopy(text, label) {
  try {
    var ta = document.createElement('textarea');
    ta.value = text;
    ta.setAttribute('readonly', '');
    ta.style.position = 'fixed';
    ta.style.top = 0;
    ta.style.left = 0;
    ta.style.opacity = 0;
    document.body.appendChild(ta);
    ta.focus();
    ta.select();
    var ok = document.execCommand('copy');
    document.body.removeChild(ta);
    _setStatus(!!ok, label);
  } catch (e) {
    _setStatus(false, label);
  }
}

$(document).on('shiny:connected', function () {
  Shiny.addCustomMessageHandler('copy_to_clipboard', function(msg) {
    var text = (typeof msg === 'string') ? msg : msg.text;
    var label = (typeof msg === 'string') ? 'Text' : (msg.label || 'Text');
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(
        function ()  { _setStatus(true, label); },
        function () { _legacyCopy(text, label); }
      );
    } else {
      _legacyCopy(text, label);
    }
  });
});
"

# ==========================================================================
# UI
# ==========================================================================
ui <- function(request) {
  page_sidebar(
    title = "HCR Explorer: asymptotic Hillary step",
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2a6ebb"
    ),
    window_title = "HCR Explorer",

    # MathJax + JS go in <head> so they load before the rest of the body.
    tags$head(
      withMathJax(),
      tags$script(HTML(tooltip_and_clipboard_js))
    ),

    sidebar = sidebar(
      width = 340,
      title = "Parameters",

      accordion(
        id = "param_accordion",
        multiple = TRUE,
        # open = TRUE opens every panel on load
        open = TRUE,

        # ---- Selected point -------------------------------------------
        accordion_panel(
          "Selected point",
          icon = bs_icon("crosshair"),
          sliderInput("hcr_x",
            label = label_with_tip(
              "Current \\(SB/SB_{F=0}\\)",
              "Position on the x-axis where the selected point is drawn on the curve.",
              swatch = selected_color
            ),
            min = 0, max = 2, value = default_hcr_x, step = 0.01)
        ),

        # ---- Breakpoints (x-axis) -------------------------------------
        accordion_panel(
          "Breakpoints (x-axis)",
          icon = bs_icon("arrows-expand-vertical"),

          sliderInput("sbsbf0_min",
            label = label_with_tip(
              "Min \\(SB/SB_{F=0}\\)",
              "Lower x-axis breakpoint. Below this depletion the curve is clamped at Min output multiplier.",
              swatch = break_colors$sbsbf0_min
            ),
            min = 0, max = 2, value = unname(default_params["sbsbf0_min"]), step = 0.01),

          sliderInput("sbsbf0_step_min",
            label = label_with_tip(
              "Step start \\(SB/SB_{F=0}\\)",
              "End of the asymptotic left branch. Beyond this point the curve switches to the right threshold branch.",
              swatch = break_colors$sbsbf0_step_min
            ),
            min = 0, max = 2, value = unname(default_params["sbsbf0_step_min"]), step = 0.01),

          sliderInput("sbsbf0_step_max",
            label = label_with_tip(
              "Step end \\(SB/SB_{F=0}\\)",
              "Lower x of the upper linear ramp. Below this (but above Step start) the right branch is clamped at Step height.",
              swatch = break_colors$sbsbf0_step_max
            ),
            min = 0, max = 2, value = unname(default_params["sbsbf0_step_max"]), step = 0.01),

          sliderInput("sbsbf0_max",
            label = label_with_tip(
              "Max \\(SB/SB_{F=0}\\)",
              "Upper x-axis breakpoint. Above this depletion the curve is clamped at Max output multiplier.",
              swatch = break_colors$sbsbf0_max
            ),
            min = 0, max = 2, value = unname(default_params["sbsbf0_max"]), step = 0.01)
        ),

        # ---- Levels (y-axis) ------------------------------------------
        accordion_panel(
          "Levels (y-axis)",
          icon = bs_icon("arrows-expand"),

          sliderInput("out_min",
            label = label_with_tip(
              "Min output multiplier",
              "Output multiplier applied below the lower x-axis breakpoint.",
              swatch = level_colors$out_min
            ),
            min = 0, max = 2, value = unname(default_params["out_min"]), step = 0.01),

          sliderInput("step_height",
            label = label_with_tip(
              "Step height",
              "Output multiplier on the flat plateau between Step start and Step end.",
              swatch = level_colors$step_height
            ),
            min = 0, max = 2, value = unname(default_params["step_height"]), step = 0.01),

          sliderInput("out_max",
            label = label_with_tip(
              "Max output multiplier",
              "Output multiplier applied above the upper x-axis breakpoint.",
              swatch = level_colors$out_max
            ),
            min = 0, max = 2, value = unname(default_params["out_max"]), step = 0.01)
        ),

        # ---- Curve shape ----------------------------------------------
        accordion_panel(
          "Curve shape",
          icon = bs_icon("graph-up"),
          sliderInput("curve",
            label = label_with_tip(
              "Curvature (\\(\\kappa\\))",
              "Steepness of the asymptotic left branch. Larger values give a sharper rise from Min to Step height."
            ),
            min = 0.1, max = 50, value = unname(default_params["curve"]), step = 0.1)
        )
      ),

      # ---- Action buttons ---------------------------------------------
      tags$hr(),
      div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 8px;",
        actionButton("reset_defaults", "Reset",
                     icon = icon("rotate-left"),
                     class = "btn-outline-secondary btn-sm"),
        actionButton("copy_params", "Copy params",
                     icon = icon("copy"),
                     class = "btn-outline-primary btn-sm")
      ),
      div(
        style = "margin-top: 8px;",
        actionButton("share_link", "Copy share link",
                     icon = icon("link"), width = "100%",
                     class = "btn-primary btn-sm")
      )
    ),

    # ---- Main panel: tabs ---------------------------------------------
    navset_card_underline(
      id = "main_tabs",

      # ----- Plot tab --------------------------------------------------
      nav_panel(
        title = "Plot",
        icon  = bs_icon("graph-up-arrow"),

        # Validation message (shown only if breakpoints are out of order)
        uiOutput("validation_msg"),

        # Two value boxes for the current selected point
        layout_columns(
          fill = FALSE,
          col_widths = c(6, 6),
          value_box(
            title = HTML("Estimated relative SB/SB<sub>F=0</sub>"),
            value = textOutput("vb_x", inline = TRUE),
            showcase = bs_icon("crosshair"),
            theme = "primary"
          ),
          value_box(
            title = "Output multiplier",
            value = textOutput("vb_y", inline = TRUE),
            showcase = bs_icon("arrow-up-right"),
            theme = value_box_theme(bg = selected_color, fg = "white")
          )
        ),

        plotlyOutput("hcr_plot", height = "550px")
      ),

      # ----- Data tab --------------------------------------------------
      nav_panel(
        title = "Data",
        icon  = bs_icon("table"),

        div(
          class = "mb-3",
          style = "display: flex; align-items: flex-start;
                   justify-content: space-between; gap: 16px; flex-wrap: wrap;",
          p(class = "text-muted mb-0", style = "flex: 1 1 320px; min-width: 280px;",
            HTML("This table shows the harvest-control-rule output multiplier
                 evaluated on a coarse grid of <b>SB/SB<sub>F=0</sub></b> values
                 (0 to 2 in steps of 0.05) using the parameter set currently
                 selected in the sidebar. Adjust the sliders to update the
                 values; export the table with the buttons above it, or
                 download the parameter set on the right.")
          ),
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap;",
            downloadButton("download_params_json", "Parameters (JSON)",
                           class = "btn-outline-primary btn-sm"),
            downloadButton("download_params_r", "Parameters (R vector)",
                           class = "btn-outline-primary btn-sm")
          )
        ),

        DT::DTOutput("hcr_data_table")
      )
    )
  )
}

# ==========================================================================
# Server
# ==========================================================================
server <- function(input, output, session) {

  # -- Bookmarking: exclude action buttons from the URL state --------------
  setBookmarkExclude(c("reset_defaults", "copy_params", "share_link",
                       "main_tabs", "param_accordion",
                       "download_params_json", "download_params_r",
                       "clipboard_status"))

  # -- Bundle current sliders into the named vector the HCR fn expects ----
  hcr_params <- reactive({
    c(
      sbsbf0_min      = input$sbsbf0_min,
      sbsbf0_step_min = input$sbsbf0_step_min,
      sbsbf0_step_max = input$sbsbf0_step_max,
      sbsbf0_max      = input$sbsbf0_max,
      out_min         = input$out_min,
      step_height     = input$step_height,
      out_max         = input$out_max,
      curve           = input$curve
    )
  })

  # -- Breakpoint validation ---------------------------------------------
  observeEvent(input$sbsbf0_min, {
    if (!is.null(input$sbsbf0_step_min) && input$sbsbf0_min >= input$sbsbf0_step_min) {
      updateSliderInput(session, "sbsbf0_step_min",
                        value = min(2, input$sbsbf0_min + 0.01))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$sbsbf0_step_min, {
    if (!is.null(input$sbsbf0_min) && input$sbsbf0_step_min <= input$sbsbf0_min) {
      updateSliderInput(session, "sbsbf0_min",
                        value = max(0, input$sbsbf0_step_min - 0.01))
    }
    if (!is.null(input$sbsbf0_step_max) && input$sbsbf0_step_min >= input$sbsbf0_step_max) {
      updateSliderInput(session, "sbsbf0_step_max",
                        value = min(2, input$sbsbf0_step_min + 0.01))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$sbsbf0_step_max, {
    if (!is.null(input$sbsbf0_step_min) && input$sbsbf0_step_max <= input$sbsbf0_step_min) {
      updateSliderInput(session, "sbsbf0_step_min",
                        value = max(0, input$sbsbf0_step_max - 0.01))
    }
    if (!is.null(input$sbsbf0_max) && input$sbsbf0_step_max >= input$sbsbf0_max) {
      updateSliderInput(session, "sbsbf0_max",
                        value = min(2, input$sbsbf0_step_max + 0.01))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$sbsbf0_max, {
    if (!is.null(input$sbsbf0_step_max) && input$sbsbf0_max <= input$sbsbf0_step_max) {
      updateSliderInput(session, "sbsbf0_step_max",
                        value = max(0, input$sbsbf0_max - 0.01))
    }
  }, ignoreInit = TRUE)

  output$validation_msg <- renderUI({
    p <- hcr_params()
    bp <- c(p["sbsbf0_min"], p["sbsbf0_step_min"],
            p["sbsbf0_step_max"], p["sbsbf0_max"])
    if (any(diff(bp) <= 0)) {
      div(class = "alert alert-warning py-2 mb-2",
          icon("triangle-exclamation"),
          " Breakpoints should satisfy Min < Step start < Step end < Max.")
    }
  })

  # -- Reactive curves ----------------------------------------------------
  hcr_curve_plot <- reactive({
    x <- seq(0, 2, length.out = 1000)
    y <- hcr_asymptotic_hillary_step_manual(x, hcr_params())
    data.frame(sbsbf0 = x, catch_multiplier = y)
  })

  hcr_curve_table <- reactive({
    x <- seq(0, 2, by = 0.05)
    y <- hcr_asymptotic_hillary_step_manual(x, hcr_params())
    data.frame(sbsbf0 = x, catch_multiplier = y)
  })

  selected_hcr_point <- reactive({
    x0 <- input$hcr_x
    y0 <- hcr_asymptotic_hillary_step_manual(x0, hcr_params())
    data.frame(x = x0, y = y0)
  })

  # -- Value-box outputs --------------------------------------------------
  output$vb_x <- renderText(sprintf("%.2f", selected_hcr_point()$x))
  output$vb_y <- renderText(sprintf("%.2f", selected_hcr_point()$y))

  # -- Plot ---------------------------------------------------------------
  output$hcr_plot <- renderPlotly({
    curve_df <- hcr_curve_plot()
    pt       <- selected_hcr_point()
    params   <- hcr_params()

    break_names <- c("sbsbf0_min", "sbsbf0_step_min",
                     "sbsbf0_step_max", "sbsbf0_max")
    vlines <- lapply(break_names, function(nm) list(
      type = "line", x0 = params[nm], x1 = params[nm],
      y0 = 0, y1 = 2,
      line = list(color = break_colors[[nm]], width = 1.5, dash = "dash")
    ))

    level_names <- c("out_min", "step_height", "out_max")
    hlines <- lapply(level_names, function(nm) list(
      type = "line", x0 = 0, x1 = 2,
      y0 = params[nm], y1 = params[nm],
      line = list(color = level_colors[[nm]], width = 1.5, dash = "dot")
    ))

    break_labels <- c(sbsbf0_min = "Min", sbsbf0_step_min = "Step start",
                      sbsbf0_step_max = "Step end", sbsbf0_max = "Max")
    level_labels <- c(out_min = "Min", step_height = "Step height",
                      out_max = "Max")

    vline_annos <- lapply(break_names, function(nm) list(
      x = params[[nm]], y = 1.97, xref = "x", yref = "y",
      text = break_labels[[nm]], showarrow = FALSE,
      font = list(size = 11, color = break_colors[[nm]]),
      bgcolor = "rgba(255,255,255,0.75)", borderpad = 2
    ))
    hline_annos <- lapply(level_names, function(nm) list(
      x = 1.97, y = params[[nm]], xref = "x", yref = "y",
      text = level_labels[[nm]], showarrow = FALSE,
      xanchor = "right",
      font = list(size = 11, color = level_colors[[nm]]),
      bgcolor = "rgba(255,255,255,0.75)", borderpad = 2
    ))

    selected_anno <- list(
      x = pt$x, y = pt$y, xref = "x", yref = "y",
      text = sprintf("(%.2f, %.2f)", pt$x, pt$y),
      showarrow = FALSE, yshift = 18,
      font = list(size = 14, color = selected_color)
    )

    plot_ly() |>
      add_lines(
        data = curve_df, x = ~sbsbf0, y = ~catch_multiplier,
        line = list(color = "black", width = 2),
        name = "HCR curve",
        hovertemplate = paste0(
          "SB/SBF=0: %{x:.3f}<br>",
          "Output multiplier: %{y:.3f}<extra></extra>"
        )
      ) |>
      add_markers(
        data = pt, x = ~x, y = ~y,
        marker = list(color = selected_color, size = 14,
                      line = list(color = "white", width = 2)),
        name = "Selected point",
        hovertemplate = paste0(
          "SB/SBF=0: %{x:.3f}<br>",
          "Output multiplier: %{y:.3f}<extra></extra>"
        )
      ) |>
      layout(
        xaxis = list(title = "Estimated relative SB/SB<sub>F=0</sub>",
                     range = c(0, 2), zeroline = FALSE),
        yaxis = list(title = "Output multiplier",
                     range = c(0, 2), zeroline = FALSE),
        shapes = c(vlines, hlines),
        annotations = c(vline_annos, hline_annos, list(selected_anno)),
        showlegend = FALSE,
        margin = list(t = 20, r = 20, b = 60, l = 60)
      ) |>
      config(displaylogo = FALSE)
  })

  # -- Data table ---------------------------------------------------------
  output$hcr_data_table <- DT::renderDT({
    dat <- hcr_curve_table()
    names(dat) <- c("Estimated relative SB/SBF=0", "Output multiplier")

    DT::datatable(
      dat,
      extensions = "Buttons",
      rownames   = FALSE,
      options    = list(
        dom        = "Bfrtip",
        buttons    = list(
          list(extend = "copy",  text = "Copy",  title = "HCR curve values"),
          list(extend = "csv",   text = "CSV",   filename = "hcr_curve_values"),
          list(extend = "excel", text = "Excel", filename = "hcr_curve_values",
               title = "HCR curve values"),
          list(extend = "pdf",   text = "PDF",   filename = "hcr_curve_values",
               title = "HCR curve values", orientation = "landscape")
        ),
        pageLength = 25,
        ordering   = TRUE,
        searching  = TRUE,
        info       = TRUE,
        scrollX    = TRUE
      ),
      class = "stripe hover compact"
    ) |>
      DT::formatRound(columns = 1:2, digits = 4) |>
      DT::formatStyle(1:2, `text-align` = "right")
  }, server = FALSE)

  # -- Parameter downloads -----------------------------------------------
  output$download_params_json <- downloadHandler(
    filename = function() {
      sprintf("hcr_parameters_%s.json", format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      p <- as.list(hcr_params())
      p$selected_sbsbf0 <- input$hcr_x
      p$selected_output_multiplier <-
        hcr_asymptotic_hillary_step_manual(input$hcr_x, hcr_params())
      writeLines(jsonlite::toJSON(p, auto_unbox = TRUE, pretty = TRUE), file)
    }
  )

  output$download_params_r <- downloadHandler(
    filename = function() {
      sprintf("hcr_parameters_%s.R", format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      p <- hcr_params()
      lines <- c(
        "# HCR parameter set exported from HCR Explorer",
        sprintf("# Generated: %s", Sys.time()),
        "",
        "hcr_params <- c(",
        paste0("  ", names(p), " = ", format(p, trim = TRUE),
               c(rep(",", length(p) - 1), "")),
        ")",
        "",
        sprintf("selected_sbsbf0 <- %s", format(input$hcr_x, trim = TRUE))
      )
      writeLines(lines, file)
    }
  )

  # -- Reset defaults ----------------------------------------------------
  observeEvent(input$reset_defaults, {
    updateSliderInput(session, "hcr_x", value = default_hcr_x)
    for (nm in names(default_params)) {
      updateSliderInput(session, nm, value = unname(default_params[nm]))
    }
  })

  # -- Copy parameters to clipboard --------------------------------------
  observeEvent(input$copy_params, {
    p <- hcr_params()
    nm_w <- max(nchar(names(p)))
    lines <- vapply(seq_along(p), function(i) {
      sprintf("%-*s = %s", nm_w, names(p)[i], format(p[[i]], trim = TRUE))
    }, character(1))
    text <- paste(lines, collapse = "\n")
    session$sendCustomMessage("copy_to_clipboard",
                              list(text = text, label = "Parameters"))
  })

  # -- Bookmark / share link ---------------------------------------------
  observeEvent(input$share_link, {
    session$doBookmark()
  })

  onBookmarked(function(url) {
    session$sendCustomMessage("copy_to_clipboard",
                              list(text = url, label = "Share link"))
  })

  # -- Clipboard status reporter -----------------------------------------
  # The JS handler sets input$clipboard_status with {ok, label, t} after
  # the copy attempt actually completes (or fails). t is a timestamp so
  # the same outcome twice in a row still triggers a notification.
  observeEvent(input$clipboard_status, {
    s <- input$clipboard_status
    if (isTRUE(s$ok)) {
      showNotification(sprintf("%s copied to clipboard.", s$label),
                       type = "message", duration = 3)
    } else {
      showNotification(
        paste(s$label, "could not be copied to the clipboard.",
              "Your browser may block clipboard access on insecure (http://) pages."),
        type = "warning", duration = 6)
    }
  })
}

# ---- Run -----------------------------------------------------------------
shinyApp(ui, server, enableBookmarking = "url")