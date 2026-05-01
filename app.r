library(shiny)
library(plotly)
library(DT)

# ---- HCR function (verbatim from south pacific albacore app) -------------
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

# ---- Defaults (from source app hcr_params reactive) ---------------------
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

# ---- Tooltip helper ------------------------------------------------------
# Wraps a label and an info icon with a Bootstrap tooltip. The label can
# be HTML (so MathJax inline math survives).
label_with_tip <- function(label_html, tip) {
  tags$span(
    HTML(label_html),
    tags$span(
      `data-toggle` = "tooltip",
      `data-placement` = "right",
      title = tip,
      style = "margin-left: 6px; color: #2a6ebb; cursor: help;",
      HTML("&#9432;")  # circled "i"
    )
  )
}

# Initialize Bootstrap tooltips on every Shiny redraw and set up the
# clipboard handler. Kept tiny and dependency-free.
tooltip_and_clipboard_js <- "
$(function () {
  function refreshTips() {
    $('[data-toggle=\"tooltip\"]').tooltip({container: 'body', html: false});
  }
  refreshTips();
  $(document).on('shiny:value', refreshTips);
  $(document).on('shiny:inputchanged', refreshTips);
});

Shiny.addCustomMessageHandler('copy_to_clipboard', function(text) {
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(text);
  } else {
    var ta = document.createElement('textarea');
    ta.value = text;
    ta.style.position = 'fixed';
    ta.style.opacity = 0;
    document.body.appendChild(ta);
    ta.select();
    try { document.execCommand('copy'); } catch (e) {}
    document.body.removeChild(ta);
  }
});
"

# ---- UI ------------------------------------------------------------------
ui <- fluidPage(
  withMathJax(),
  tags$head(tags$script(HTML(tooltip_and_clipboard_js))),

  titlePanel("HCR Explorer: asymptotic Hillary step"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      sliderInput("hcr_x",
                  label = label_with_tip(
                    "Current \\(SB/SB_{F=0}\\)",
                    "Position on the x-axis where the selected point is drawn on the curve."
                  ),
                  min = 0, max = 2, value = default_hcr_x, step = 0.01),

      tags$hr(style = "border: 0; height: 2px; background: #B3B3B3; margin: 18px 0;"),
      strong("Curve x-axis breakpoints"),

      sliderInput("sbsbf0_min",
                  label = label_with_tip(
                    "Min \\(SB/SB_{F=0}\\)",
                    "Lower x-axis breakpoint. Below this depletion the curve is clamped at Min output multiplier."
                  ),
                  min = 0, max = 2, value = unname(default_params["sbsbf0_min"]), step = 0.01),

      sliderInput("sbsbf0_step_min",
                  label = label_with_tip(
                    "Step start \\(SB/SB_{F=0}\\)",
                    "End of the asymptotic left branch. Beyond this point the curve switches to the right threshold branch."
                  ),
                  min = 0, max = 2, value = unname(default_params["sbsbf0_step_min"]), step = 0.01),

      sliderInput("sbsbf0_step_max",
                  label = label_with_tip(
                    "Step end \\(SB/SB_{F=0}\\)",
                    "Lower x of the upper linear ramp. Below this (but above Step start) the right branch is clamped at Step height."
                  ),
                  min = 0, max = 2, value = unname(default_params["sbsbf0_step_max"]), step = 0.01),

      sliderInput("sbsbf0_max",
                  label = label_with_tip(
                    "Max \\(SB/SB_{F=0}\\)",
                    "Upper x-axis breakpoint. At this depletion the right branch reaches Max output multiplier and is clamped above."
                  ),
                  min = 0, max = 2, value = unname(default_params["sbsbf0_max"]), step = 0.01),

      tags$hr(style = "border: 0; height: 2px; background: #B3B3B3; margin: 18px 0;"),
      strong("Curve y-axis levels"),

      sliderInput("out_min",
                  label = label_with_tip(
                    "Min output multiplier",
                    "Lower asymptote and floor of the left branch."
                  ),
                  min = 0, max = 2, value = unname(default_params["out_min"]), step = 0.01),

      sliderInput("step_height",
                  label = label_with_tip(
                    "Step height (output multiplier)",
                    "Plateau between Step start and Step end. Also the upper cap of the left branch and lower cap of the right branch."
                  ),
                  min = 0, max = 2, value = unname(default_params["step_height"]), step = 0.001),

      sliderInput("out_max",
                  label = label_with_tip(
                    "Max output multiplier",
                    "Upper asymptote and ceiling of the right branch (at and above Max SB/SB_F=0)."
                  ),
                  min = 0, max = 2, value = unname(default_params["out_max"]), step = 0.01),

      tags$hr(style = "border: 0; height: 2px; background: #B3B3B3; margin: 18px 0;"),
      strong("Curve shape"),

      sliderInput("curve",
                  label = label_with_tip(
                    "Curvature (\\(\\kappa\\))",
                    "Steepness of the asymptotic left branch. Larger values give a sharper rise from Min to Step height."
                  ),
                  min = 0.1, max = 50, value = unname(default_params["curve"]), step = 0.1),

      tags$hr(style = "border: 0; height: 2px; background: #B3B3B3; margin: 18px 0;"),
      fluidRow(
        column(6, actionButton("reset_defaults", "Reset defaults", width = "100%")),
        column(6, actionButton("copy_params", "Copy",
                               icon = icon("copy"), width = "100%"))
      )
    ),

    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Plot",
          tags$br(),
          wellPanel(
            style = "background-color:#f9f9f9; border:1px solid #d3d3d3; border-radius:8px;",
            tags$h4("Current HCR point", style = "margin-top:0;"),
            uiOutput("hcr_point_box")
          ),
          plotlyOutput("hcr_plot", height = "550px")
        ),
        tabPanel("Data",
          tags$br(),
          DT::DTOutput("hcr_data_table")
        )
      )
    )
  )
)

# ---- Server --------------------------------------------------------------
server <- function(input, output, session) {

  # Bundle current parameter inputs into the named vector the HCR fn expects
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

  # Dense grid for the plot (smooth curve)
  hcr_curve_plot <- reactive({
    x <- seq(0, 2, length.out = 1000)
    y <- hcr_asymptotic_hillary_step_manual(x, hcr_params())
    data.frame(sbsbf0 = x, catch_multiplier = y)
  })

  # Coarse 0.05 grid for the data tab
  hcr_curve_table <- reactive({
    x <- seq(0, 2, by = 0.05)
    y <- hcr_asymptotic_hillary_step_manual(x, hcr_params())
    data.frame(sbsbf0 = x, catch_multiplier = y)
  })

  # Currently selected point on the curve
  selected_hcr_point <- reactive({
    x0 <- input$hcr_x
    y0 <- hcr_asymptotic_hillary_step_manual(x0, hcr_params())
    data.frame(x = x0, y = y0)
  })

  # ----- "Current HCR point" box (inside Plot tab) ------------------------
  output$hcr_point_box <- renderUI({
    pt <- selected_hcr_point()
    tags$div(
      style = "font-size: 15px; line-height: 1.6;",
      tags$div(tags$b("Estimated relative SB/SBF=0: "),
               span(sprintf("%.2f", pt$x))),
      tags$div(tags$b("Output multiplier: "),
               span(sprintf("%.2f", pt$y)))
    )
  })

  # ----- Plot: native plotly mirror of source app's output$hcr_plot -------
  output$hcr_plot <- renderPlotly({
    curve_df <- hcr_curve_plot()
    pt       <- selected_hcr_point()
    params   <- hcr_params()

    vlines <- lapply(
      c("sbsbf0_min", "sbsbf0_step_min", "sbsbf0_step_max", "sbsbf0_max"),
      function(nm) list(
        type = "line", x0 = params[nm], x1 = params[nm],
        y0 = 0, y1 = 2,
        line = list(color = "gray40", width = 1, dash = "dash")
      )
    )
    hlines <- lapply(
      c("out_min", "step_height", "out_max"),
      function(nm) list(
        type = "line", x0 = 0, x1 = 2,
        y0 = params[nm], y1 = params[nm],
        line = list(color = "gray40", width = 1, dash = "dot")
      )
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
        marker = list(color = "#000080", size = 12),
        name = "Selected point",
        hovertemplate = paste0(
          "SB/SBF=0: %{x:.3f}<br>",
          "Output multiplier: %{y:.3f}<extra></extra>"
        )
      ) |>
      add_annotations(
        x = pt$x, y = pt$y,
        text = sprintf("(%.2f, %.2f)", pt$x, pt$y),
        showarrow = FALSE, yshift = 18,
        font = list(size = 14, color = "black")
      ) |>
      layout(
        title = list(text = "HCR: asymptotic Hillary step"),
        xaxis = list(title = "Estimated relative SB/SBF=0",
                     range = c(0, 2), zeroline = FALSE),
        yaxis = list(title = "Output multiplier",
                     range = c(0, 2), zeroline = FALSE),
        shapes = c(vlines, hlines),
        showlegend = FALSE,
        margin = list(t = 50)
      ) |>
      config(displaylogo = FALSE)
  })

  # ----- Data tab ---------------------------------------------------------
  output$hcr_data_table <- DT::renderDT({
    dat <- hcr_curve_table()
    names(dat) <- c("Estimated relative SB/SBF=0", "Output multiplier")

    DT::datatable(
      dat,
      extensions = "Buttons",
      caption    = "HCR curve values (SB/SBF=0 from 0 to 2 in steps of 0.05)",
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

  # ----- Reset defaults ---------------------------------------------------
  # Pass plain numerics (unname) so updateSliderInput doesn't choke on
  # named-vector elements.
  observeEvent(input$reset_defaults, {
    updateSliderInput(session, "hcr_x",           value = default_hcr_x)
    updateSliderInput(session, "sbsbf0_min",      value = unname(default_params["sbsbf0_min"]))
    updateSliderInput(session, "sbsbf0_step_min", value = unname(default_params["sbsbf0_step_min"]))
    updateSliderInput(session, "sbsbf0_step_max", value = unname(default_params["sbsbf0_step_max"]))
    updateSliderInput(session, "sbsbf0_max",      value = unname(default_params["sbsbf0_max"]))
    updateSliderInput(session, "out_min",         value = unname(default_params["out_min"]))
    updateSliderInput(session, "step_height",     value = unname(default_params["step_height"]))
    updateSliderInput(session, "out_max",         value = unname(default_params["out_max"]))
    updateSliderInput(session, "curve",           value = unname(default_params["curve"]))
  })

  # ----- Copy parameters to clipboard -------------------------------------
  observeEvent(input$copy_params, {
    p <- hcr_params()
    nm_w <- max(nchar(names(p)))
    lines <- vapply(seq_along(p), function(i) {
      sprintf("%-*s = %s", nm_w, names(p)[i], format(p[[i]], trim = TRUE))
    }, character(1))
    text <- paste(lines, collapse = "\n")

    session$sendCustomMessage("copy_to_clipboard", text)
    showNotification("HCR parameters copied to clipboard.",
                     type = "message", duration = 3)
  })
}

# ---- Run -----------------------------------------------------------------
shinyApp(ui, server)