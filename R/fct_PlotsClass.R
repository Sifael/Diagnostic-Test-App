library(ggplot2)
library(R6)


PlotsClass <- R6::R6Class(
  "PlotManager",

  public = list(
    theme = NULL,
    # initialize the theme
    initialize = function(base_size = 12) {
      self$theme <- list(
        theme_minimal(base_size = base_size),
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          strip.text = element_text(face = "bold"),
          legend.position = "right"
        )
      )
    },

    # plot individual distribution of a singlge variable
    plot_distribution = function(df, var_name) {
      req(df, var_name)
      if (!var_name %in% names(df)) {
        return(NULL)
      }

      ggplot(df, aes(x = .data[[var_name]])) +
        geom_histogram(
          bins = 30,
          fill = "steelblue",
          color = "black",
          alpha = 0.7
        ) +
        self$theme +
        labs(
          title = glue::glue("Histogram of {var_name}"),
          x = var_name,
          y = "Count"
        )
    },

    # plot function for general visualization of numerical values
    plot_numeric_boxplots = function(data) {
      req(data)
      numeric_long <- data |>
        dplyr::select(dplyr::where(is.numeric)) |>
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = "variable",
          values_to = "value"
        )

      ggplot(numeric_long, aes(x = variable, y = value)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7) +
        self$theme +
        labs(
          title = "Boxplots of Numeric Variables",
          y = "Unstandardized Values",
          x = "Variable"
        )
    },

    plot_comparison_boxplots = function(data, x_col, y_col) {
      req(data, x_col, y_col)

      # using ggplot opinionated pivot
      numeric_long <- data |>
        dplyr::select(c(x_col, y_col)) |>
        tidyr::pivot_longer(
          cols = everything(),
          names_to = "variable",
          values_to = "value"
        )

      ggplot(
        numeric_long,
        aes(x = variable, y = value)
      ) +
        geom_boxplot() +
        self$theme +
        labs(
          title = paste("Comparison of", y_col, "by", x_col)
        )
    },

    plot_ci_ranges = function(df, conf_level) {
      required_cols <- c("Metric", "Value", "Lower_CI", "Upper_CI")
      if (!all(required_cols %in% names(df))) {
        stop("Missing columns for CI plot")
      }

      ggplot(df, aes(x = Value, y = stats::reorder(Metric, Value))) +
        geom_point(aes(color = Metric), size = 3) +
        geom_errorbarh(
          aes(xmin = Lower_CI, xmax = Upper_CI),
          height = 0.3,
          color = "grey40"
        ) +
        scale_x_continuous(limits = c(0, 1), expand = c(0.02, 0.02)) +
        self$theme +
        theme(
          legend.position = "none",
          panel.grid.major.y = element_blank()
        ) +
        labs(
          title = glue::glue("Metric Estimates with {conf_level*100}% CI"),
          x = "Estimate",
          y = "Metrics"
        )
    }
  )
)
