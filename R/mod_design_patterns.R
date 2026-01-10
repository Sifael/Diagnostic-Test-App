#' Design Patterns Module
#'
#' A module to display the design patterns and technical implementation details
#' of the application.
#'
#' @import shiny
#' @import bs4Dash
#' @export
mod_design_patterns_ui <- function(id) {
    ns <- NS(id)

    tagList(
        fluidRow(
            col_12(
                h2("App Info - Implementation Details"),
                br()
            )
        ),
        fluidRow(
            col_12(
                # 1. Technical Implementation
                bs4Card(
                    title = "Technical Implementation",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    h4("Technology Stack"),
                    tags$ul(
                        tags$li(strong("Core:"), " R Shiny"),
                        tags$li(strong("UI Framework:"), " bs4Dash (Bootstrap 4)"),
                        tags$li(strong("OOP System:"), " R6"),
                        tags$li(strong("Libraries:"), " DT (DataTables), ggplot2"),
                        tags$li(strong("Testing:"), " testthat")
                    )
                ),

                # 2. Features
                bs4Card(
                    title = "Key Features",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    tags$ul(
                        tags$li(strong("Dynamic Data Loading:"), " Support for CSV, Excel, and random data generation with validation."),
                        tags$li(strong("Flexible Cutoffs:"), " User-defined cutoffs with various relational operators."),
                        tags$li(strong("Metrics:"), "A collection of performance metrics Sensitivity, Specificity, PPV, NPV calculations. together with counts"),
                        tags$li(strong("Confidence Intervals:"), " Multiple methods available (Clopper-Pearson, Wald, etc.) with adjustable confidence levels."),
                        tags$li(strong("Table Outputs:"), " View Results Dynamically and download reports"),
                        tags$li(strong("Validation:"), " Robust data validation with immediate feedback.")
                    )
                ),

                # 3. Design Patterns
                bs4Card(
                    title = "Design Patterns",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    collapsible = TRUE,
                    h5("1. Module Pattern (Shiny Modules)"),
                    p("Encapsulate related UI and server logic into reusable components. Each major functional step (Upload, Cutoffs, etc.) is a separate module with its own namespace."),
                    h5("2. Factory Pattern"),
                    p("Used to define templates and modules as well as Metrics. "),
                    h5("3. R6 Class Pattern (Business Logic)"),
                    p("Encapsulate stateful business logic using OOP. Key classes include:"),
                    tags$ul(
                        tags$li(strong("DataClass:"), " Handles data validation and validation. Also stores the data"),
                        tags$li(strong("CIMetricsClass:"), " Manages contingency table construction and metric calculations."),
                        tags$li(strong("CIMethodsClass:"), " Implements various Confidence Interval calculation strategies (Clopper-Pearson, Wald, etc.)."),
                        tags$li(strong("PlotClass:"), " Handles the generation of visualizations.")
                    ),
                    br(),
                    h5("4. Strategy Pattern"),
                    p("Interchangeable computation of methods such as clooper-person and wald test. Also useful in metrics calculation that take in same values but different outcomes"),
                    h5("5. Observer Pattern"),
                    p("Native to Shiny's reactive graph. Automatically propagates changes from inputs to outputs.")
                ),

                # 4. Architecture
                bs4Card(
                    title = "User Flow",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    collapsible = TRUE,
                    h5("User Flow"),
                    tags$pre(
                        "
                ┌───────────────────────────┐
                │     User Uploads Data     │
                └─────────────┬─────────────┘
                              │
                              ▼
                ┌───────────────────────────┐
                │ Data Validation & Loading │
                │       (DataClass)         │
                └─────────────┬─────────────┘
                              │ Validated Data
                              ▼
                ┌───────────────────────────┐      ┌───────────────────────────┐
                │      Cutoff Module        │────► │ Method Assignment Module  │
                └─────────────┬─────────────┘      └─────────────┬─────────────┘
                              │                                  │
                              │ Configuration & Parameters       │
                              ▼                                  ▼
                ┌──────────────────────────────────────────────────────────────┐
                │                    Business Logic Layer                      │
                │                                                              │
                │  ┌───────────────┐                  ┌───────────────┐        │
                │  │   CIMetrics   │◄────────────────►│   CIMethods   │        │
                │  │    (Metric    │                  │  (Calculation │        │
                │  │  Calculation) │                  │   Strategies) │        │
                │  └──────┬────────┘                  └───────────────┘        │
                └─────────┼────────────────────────────────────────────────────┘
                          │ Analysis Results
                          ▼
                ┌───────────────────────────┐
                │     Output Generation     │
                │  ┌──────────┐ ┌────────┐  │
                │  │ Reports  │ │ Plots  │  │
                │  └──────────┘ └────────┘  │
                └───────────────────────────┘"
                    ),
                    h5("Class Structure Definitions"),
                    tags$pre(
                        "
                ┌──────────────────────────────────────────────────┐
                │                    DataClass                     │
                ├──────────────────────────────────────────────────┤
                │ + initialize(df)                                 │
                │ + generate_demo_data(n)                          │
                │ + read_csv(file) / read_excel(file)              │
                │ + validate_empty_file(file)                      │
                │ + read_validated_file(file)                      │
                │ + get_numeric_columns(df)                        │
                │ + (static) summarize_column(x)                   │
                │ + (static) summarize_dataset(df)                 │
                └──────────────────────────────────────────────────┘

                ┌──────────────────────────────────────────────────┐
                │          CIMetrics (BinaryContingency)           │
                ├──────────────────────────────────────────────────┤
                │ + initialize(ref_labels, test_labels)            │
                │ + get_basic_counts()                             │
                │ + get_contingency_values()                       │
                │ + get_contingency_matrix()                       │
                │ + get_contingency_counts_df()                    │
                │ + calculate_classification_metrics()             │
                │ + get_classification_metrics_df()                │
                │ + get_clean_summary(ref_info, test_info)         │
                └──────────────────────────────────────────────────┘

                ┌──────────────────────────────────────────────────┐
                │                    CIMethods                     │
                ├──────────────────────────────────────────────────┤
                │ + initialize(conf_level)                         │
                │ + calculate_ci(metrics_df, method)               │
                │   - validate_method(method)                      │
                │   - validate_dataframe(metrics_df)               │
                │   - compute_row_ci(x, n, alpha, method_fn)       │
                └──────────────────────────────────────────────────┘

                ┌──────────────────────────────────────────────────┐
                │                   PlotsClass                     │
                ├──────────────────────────────────────────────────┤
                │ + initialize(base_size)                          │
                │ + plot_distribution(df, var_name)                │
                │ + plot_numeric_boxplots(data)                    │
                │ + plot_comparison_boxplots(data, x_col, y_col)   │
                │ + plot_ci_ranges(df, conf_level)                 │
                └──────────────────────────────────────────────────┘"
                    )
                )
            )
        )
    )
}

#' Design Patterns Server Logic
#'
#' @export
mod_design_patterns_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Static content
    })
}

# Helper function for columns if not defined elsewhere
if (!exists("col_12")) {
    col_12 <- function(...) {
        column(width = 12, ...)
    }
}
