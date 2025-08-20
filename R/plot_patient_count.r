#' Plot Patient Count
#'
#' This function generates a plot of unique patient counts from APAC data based on the specified parameters.
#'
#' @param data DataFrame. A data frame containing the APAC data.
#' @param method Character vector. The method, either 'APAC' or 'AIH'.
#' @param y_type Character string. The type of y-axis, either 'number' or 'percentage'. Default is 'number'.
#' @param group Character string (optional). The grouping variable for the data.
#' @param first_year Integer (optional). The first year to include in the plot.
#' @param last_year Integer (optional). The last year to include in the plot.
#' @param legend_title Character string (optional). The title of the legend.
#' @param legend_labels Character vector (optional). The labels for the legend.
#' @param include_label Logical. Whether to include labels on the plot. Default is `TRUE`.
#' @param age_group Character string. Specifies the age group, options are 'all', 'adult', 'young_adult', or 'pediatric'. Default is 'all'.
#' @param age_type Character string. The type of age, either 'age_diagnosis' or 'age_attendance'. Default is 'age_diagnosis'.
#' @param ylim Numeric vector (optional). The y-axis limits.
#' @param y_break Numeric (optional). The interval between y-axis breaks.
#' @param ylim_adjust Numeric. Adjustment for the y-axis limits. Default is `0`.
#' @param ny Numeric vector (optional). Positions for y-axis labels.
#' @param nx Numeric vector (optional). Positions for x-axis labels.
#' @param line_size Numeric. The size of the line. Default is `1`.
#' @param point_size Numeric. The size of the points. Default is `1.5`.
#' @param text_color Character string. The color of the text labels. Default is 'black'.
#' @param line_color Character string (optional). The color of the line.
#' @param point_color Character string (optional). The color of the points.
#'
#' @details
#' - The plot is created using `ggplot2` and includes customizable options for age groups, years, grouping variables, and label positions.
#' - The y-axis can represent either the count (`number`) or percentage (`percentage`) of patients.
#' - Labels can be positioned using `nx` and `ny` for finer control over their placement.
#' - The method (`APAC` or `AIH`) determines specific settings for columns and grouping.
#'
#' @return A `ggplot` object representing the plot of patient counts.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example of plotting patient count
#' plot <- plot_patient_count(
#'   data = patient_data,
#'   method = "APAC",
#'   y_type = "number",
#'   group = "AQ_ESTADI",
#'   first_year = 2008,
#'   last_year = 2018,
#'   include_label = TRUE,
#'   ny = c(1, 0, 1, 1, 1, 1),
#'   ylim = c(0, 2500),
#'   y_break = 500
#' )
#' print(plot)
#' }
plot_patient_count <- function(
  data,
  method = c("APAC", "AIH"),
  y_type = "number",
  group = NULL,
  first_year = NULL,
  last_year = NULL,
  legend_title = NULL,
  legend_labels = NULL,
  include_label = TRUE,
  age_group = "all",
  age_type = "age_diagnosis",
  ylim = NULL,
  y_break = NULL,
  ylim_adjust = 0,
  ny = NULL,
  nx = NULL,
  line_size = 1,
  point_size = 1.5,
  text_color = "black"
) {
  names(data) <- tolower(names(data))

  # Age parameters

  age_params <- list(
    all = list(
      breaks = c(-1, 17, 29, 49, 69, 99),
      labels = c("0-17", "18-29", "30-49", "50-69", "70-99"),
      min_age = 0,
      max_age = 99
    ),
    adult = list(
      breaks = c(17, 29, 49, 69, 99),
      labels = c("18-29", "30-49", "50-69", "70-99"),
      min_age = 18,
      max_age = 99
    ),
    young_adult = list(
      breaks = c(14, 19, 24, 29, 34, 39),
      labels = c("15-19", "20-24", "25-29", "30-34", "35-39"),
      min_age = 15,
      max_age = 39
    ),
    pediatric = list(
      breaks = c(-1, 1, 5, 11, 18),
      labels = c("0-1", "2-5", "6-11", "12-18"),
      min_age = 0,
      max_age = 18
    )
  )

  # Setting the column names according to the method

  age_col <- ifelse(
    age_type == "age_diagnosis",
    "idade_diagnostico",
    "idade_anos"
  )

  cod_mun <- ifelse(method == "APAC", "ap_munpcn", "munic_res")

  if (method == "APAC") {
    ano <- ifelse(
      age_type == "age_diagnosis",
      "ano_diagnostico",
      "ano_atendimento"
    )
    x_label <- ifelse(
      age_type == "age_diagnosis",
      "Year of diagnosis",
      "Year of attendance"
    )
  } else {
    ano <- "ano_internacao"
    x_label <- "Year of hospitalization"
  }

  # Validations for input parameters
  if (!y_type %in% c("number", "percentage"))
    stop("Error: y_type must be 'number' or 'percentage'")

  if (
    !is.null(first_year) &&
      !is.null(last_year) &&
      first_year >= last_year
  )
    stop("Error: first_year must be less than last_year")

  if (!method %in% c("APAC", "AIH"))
    stop("Error: method must be 'APAC' or 'AIH'")

  if (!age_group %in% c("all", "adult", "young_adult", "pediatric"))
    stop(
      "Error: age_group must be 'all', 'adult', 'young_adult', or 'pediatric'"
    )

  if (!age_type %in% c("age_diagnosis", "age_attendance"))
    stop("Error: age_type must be 'age_diagnosis' or 'age_attendance'")

  if (method == "AIH") {
    if (age_type == "age_diagnosis")
      stop("Error: age_type must be 'age_attendance' for AIH")

    if (
      any(
        c("estadi", "database") %in%
          group
      )
    )
      stop("Error: For AIH group cannot contain 'estadi' or 'database'")
  }

  if (!is.null(group)) {
    if (
      !group %in%
        c(
          "icd",
          "icd_group",
          "procedure",
          "database",
          "region",
          "sex",
          "estadi",
          "age_group"
        )
    )
      stop(
        "Error: Unknown group variable. It should be 'icd', 'icd_group', 'procedure', 'database",
        "region",
        "sex",
        "estadi",
        "age_group"
      )

    if (is.numeric(data[[group]]))
      stop("Error: group should be a factor or character, not numeric")

    if (y_type == "number") {
      if (
        !is.null(legend_labels) &&
          length(legend_labels) !=
            length(unique(data[[group]])) +
              1
      )
        stop(
          "Error: length of legend_labels should be equal to the number of levels in the group variable plus Total"
        )
    } else {
      if (
        !is.null(legend_labels) &&
          length(legend_labels) != length(unique(data[[group]]))
      )
        stop(
          "Error: length of legend_labels should be equal to the number of levels in the group variable"
        )
    }

    if (group == "sex") group <- "sexo" else if (group == "icd")
      group <- ifelse(method == "APAC", "cid_prim", "cid") else if (
      group == "procedure"
    )
      group <- ifelse(method == "APAC", "tratamento", "proc_rea") else if (
      group == "database"
    )
      group <- "Base" else if (group == "age_group") {
      data <- data %>%
        dplyr::mutate(
          age_cat = cut(
            .data[[age_col]],
            breaks = age_params[[age_group]]$breaks,
            labels = age_params[[age_group]]$labels
          )
        )
      group <- "age_cat"
    } else if (group == "region") {
      data <- data %>%
        dplyr::mutate(cod = substr(.data[[cod_mun]], 1, 2)) %>%
        merge(OncoDatasusR::ufs, by = "cod", all.x = TRUE)
    }
  }

  # Filter data and determine year range

  if (is.null(first_year)) first_year <- min(data[[ano]], na.rm = TRUE)

  if (is.null(last_year)) last_year <- max(data[[ano]], na.rm = TRUE)

  # Create summary tables
  APAC_plot <- data %>%
    dplyr::filter(
      .data[[age_col]] >= age_params[[age_group]]$min_age &
        .data[[age_col]] <= age_params[[age_group]]$max_age
    ) %>%
    dplyr::filter(
      .data[[ano]] >= first_year &
        .data[[ano]] <= last_year
    ) %>%
    {
      if (!is.null(group)) dplyr::group_by(., .data[[ano]], .data[[group]]) else
        dplyr::group_by(., .data[[ano]])
    } %>%
    dplyr::summarise(n = dplyr::n_distinct(cod_paciente), .groups = "drop") %>%
    dplyr::mutate(ano_x_value = as.numeric(.data[[ano]]))

  APAC_total <- APAC_plot %>%
    dplyr::group_by(., .data[[ano]]) %>%
    dplyr::summarise(total = sum(n), .groups = "drop")

  if (y_type == "number" && !is.null(group)) {
    # Add Total layer to the plot

    APAC_plot <- APAC_total %>%
      dplyr::mutate(!!rlang::sym(group) := "Total", n = total) %>%
      dplyr::select(all_of(c(ano, group, "n"))) %>%
      dplyr::bind_rows(APAC_plot) %>%
      dplyr::mutate(ano_x_value = as.numeric(.data[[ano]]))
  }

  # Adjust y values

  if (y_type == "number") {
    APAC_plot <- APAC_plot %>%
      dplyr::mutate(y_value = n)
    y_label <- "Number of unique patients"
  } else if (y_type == "percentage") {
    APAC_plot <- APAC_plot %>%
      dplyr::left_join(APAC_total, by = c(ano)) %>%
      dplyr::mutate(y_value = round((n / total) * 100, 1))
    y_label <- "% of unique patients"
  }

  # Create the ggplot object

  g_plot <- ggplot2::ggplot(
    APAC_plot,
    ggplot2::aes(x = ano_x_value, y = y_value)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        color = if (!is.null(group)) .data[[group]] else NULL,
        group = if (!is.null(group)) .data[[group]] else 1
      ),
      linewidth = line_size
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = if (!is.null(group)) .data[[group]] else NULL),
      size = point_size
    ) +
    ggplot2::theme_minimal() +
    {
      if (!is.null(group)) {
        ggplot2::scale_color_manual(
          values = setNames(
            (scales::hue_pal())(length(
              unique(APAC_plot[[group]])
            )),
            unique(APAC_plot[[group]])
          ),
          name = if (!is.null(legend_title)) legend_title else group,
          labels = if (!is.null(legend_labels)) legend_labels else
            sort(unique(APAC_plot[[group]]))
        )
      } else {
        ggplot2::scale_color_manual(values = NULL)
      }
    } +
    ggplot2::xlab(x_label) +
    ggplot2::ylab(y_label) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.line = ggplot2::element_line(color = "black")
    ) +
    ggplot2::scale_x_continuous(breaks = seq(first_year, last_year, by = 1))

  # Add labels if required
  if (include_label) {
    if (!is.null(group)) {
      # Obter os grupos únicos do conjunto de dados
      unique_groups <- unique(APAC_plot[[group]])

      # Inicializar nx e ny
      if (is.null(ny)) {
        ny <- rep(0, length(unique_groups)) # Inicializa com zeros
      } else if (length(ny) != length(unique_groups)) {
        stop("Error: The length of 'ny' does not match the number of groups.")
      }

      if (is.null(nx)) {
        nx <- rep(0, length(unique_groups)) # Inicializa com zeros
      } else if (length(nx) != length(unique_groups)) {
        stop("Error: The length of 'nx' does not match the number of groups.")
      }

      APAC_plot <- APAC_plot %>%
        dplyr::mutate(
          nudge_y = if (!is.null(group))
            ny[match(.data[[group]], unique(APAC_plot[[group]]))] else 0,
          nudge_x = if (!is.null(group))
            nx[match(.data[[group]], unique(APAC_plot[[group]]))] else 0
        )
    } else {
      # Sem grupos, apenas um deslocamento padrão
      nx <- if (is.null(nx)) 0 else nx
      ny <- if (is.null(ny)) 0 else ny

      APAC_plot <- APAC_plot %>%
        dplyr::mutate(nudge_y = ny, nudge_x = nx)
    }

    # Add labels to the plot with adjustments on the X and Y axes
    g_plot <- g_plot +
      ggplot2::geom_text(
        data = APAC_plot,
        mapping = ggplot2::aes(label = y_value),
        color = text_color,
        position = ggplot2::position_nudge(
          x = APAC_plot$nudge_x,
          y = APAC_plot$nudge_y
        )
      )
  }

  # Adjust y-axis
  if (!is.null(ylim)) {
    if (is.null(y_break)) {
      y_break <- if (y_type == "percentage") 20 else
        round(diff(range(APAC_plot$y_value)) / 5, 0)
    }
    g_plot <- g_plot +
      ggplot2::scale_y_continuous(
        breaks = seq(ylim[1], ylim[2], y_break),
        limits = c(ylim[1] + ylim_adjust, ylim[2])
      )
  }

  return(g_plot)
}
