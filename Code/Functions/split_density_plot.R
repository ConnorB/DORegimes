split_density_plot <- function(data, title, fold_col = NULL) {
  d <- data |>
    dplyr::select(
      role,
      dplyr::any_of(fold_col),
      dplyr::where(is.numeric)
    ) |>
    tidyr::pivot_longer(
      -c(role, dplyr::any_of(fold_col)),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::mutate(
      variable = dplyr::coalesce(var_names[variable], variable)
    )

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = value, fill = role)
  ) +
    ggplot2::geom_density(alpha = 0.5, na.rm = TRUE) +
    ggplot2::scale_fill_manual(
      values = c(Training = "#85898A", Testing = "#0051BA")
    ) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    )

  if (is.null(fold_col)) {
    p +
      ggplot2::facet_wrap(
        ggplot2::vars(variable),
        scales = "free"
      )
  } else {
    p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(rlang::.data[[fold_col]]),
        cols = ggplot2::vars(variable),
        scales = "free"
      )
  }
}
