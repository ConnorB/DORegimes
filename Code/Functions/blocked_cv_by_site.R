blocked_cv_by_site <- function(data, site, v = 5) {
  blocks <- data |>
    dplyr::mutate(.row = row_number()) |>
    dplyr::group_by({{ site }}) |>
    dplyr::mutate(.block = ntile(.row, v)) |>
    dplyr::ungroup() |>
    dplyr::pull(.block)

  splits <- purrr::map(seq_len(v), function(i) {
    rsample::make_splits(
      list(analysis = which(blocks != i), assessment = which(blocks == i)),
      data = data
    )
  })
  rsample::manual_rset(splits, paste0("Block", seq_len(v)))
}
