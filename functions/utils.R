plot_colony_diff <- function(res, iter, which_colonies) {
  i = iter
  res[[i]] |> 
    filter(
      !is.na(colonyID)
    ) |> 
    mutate(
      colonyID = factor(
        colonyID,
        levels = c(
          intersect(
            which_colonies,
            unique(.data[["colonyID"]])
          )
        )
      )
    ) |> 
    mutate(
      sig_diff = if_else(
        pr_z < 0.05,
        "yes",
        "no"
      ) |> 
        factor(
          levels = c("no", "yes")
        ),
      ci = 1.96*std_error
    ) |> 
    ggplot(
      aes(
        x = colonyID,
        y = estimate
      )
    ) +
    geom_errorbar(
      aes(
        ymin = estimate - ci,
        ymax = estimate + ci
      )
    ) +
    geom_point(
      aes(
        fill = sig_diff
      ),
      shape = 21,
      size = 3
    ) +
    # facet_grid(
    #   ~ yr
    # ) +
    labs(
      title = paste0("reference colony: ", unique(res[[i]]$ref)),
      fill = "significant\ndiffernce?",
      y = "estimated diff (logit scale)",
      x = ""
    ) +
    geom_hline(
      yintercept = 0,
      alpha = .4
    ) +
    coord_flip() +
    scale_fill_manual(values = c("grey30", "orange")) +
    theme_bw(10)
}