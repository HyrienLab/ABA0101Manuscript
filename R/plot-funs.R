
wrap_legend_plot <- function(legend_grob) {
  ggplot() +
    theme_void() +
    annotation_custom(grob = legend_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    coord_cartesian(clip = "off")  # ensure grob isn't clipped
}

.mean_plots_boiler = function(route){
  list(geom_errorbar(), geom_point(), geom_line(), 
       scale_x_continuous(breaks = 0:13 * 7), 
       scale_color_manual(values = groups_colors_altname),
       guides(color = guide_legend(ncol = 1)),
       theme(legend.position = "bottom", panel.grid.minor.x = element_blank(),
             legend.byrow = TRUE, text = element_text(size = 11),
             axis.text.x = element_text(size = 8),
             legend.text = element_text(size = 7),
             axis.title.y = element_text(size = 10),
             #legend.key.height = unit(0.3, "lines"),
             #strip.background = element_blank(), 
             strip.text = element_text(hjust = 0.5, size = 11)),
       labs( x = "Visit day", color = paste(route, "Group")))
}

.draw_vpc_plot <- function(input_pk_df, subgroups, pop_mean, pop_int, yvar = "L",
                           time_trunc = 40, lloq_ug = .2){
  
  # observed concentrations
  vpc_point_data <- input_pk_df %>%
    filter(group %in% subgroups, day <= time_trunc, day >= 0) %>%
    mutate(
      group_label = glue("{group}: {dose} {dose_units}")
    ) %>%
    rename(time = day)

  interval_data <- pop_int %>%
    filter(group %in% subgroups) %>%
    filter(time <= time_trunc & time >= 0) %>%
    mutate(group_label = simplify_group_label(group_label)) %>%
    mutate_at(vars(`5%`, `50%`, `95%`), ~if_else(. < lloq_ug, lloq_ug, .))
  
  base_plot <- ggplot(data = interval_data, aes(x = time, y = `50%`)) +
    geom_ribbon(aes(x = time, ymin = `5%`, ymax = `95%`), fill = "gray", alpha = 0.75) +
    geom_line() +
    scale_x_continuous(limits = c(0, 40)) +
    labs(x = "Days post-administration")
  
  if (yvar == "L") {
    vpc_point_data <- vpc_point_data %>%
      filter(!is.na(concentration_ug)) %>%
      mutate(
        censored = if_else(concentration_ug <= lloq_ug, 1, 0),
      )
    
    myplot <- base_plot +
      geom_point(
        data = vpc_point_data,
        aes(x = time, y = concentration_ug, shape = factor(censored), color = group_label),
        alpha = .6
      ) +
      scale_y_log10(
        breaks = c(.2, 1, 10, 100, 1000),
        labels = c(as.expression(bquote("" <= .(lloq_ug))), 1, 10, 100, 1000),
        limits = c(lloq_ug, 5000)
      ) +
      geom_hline(aes(yintercept = lloq_ug), linetype = "dotted") +
      scale_shape_manual(values = c(16, 1)) +
      labs(y = conc_title)
  }
  
  if (yvar == "PT") {
    vpc_point_data <- filter(vpc_point_data, !is.na(cd4ro))
    
    myplot <- base_plot +
      geom_point(
        data = vpc_point_data, aes(x = time, y = cd4ro_trunc, color = group_label), alpha = .6
      ) +
      labs(y = "% CD4RO")
  }
  
  myplot +
    scale_color_manual(values = groups_colors_altname) +
    theme(legend.position = "none", axis.title.y = element_text(size = 10))
}
