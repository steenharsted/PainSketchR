# CUMMULATIVE PLOTS
## Prepare the data
set.seed(1)

draw_my_group_pain <- function(
    .data,
    variables_to_plot,
    n_groups = c(2, 2),
    same_n_in_groups = TRUE,
    max_n_in_groups = 1000,
    txt = "pct", # Must be "pct" "num" or "both"
    point_size = 0.25,
    min_alpha = 0.1,
    write_n = TRUE,
    color_scale = "max",
    strips_in_markdown = TRUE, 
    save_plot = TRUE, # This is the fastest solution. Change to FALSE if you need to modify the plot further.
    save_name = "cum_pain_diagram.png",
    scale = 1.5, 
    plot_height = NA, 
    plot_width = NA, 
    paindrawing_coord_object = pd_coords,  # Name of the paindrawing object
    background_image_object = background_image 
    ) {
  
  # This function will create a grid of heat-map cumulative pain diagrams
  # The first argument is .data

  # Second argument is the variable(s) to plot (We can only choose 1 or 2)

  # We need to remove rows with NA is either VAR1 or VAR2
  NA_VAR1 <- .data %>%
    filter(is.na(!!sym(variables_to_plot[1]))) %>%
    count() %>%
    pull()
  NA_VAR2 <- .data %>%
    filter(is.na(!!sym(variables_to_plot[2]))) %>%
    count() %>%
    pull()

  if (NA_VAR1 > 0 | NA_VAR2 > 0) {
    message(paste0("Removing NA values. ", NA_VAR1, " removed from '", variables_to_plot[1], "', and ", NA_VAR2, " removed from '", variables_to_plot[2], "'"))
    .data <- .data %>%
      filter(!is.na(!!sym(variables_to_plot[1]))) %>%
      filter(!is.na(!!sym(variables_to_plot[2])))
  }


  VAR1 <- .data %>%
    dplyr::select(variables_to_plot[1]) %>%
    pull()
  VAR2 <- .data %>%
    dplyr::select(variables_to_plot[2]) %>%
    pull()

  # For each of the variables we choose to plot. We need to know how many groups each of the columns should be stratified into.
  # The argument must be supplied in a vector of length equal to variables_to_plot (e.g. c(2,2))
  

  # Stratify VAR1
  # If we have more groups than unique values in VAR1 we have an error
  if (VAR1 %>% unique() %>% length() < n_groups[1]) {
    stop(paste0("You have asked to split the variable '", variables_to_plot[1], "' into ", n_groups[1], " groups. There is only ", VAR1 %>% unique() %>% length(), " unique values present in '", variables_to_plot[1], "'. Please reduce the n_groups value."))
  }

  # If we are dealing with logicals, factors, ordered, or characters then we cant collapse groups automagically
  if (VAR1 %>% vctrs::vec_ptype_abbr() %in% c("lgl", "fct", "ord", "chr") &
    VAR1 %>%
      unique() %>%
      length() != n_groups[1]) {
    message(paste0("The variable '", variables_to_plot[1], "' is a ", VAR1 %>% vctrs::vec_ptype_abbr(), ". I cannot collapse this type of variable into fewer groups. Continuing with ", VAR1 %>% unique() %>% length(), " groups."))
    n_groups[1] <- VAR1 %>%
      unique() %>%
      length()
  }


  if (VAR1 %>% vctrs::vec_ptype_abbr() %in% c("lgl", "fct", "ord", "chr")) {
    .data <- .data %>%
      mutate(
        .VAR1_grp = VAR1,
        .after = variables_to_plot[1]
      )
  } else {
    .data <- .data %>%
      mutate(
        .VAR1_grp = my_quantile(VAR1, n_quant_groups = n_groups[1], txt = txt),
        .after = variables_to_plot[1]
      )
  }



  # Stratify VAR2
  # If we have more groups than unique values in VAR1 we have an error
  if (VAR2 %>% unique() %>% length() < n_groups[2]) {
    stop(paste0("You have asked to split the variable '", variables_to_plot[2], "' into ", n_groups[2], " groups. There is only ", VAR2 %>% unique() %>% length(), " unique values present in '", variables_to_plot[2], "'. Please reduce the n_groups value."))
  }

  # If we are dealing with logicals, factors, ordered, or characters then we cant collapse groups automagically
  if (VAR2 %>% vctrs::vec_ptype_abbr() %in% c("lgl", "fct", "ord", "chr") &
    VAR2 %>%
      unique() %>%
      length() != n_groups[2]) {
    message(paste0("The variable '", variables_to_plot[2], "' is a ", VAR2 %>% vctrs::vec_ptype_abbr(), ". I cannot collapse this type of variable into fewer groups. Continuing with ", VAR2 %>% unique() %>% length(), " groups."))
    n_groups[2] <- VAR2 %>%
      unique() %>%
      length()
  }


  if (VAR2 %>% vctrs::vec_ptype_abbr() %in% c("lgl", "fct", "ord", "chr")) {
    .data <- .data %>%
      mutate(
        .VAR2_grp = VAR2,
        .after = variables_to_plot[2]
      )
  } else {
    .data <- .data %>%
      mutate(
        .VAR2_grp = my_quantile(VAR2, n_quant_groups = n_groups[2], txt = txt),
        .after = variables_to_plot[2]
      )
  }




  if (same_n_in_groups) {
    min_group <- .data %>%
      count(.VAR1_grp, .VAR2_grp) %>%
      summarise(min(n, na.rm = TRUE)) %>%
      pull()
    min_group

    message(paste0("same_n_in_groups is set to TRUE. The smallest group is ", min_group))
    message(paste0("max_n_in_groups is to ", max_n_in_groups, ". Reducing all groups to match ", min(min_group, max_n_in_groups) ))
    
    if(txt != "pct" & (VAR1 %>% vctrs::vec_ptype_abbr() == "dbl" | VAR2 %>% vctrs::vec_ptype_abbr() == "dbl")){
      warning("\nWarning: Rows have likely been removed from the dataset (see message above).However, the displayed max and min values on strip labels were calculated from the original, full dataset. These values may not accurately reflect the range in the current, reduced dataset. \nBe aware that the max and min might represent values from excluded data.\n")

    }

    pd_df <- .data %>%
      dplyr::select(id, variables_to_plot[1], variables_to_plot[2], .VAR1_grp, .VAR2_grp) %>%
      group_by(.VAR1_grp, .VAR2_grp) %>% 
      sample_n(min(min_group, max_n_in_groups), replace = FALSE)
    
  } else {
        min_group <- .data %>%
      count(.VAR1_grp, .VAR2_grp) %>%
      summarise(min(n, na.rm = TRUE)) %>%
      pull()
    min_group
    
    
     if(txt != "pct" & (VAR1 %>% vctrs::vec_ptype_abbr() == "dbl" | VAR2 %>% vctrs::vec_ptype_abbr() == "dbl") & max_n_in_groups < min_group){
      warning("\nWarning: Rows have been removed from the dataset because max_n_in_groups is smaller than n in one or more of the stratified groups.\nHowever, the displayed max and min values on strip labels were calculated from the original, full dataset. These values may not accurately reflect the range in the current, reduced dataset. \nBe aware that the max and min might represent values from excluded data.\n")
    }
    
    pd_df <- .data %>%
      dplyr::select(id, variables_to_plot[1], variables_to_plot[2], .VAR1_grp, .VAR2_grp) %>%
      group_by(.VAR1_grp, .VAR2_grp) %>%
      mutate(row_id = sample(n())) %>%                 # Assign a unique random number to each row in the group
      filter(row_id <= min(max_n_in_groups, n())) %>%  # Keep rows where the assigned number is below max_n_groups or n() - whatever is smaller
      dplyr::select(-row_id)                                  # Remove the helper column
    
  
  }




  pd_df_and_coord_flipped <- paindrawing_coord_object %>% # Insert paindrawing coordinate object here
    filter(id %in% pd_df$id) %>%
    full_join(pd_df,
      multiple = "all",
      relationship = "many-to-many"
    )


  ### Using a scaled color

  plot_pct_flipped_reduced_data <-
    pd_df_and_coord_flipped %>%
    mutate(
      X_grp = as.integer(round(X / 10) * 10),
      Y_grp = as.integer(round(Y / 10) * 10)
    ) %>%
    group_by(.VAR1_grp, .VAR2_grp) %>%
    mutate(n_id_grp = length(unique(id))) %>%
    group_by(.VAR1_grp, .VAR2_grp, X_grp, Y_grp) %>%
    mutate(
      n_id_grp_X_Y = length(unique(id)),
      n = n(),
      pct = (n_id_grp_X_Y / n_id_grp)
    )
  
  low  <- plot_pct_flipped_reduced_data$pct %>% quantile(0.33, n.rm = TRUE, names = FALSE)
  high <- plot_pct_flipped_reduced_data$pct %>% quantile(0.66, n.rm = TRUE, names = FALSE)


  plot_pct_flipped_reduced <- plot_pct_flipped_reduced_data %>%
    # The plot!
    ggplot(aes(
      x = X, y = Y,
      color = pct
    )) +
    annotation_raster(as.raster(background_image_object), 0, 450, 0, 500) +
    
    # The point layers. Draw in three steps
    geom_jitter(aes(alpha = if_else((pct^2) / 2 < min_alpha, min_alpha, (pct^2) / 2)),
      data = plot_pct_flipped_reduced_data %>% filter(pct <= low),
      stroke = 0,
      size = point_size,
      width = 2,
      height = 2
    ) +
    
    geom_jitter(aes(alpha = if_else((pct^2) / 2 < min_alpha, min_alpha, (pct^2) / 2)),
      data = plot_pct_flipped_reduced_data %>% filter(pct > low, pct <= high),
      stroke = 0,
      size = point_size,
      width = 2,
      height = 2
    ) +    
    
    geom_jitter(aes(alpha = if_else((pct^2) / 2 < min_alpha, min_alpha, (pct^2) / 2)),
      data = plot_pct_flipped_reduced_data %>% filter(pct > high),
      stroke = 0,
      size = point_size,
      width = 2,
      height = 2
    ) +       
    
    facet_grid(
      rows = vars(.VAR1_grp),
      cols = vars(.VAR2_grp),
      switch = "y", 
      labeller = label_wrap_gen(width = 15, multi_line = TRUE)
    ) +
    
    coord_fixed(xlim = c(0, 450), ylim = c(0, 500))
  
  # This is the maximum value of the color scale of the heat plot
  if(color_scale == "max"){
    max = max(plot_pct_flipped_reduced_data$pct, na.rm = TRUE)
    }else{
    max = as.numeric(color_scale)
  }
  
  
plot_pct_flipped_reduced <- plot_pct_flipped_reduced +  
    scale_colour_viridis_c(
      option = "inferno", direction = -1,
      begin = 0,
      end = 1,
      labels = scales::percent,
      breaks = seq(0, max, by = 0.1),
      limits = c(0, max)
    ) +
    scale_alpha_identity() +
    guides(color = guide_colorbar(
      title = NULL,
      barheight = 0.5,
      barwidth = 20,
      nbin = 50
    )) +
    labs(
      subtitle = paste0(variables_to_plot[2]),
      y = paste0(variables_to_plot[1])
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.background = element_rect(fill = "white", color = "white"),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x.top = element_text(hjust = 0.5), # x axis title on top
      axis.title.x.bottom = element_blank(), # remove x axis title on bottom
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, vjust = 0.5)
    )

if(strips_in_markdown){
  plot_pct_flipped_reduced <- plot_pct_flipped_reduced +
    theme(
      strip.text.y.left = ggtext::element_markdown(angle = 45, vjust = 0.5),
      strip.text.x.top = ggtext::element_markdown(hjust = 0.5)
      )
  }else{
    plot_pct_flipped_reduced <- plot_pct_flipped_reduced +
      theme(
        strip.text.y.left = element_text(angle = 0, vjust = 0.5),
        strip.text.x.top = element_text(hjust = 0.5)
      )
    }
  
  if(write_n){
    sum_data <- pd_df %>%
      group_by(.VAR1_grp, .VAR2_grp) %>%
      count()
    
    plot_pct_flipped_reduced <- plot_pct_flipped_reduced+
     geom_text(aes(label = paste0("n=",n) ),
               color = "black",
               alpha = 1, 
               data = sum_data,
               hjust = 0.5,
               size = rel(2),
               x =200, y = 50) 
    
  }
  
  if(save_plot){
    ggsave(save_name,
    plot = plot_pct_flipped_reduced,
    dpi = "retina",
    scale = scale, 
    height = plot_height, 
    width = plot_width
  )
  }else{
    plot_pct_flipped_reduced
  }
}


