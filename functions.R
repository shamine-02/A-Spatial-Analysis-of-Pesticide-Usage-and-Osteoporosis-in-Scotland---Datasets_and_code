# FUNCTION 1: finding total area of crop grown in each boundary
preparing_agri_for_plotting <- function(crop_data_from_digimap) {
  
  #convert polygons from oat data into centroids
  centroid_data <- st_centroid(crop_data_from_digimap)
  
 
  joined_centroid <- st_join(centroid_data, boundary_data, join = st_intersects)

  joined_centroid %>%
    st_drop_geometry() %>%
    select(ONS_2010, agcval) %>%
    right_join(boundary_data, join_by(ONS_2010 == ONS_2010)) %>%
    group_by(ONS_2010) %>%
    #filter(!is.na(agcval)) %>%
    summarise(ONS_2010, geometry, crop_area = sum(agcval)) %>% 
    group_by(ONS_2010) %>% 
    filter(row_number() == 1)
}

# FUNCTION 2: finding points where agricultural data couldn't be plotted
finding_missing_ONS <- function(crop_data_from_digimap) {
  
  crop_data_from_digimap %>% 
    st_centroid() %>% 
    st_join(boundary_data, join = st_intersects) %>% 
    filter(is.na(ONS_2010))
  
}

#FUNCTION 3: plotting the missing agricultural data points (use dataset produced from function 1 and 2)
plotting_agrcultural_w_missing_ONS <- function(prepared_crop_data, missing_ONS_crop_data) {
  
  prepared_crop_data %>% 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = crop_area), colour = NA) +
    #missing ONS all on the very borders of the map
    geom_sf(aes(geometry = geometry), data = missing_ONS_crop_data, colour = "coral", size = 0.4, show.legend = FALSE) + 
    labs(fill = NULL,
         colour = NULL) +
    coord_sf(datum = NULL) +
    theme_void() #+
    # theme(legend.justification = c(0,1),
    #       legend.position = c(0.1, 0.9))
  
}

#FUNCTION 3: plotting crop area grown within each boundary (use dataset produced by function 1)
plotting_agricultural_data <- function(prepared_crop_data) {
  
  prepared_crop_data %>% 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = crop_area), colour = NA) +
    labs(fill = NULL,
         colour = NULL) +
    coord_sf(datum = NULL) +
    theme_void() #+
    #theme(legend.justification = c(0,1),
          #legend.position = c(0.1, 0.9)) 
}

#FUNCTION 4: finding proportion of total crop grown in scotland in each boundary
finding_proportion_crop <- function(crop_data_from_digimap) {
  
  crop_data_from_digimap %>% 
    st_centroid(crop_data_from_digimap) %>% 
    st_join(boundary_data, join = st_intersects) %>% 
    filter(!is.na(agcval)) %>% 
    mutate(total_crop_area = sum(agcval)) %>% 
    mutate(prop_total_crop_area = (agcval/total_crop_area)) %>% 
    st_drop_geometry() %>%
    select(ONS_2010, prop_total_crop_area) %>%
    right_join(boundary_data, join_by(ONS_2010 == ONS_2010)) 
}

# FUNCTION 5: finding how much pesticide was sprayed in each boundary
finding_pesticide_spray <- function(crop_data_from_digimap, total_pesticide_sprayed) {
  
  crop_data_from_digimap %>% 
    st_centroid(crop_data_from_digimap) %>% 
    st_join(boundary_data, join = st_intersects) %>% 
    filter(!is.na(agcval)) %>% 
    mutate(total_crop_area = sum(agcval)) %>%
    group_by(ONS_2010) %>%
    summarise(ONS_2010, geometry, total_crop_area, crop_area_per_region = sum(agcval)) %>% 
    mutate(prop_total_crop_area_grown_in_region = (crop_area_per_region/total_crop_area)) %>% # finding proportion of total
    mutate(pesticide_applied_kilos_each_region = prop_total_crop_area_grown_in_region*total_pesticide_sprayed) %>%
    mutate(pesticide_applied_per_hectare = total_pesticide_sprayed/total_crop_area) %>%
    st_drop_geometry() %>%
    select(ONS_2010, pesticide_applied_kilos_each_region, pesticide_applied_per_hectare) %>%
    right_join(boundary_data, join_by(ONS_2010 == ONS_2010)) %>%
    group_by(ONS_2010) %>%
    filter(row_number() == 1)

}

#FUNCTION 6: plotting how much pesticide was sprayed in each boundary (use dataset produced from function 5)
plotting_pesticide_spray <- function(prepared_pesticide_crop_data) {
  
  prepared_pesticide_crop_data %>% 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = pesticide_applied_kilos_each_region), colour = NA) +
    scale_fill_viridis_c(option = "turbo", na.value = "grey") +
    #labs(fill = NULL,
         #colour = NULL) +
    coord_sf(datum = NULL) +
    theme_void() #+
    #theme(legend.justification = c(0,1),
          #legend.position = c(0.1, 0.9))
  
  }

#FUNCTION 7: finding ratio between osteoporosis_datazones and pestide-crop dataset (produced by function 5)
plotting_osteoporosis_pesticide_ratio <- function(osteoporosis_datazones, pesticide_crop_dataset) {
  
  osteoporosis_datazones %>% 
    st_drop_geometry() %>% 
    select(ONS_2010, Rate_Osteoporosis) %>% 
    full_join(pesticide_crop_dataset, join_by(ONS_2010 == ONS_2010), relationship = "many-to-many") %>%
    mutate(ratio_pesticide_osteoporosis = (Rate_Osteoporosis/pesticide_applied_kilos_each_region)) %>% 
    mutate(ratio_pesticide_osteoporosis_logged = log10(Rate_Osteoporosis/pesticide_applied_kilos_each_region) + 1) %>% #filter(ratio_pesticide_osteoporosis <800000) %>%  
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = ratio_pesticide_osteoporosis_logged), colour = NA) +
    scale_fill_viridis_c(option = "turbo", na.value = "lightgrey") +
    theme_void() +
    coord_sf(datum = NA)
}


#setting a shared scale between pesticides of the same crop

simple_range_extracter <- function(plot, scale) {
  
  dimensions <- ggplot_build(plot)
  dimensions$plot$scales$get_scales(scale$aesthetics)$range$range
  
}


get_shared_scale <- function(..., scale) {
  
  plots <- list(...)
  
  ranges <- purrr::map(plots, ~simple_range_extracter(., scale))
  
  single_range <- range(unlist(ranges))
  
  scale$limits <- single_range
  
  scale
}


# Main function
set_scale_union <- function(..., scale) {
  exprs <- rlang::enexprs(...)
  scale <- get_shared_scale(..., scale = scale)
  var_nms <- purrr::map_chr(exprs, rlang::as_name)
  edit_plots_in_place(var_nms, env = parent.frame(),
                      scale = scale)
  # Invisibly return the scale, in case you need it later
  invisible(scale)
}

# Sub-function
edit_plots_in_place <- function(names, env, scale) {
  vars <- rlang::env_has(env = env, nms = names)
  if (!all(vars))
    stop("Environment does not have variables for ",
         paste(names(vars[!vars]), collapse=", "))
  
  purrr:::walk(names, function(nm) {
    og_plot <- rlang::env_get(env, nm = nm)
    message("Changing plot `", nm, "`")
    # Muffles messages about already having scales
    withCallingHandlers(
      assign(x = nm, envir = env,
             value = og_plot + scale),
      message = function(err) {
        if (grepl("already present", err$message))
          invokeRestart("muffleMessage")
      })
  })
}