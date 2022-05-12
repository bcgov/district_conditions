static_map <- function(nm, tbbl, bc_map, anns) {
  set.seed(123)
  print(nm)
  tbbl <- tbbl %>%
    filter(name == nm)
  label_colour <- ifelse(tbbl$value < mean(tbbl$value, na.rm = TRUE), "white", "black")
  plt <- ggplot() +
    geom_sf(data = tbbl, mapping = aes(
      geometry = geometry,
      fill = value
    ), lwd = 0.5, colour = "white") +
    coord_sf(expand = FALSE)
    if(anns==TRUE){
      plt <- plt+
        geom_sf(data = bc_map, colour = "black", fill='brown', alpha=.05)
    }
    plt <- plt+
      ggrepel::geom_label_repel(data = tbbl, mapping = aes(
      geometry = geometry,
      fill = value,
      label = paste0(
        str_to_title(str_replace_all(District, "_", " ")),
        ": ",
        scales::comma(value, accuracy = .01)
      )
    ), colour = label_colour, stat = "sf_coordinates", size = 5, min.segment.length = 1) +
    scale_fill_viridis_c(label = scales::comma, name = "", na.value = 'white')
    if(anns == TRUE){
      plt <- plt+
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
    ggspatial::annotation_north_arrow(location = "bl",
                           which_north = "true",
                           pad_x = unit(0.75, "in"),
                           pad_y = unit(0.5, "in")) +
    xlab("") +
    ylab("") +
    theme(panel.grid.major = element_line(color = gray(.5),
                                          linetype = "dashed", size = 0.5))
    }else{
      plt <- plt+
        ggthemes::theme_map()
    }
  aest::aest_fix_labs(plt)
}
