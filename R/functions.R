static_map <- function(nm, tbbl) {
  set.seed(123)
  print(nm)
  tbbl <- tbbl %>%
    filter(name == nm)
  just_geometry <- tbbl%>%
    ungroup()%>%
    select(geometry)
  tbbl <- tbbl %>%
    filter(is.na(value)==FALSE)
  label_colour <- ifelse(tbbl$value < mean(tbbl$value, na.rm = TRUE), "white", "black")
  plt <- ggplot() +
    geom_sf(data = just_geometry, 
            fill="grey", 
            alpha=.5, 
            lwd = 0.5, 
            colour = "white")+
      geom_sf(data = tbbl, 
              mapping = aes(
                geometry = geometry,
                fill = value
              ), 
              lwd = 0.5, 
              colour = "white") +
    coord_sf()
    plt <- plt+
      ggrepel::geom_label_repel(data=tbbl, mapping = aes(
      geometry = geometry,
      fill = value,
      label = paste0(
        str_to_title(str_replace_all(regional_district, "_", " ")),
        ": ",
        scales::comma(value, accuracy = .01)
      )
    ), colour = label_colour, stat = "sf_coordinates", size = 5, min.segment.length = 1) +
    scale_fill_viridis_c(label = scales::comma, name = "", na.value = 'white')
    plt <- plt+
      theme_void()+ 
      theme(text=element_text(size=15))+
      guides(fill = guide_colourbar(barwidth = 2, barheight = 10))
  aest::aest_fix_labs(plt)
}
