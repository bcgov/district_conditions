static_map <- function(nm, df){
  print(nm)
  df <- df%>%
    filter(name==nm)
  label_colour = ifelse(df$value< mean(df$value, na.rm=TRUE), "white", "black")
  plt <-  ggplot(data = df, mapping=aes(geometry = geometry,
                                        fill=value,
                                        label= paste0(
                                        str_to_title(str_replace_all(aa_name,"_"," ")),
                                          ": ",
                                        scales::comma(value, accuracy = .01))))+
    geom_sf(lwd=.2, colour="white")+
    ggrepel::geom_label_repel(colour=label_colour, stat = "sf_coordinates", size=3.5, min.segment.length = 1)+
    scale_fill_viridis_c(label=scales::comma, name = "", na.value="grey90")+
    theme_void()+
    theme(plot.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = 0)) # Left margin

  aest::aest_fix_labs(plt)
}
