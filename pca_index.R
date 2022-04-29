pca <- district_conditions%>%
  filter(type=="normalized")%>%
  select(-region, -thing, -type)%>%
  pivot_wider(id_cols = c(district), names_from = name, values_from = value)%>%
  column_to_rownames(var = "district")%>%
  as.matrix()%>%
  nipals::nipals(fitted=TRUE)

class(pca) <- "princomp"
pca$sdev <- sqrt(pca$eig)
pca$n.obs <- dim(pca$scores)[1]

pca_names <- c("Biplot","Comparison")
pca_plots <- list()
pca_plots[[1]] <- ggbiplot::ggbiplot(pca, alpha=.5, labels.size = 4, varname.size = 4)+
  ggrepel::geom_text_repel(aes(label= str_to_title(str_replace_all(rownames(pca$scores),"_", " "))), alpha=.5)+
  theme_minimal()

pca_index <- as_tibble(pca$scores, rownames="district")%>%
  select(district, new_index=PC1)
old_index <- district_conditions%>%
  filter(name=="Overall: index")%>%
  select(district, value)

pca_plots[[2]] <-full_join(old_index, pca_index)%>%
  ggplot(aes(value, new_index, label=str_to_title(str_replace_all(district,"_", " "))))+
  geom_line(stat="smooth", method="lm", se=FALSE, alpha=.25)+
  geom_point(alpha=.5)+
  ggrepel::geom_text_repel(min.segment.length = 0, max.overlaps=20, alpha=.5)+
  theme_minimal()+
  labs(x="Index based on quartiles", 
       y="index based on principal components")

pca_plots <- tibble(names=pca_names, plots=pca_plots)