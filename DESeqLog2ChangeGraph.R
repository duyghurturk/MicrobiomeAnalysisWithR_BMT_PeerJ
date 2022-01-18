#oppm vs 10ppm ####
# At species level
sigtab_species <- sigtab
sigtab_species <- as.tibble(sigtab_species)
sigtab_species %>% mutate_if(is.factor, as.character) -> sigtab_species
sigtab_species[is.na(sigtab_species)] <- "Unclassified"
sigtab_species$Kingdom <- paste0("k_", sigtab_species$Kingdom)
sigtab_species$Phylum <- paste0("p_", sigtab_species$Phylum)
sigtab_species$Class <- paste0("c_", sigtab_species$Class)
sigtab_species$Order <- paste0("o_", sigtab_species$Order)
sigtab_species$Family <- paste0("f_", sigtab_species$Family)
sigtab_species$Genus <- paste0("g_", sigtab_species$Genus)
sigtab_species$Species <- paste0("s_", sigtab_species$Species)
sigtab_species %>%  unite("Taxa", Phylum:Species,sep = ",") -> sigtab_species

#########################################################################################3
# At genus level

sigtab_genus <- sigtab
sigtab_genus <- as.tibble(sigtab_genus)
sigtab_genus %>% mutate_if(is.factor, as.character) -> sigtab_genus
sigtab_genus[is.na(sigtab_genus)] <- "Unclassified"
sigtab_genus$Kingdom <- paste0("k_", sigtab_genus$Kingdom)
sigtab_genus$Phylum <- paste0("p_", sigtab_genus$Phylum)
sigtab_genus$Class <- paste0("c_", sigtab_genus$Class)
sigtab_genus$Order <- paste0("o_", sigtab_genus$Order)
sigtab_genus$Family <- paste0("f_", sigtab_genus$Family)
sigtab_genus$Genus <- paste0("g_", sigtab_genus$Genus)
sigtab_genus %>%  unite("Taxa", Phylum:Genus,sep = ",") -> sigtab_genus
sigtab_genus <- sigtab_genus %>% select(-Species)

#########################################################################################3
# at family level
sigtab_family <- sigtab
sigtab_family <- as.tibble(sigtab_family)
sigtab_family %>% mutate_if(is.factor, as.character) -> sigtab_family
sigtab_family[is.na(sigtab_family)] <- "Unclassified"
sigtab_family$Kingdom <- paste0("k_", sigtab_family$Kingdom)
sigtab_family$Phylum <- paste0("p_", sigtab_family$Phylum)
sigtab_family$Class <- paste0("c_", sigtab_family$Class)
sigtab_family$Order <- paste0("o_", sigtab_family$Order)
sigtab_family$Family <- paste0("f_", sigtab_family$Family)
sigtab_family %>%  unite("Taxa", Phylum:Family,sep = ",") -> sigtab_family
sigtab_family <- sigtab_family %>% select(-Species, -Genus)
#########################################################################################3
# At Order level
sigtab_order <- sigtab
sigtab_order <- as.tibble(sigtab_order)
sigtab_order %>% mutate_if(is.factor, as.character) -> sigtab_order
sigtab_order[is.na(sigtab_order)] <- "Unclassified"
sigtab_order$Kingdom <- paste0("k_", sigtab_order$Kingdom)
sigtab_order$Phylum <- paste0("p_", sigtab_order$Phylum)
sigtab_order$Class <- paste0("c_", sigtab_order$Class)
sigtab_order$Order <- paste0("o_", sigtab_order$Order)
sigtab_order %>%  unite("Taxa", Phylum:Order,sep = ",") -> sigtab_order
sigtab_order <- sigtab_order %>% select(-Species, -Genus,-Family)

#########################################################################################3
# At Class level
sigtab_class <- sigtab
sigtab_class <- as.tibble(sigtab_class)
sigtab_class %>% mutate_if(is.factor, as.character) -> sigtab_class
sigtab_class[is.na(sigtab_class)] <- "Unclassified"
sigtab_class$Kingdom <- paste0("k_", sigtab_class$Kingdom)
sigtab_class$Phylum <- paste0("p_", sigtab_class$Phylum)
sigtab_class$Class <- paste0("c_", sigtab_class$Class)
sigtab_class %>%  unite("Taxa", Phylum:Class,sep = ",") -> sigtab_class
sigtab_class <- sigtab_class %>% select(-Species, -Genus,-Family, -Order)
#########################################################################################3
# At Phylum level
sigtab_phylum <- sigtab
sigtab_phylum <- as.tibble(sigtab_phylum)
sigtab_phylum %>% mutate_if(is.factor, as.character) -> sigtab_phylum
sigtab_phylum[is.na(sigtab_phylum)] <- "Unclassified"
sigtab_phylum$Kingdom <- paste0("k_", sigtab_phylum$Kingdom)
sigtab_phylum$Phylum <- paste0("p_", sigtab_phylum$Phylum)
sigtab_phylum %>%  unite("Taxa", Phylum:Phylum,sep = ",") -> sigtab_phylum
sigtab_phylum <- sigtab_phylum %>% select(-Species, -Genus,-Family, -Order, -Class)
#########################################################################################3

sigtab  <-  rbind(sigtab_species,sigtab_genus,sigtab_family,sigtab_order,sigtab_class,sigtab_phylum)
sigtab$Taxa <- as.character(sigtab$Taxa)
sigtab$Taxa <- make.unique(as.character(sigtab$Taxa), sep = "_")
#EnhancedVolcano(sigtab,lab = sigtab$Taxa  , x = 'log2FoldChange', FCcutoff = 2,pCutoff = 0.05,y = 'pvalue',labSize = 3,labvjust = 2, labhjust = 0.5, xlim = c(-50,50))
sigtab <- sigtab %>%  mutate(CariesRisk = log2FoldChange > 0)
sigtab$CariesRisk[sigtab$CariesRisk == "TRUE"] <- "low"
sigtab$CariesRisk[sigtab$CariesRisk == "FALSE"] <- "high"

sigtab %>%  
  ggplot(aes(x = reorder(Taxa, log2FoldChange), y = log2FoldChange, color = CariesRisk)) + 
  geom_point(size = 5) + 
  geom_hline(yintercept=0, linetype="dotted", color = "blue",size = 0.5) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 15),
        legend.justification = c(0,0),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 26),
        plot.title = element_text(hjust=0, size = 18),
        axis.title.x = element_text(size = 16)) + 
  ylim(-24,30) +
  coord_flip() + 
  xlab("")+ ggtitle("(a) DESeq2") + 
  geom_text(hjust = "outside",color = "black",size = 4,
            aes(label =Taxa, y = log2FoldChange + -20.5*sign(log2FoldChange))) 


write.csv(sigtab,"combinedSigTab.csv")
