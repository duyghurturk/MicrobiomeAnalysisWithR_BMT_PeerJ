ps_lefse <- filt.ps1
lefse_tax <-  ps_lefse %>%  tax_table %>%  data.frame(stringsAsFactors = F)
lefse_tax <- replace(lefse_tax, is.na(lefse_tax), 'Unclassified')

lefse_tax$Kingdom <- paste0("k_",lefse_tax$Kingdom)
lefse_tax$Phylum <- paste0("p_",lefse_tax$Phylum )
lefse_tax$Class <- paste0("c_",lefse_tax$Class)
lefse_tax$Order <- paste0("o_",lefse_tax$Order)
lefse_tax$Family <- paste0("f_",lefse_tax$Family)
lefse_tax$Genus <- paste0("g_",lefse_tax$Genus)
lefse_tax$Species <- paste0("s_",lefse_tax$Species)



lefse_tax <- lefse_tax %>% group_by(Kingdom, Phylum, Class, Order, Family, Genus, Species) %>% 
  mutate(id = paste(Phylum, Class, Order, Family, Genus,Species, sep = "|")) %>%
  ungroup %>%
  pull(id)

lefse_matrix <- otu_table(ps_lefse) %>%  data.frame(stringsAsFactors = F) %>%  data.frame
lefse_matrix <- t(lefse_matrix)
lefse_matrix <- lefse_matrix %>% data.frame
colnames(lefse_matrix) <-  lefse_tax

lefse_sample <- sample_data(ps_lefse)
lefse_sample_isfactor <- sapply(lefse_sample, is.factor)
lefse_sample[,lefse_sample_isfactor] <- lefse_sample[,lefse_sample_isfactor] %>% lapply(as.character)
lefse_sample <- lefse_sample %>% data.frame

colnames(lefse_matrix)
rownames(lefse_matrix) <- substring(rownames(lefse_matrix),2)



lefse_table <- full_join(rownames_to_column(lefse_sample), rownames_to_column(lefse_matrix), by = ("rowname" = "rowname")) %>% t

lefse_ta <- lefse_table[-1,]

write.table(lefse_ta, file = "LEfse_BMT_CariesRisk_Day14.txt", sep = "\t", row.names = T, col.names = F, quote = F)

