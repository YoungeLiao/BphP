rm(list=ls())

# ===== default setting =====
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(scales) 
library(RColorBrewer) 
library(showtext)

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain", 
                             hjust = 1), # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "right", # "right"
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 8, face = 'plain', hjust = 0.5)
)

# ===== 1. load data =====
path.rawdata <- './data/Fig3/b_asv_taxon.xlsx'
path.metadata <- './data/Fig3/b_metadata.xlsx'
output_dir <- "./data/Fig3/output/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
rawdata <- data.frame(read_excel(path.rawdata))
metadata <- data.frame(read_excel(path.metadata))

target_genus <- "g__Deinococcus" 
# ROUND 1: # Aliidiomarina # Vicingus # Halomonas # Kangiella  # Aequorivita
# Enhanced: # Owenweeksia # Aliidiomarina # Vicingus # Idiomarina
# Inhibited: # Nitrosomonas # g__norank_o__Actinomarinales # g__norank_f__Paracoccaceae # g__Acidiferrimicrobium # g__Muriiphilus # g__unclassified_f__Flavobacteriaceae
# Neutral: # Candidatus_Scalindua # Marinobacter # unclassified_f__Paracoccaceae # Wenzhouxiangella # Marinicella # Nitrospira # Membranihabitans # unclassified_o__Aggregatilineales  # Aggregatilinea # Rhodanobacter # norank_f__A4b # IheB3-7

rawdata.sub <- rawdata[rawdata$Genus == target_genus, ]

if(nrow(rawdata.sub) == 0) {
  stop("didnt find: ", target_genus)
}

cat("find", nrow(rawdata.sub), "items about", target_genus, "records\n")

taxonomy_cols <- c("ASV_ID", "Domain", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
abundance_cols <- setdiff(colnames(rawdata.sub), taxonomy_cols)
abundance_data <- rawdata.sub[, abundance_cols]

abundance_by_sample <- colSums(abundance_data)

df_long <- data.frame(Sample = names(abundance_by_sample), Abundance = as.numeric(abundance_by_sample))

df_long <- merge(df_long, metadata, by.x = "Sample", by.y = "Sample", all.x = TRUE)

df_long$Time <- as.numeric(df_long$Time)

df_plot <- df_long[!is.na(df_long$Time) & !is.na(df_long$Group), ]

sample_summary <- df_plot %>%
  group_by(Group, Time) %>%
  summarise(Sample_count = n())

print(sample_summary)

df_plot$Group <- factor(df_plot$Group, 
                        levels = c("IR", "Dark"),
                        labels = c("Infrared", "Dark"))
df_plot <- df_plot[!is.na(df_plot$Group), ]

# ===== 3. Statistic =====

df_stat <- df_plot %>%
  group_by(Group, Time) %>%
  summarise(
    mean_abundance = mean(Abundance, na.rm = TRUE),
    sd_abundance = sd(Abundance, na.rm = TRUE),
    se_abundance = sd_abundance / sqrt(n()), # 标准误
    n = n(),
    .groups = "drop"
  )

# ===== 4. Visualization =====
main_colors <- c("Infrared" = "#D34947", "Dark" = "#023270") # "Dark" = "#023270", "IR" = "#D34947"
point_shapes <- c("Infrared" = 16, "Dark" = 17)  # 实心圆和三角形
line_types <- c("Infrared" = "solid", "Dark" = "dashed")

p <- ggplot() +
  geom_ribbon(data = df_stat, 
              aes(x = Time, 
                  ymin = mean_abundance - se_abundance, 
                  ymax = mean_abundance + se_abundance,
                  fill = Group),
              alpha = 0.2) +
  
  geom_point(data = df_plot, 
             aes(x = Time, y = Abundance, color = Group, shape = Group),
             size = 1.5, alpha = 0.4, position = position_dodge(width = 0.5)) +
  
  geom_line(data = df_stat, 
            aes(x = Time, y = mean_abundance, color = Group, 
                group = Group, linetype = Group),
            size = 0.75) +
  
  geom_point(data = df_stat, 
             aes(x = Time, y = mean_abundance, color = Group, shape = Group, fill = Group),
             size = 1.5, stroke = 0.8) +
  
  scale_color_manual(values = main_colors, name = '') +
  scale_shape_manual(values = point_shapes, name = '') +
  scale_linetype_manual(values = line_types, name = '') +
  scale_fill_manual(values = main_colors, name = '') + 
  
  scale_x_continuous(breaks = sort(unique(df_stat$Time)),
                     expand = c(0.05, 0.05),
                     labels = function(x) paste0(x)) +
  labs(
    title = sub("g__", "", target_genus),  
    x = "Time (day)",
    y = paste("Abundance")
  ) +
  theme_pubr(base_size = 7) +
  theme_cns
  # theme(
  #   text = element_text(family = "Arial"),
  #   plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
  #   legend.position = c(0.8, 0.9),
  #   legend.background = element_rect(fill = "transparent"), 
  #   legend.title = element_blank(),
  #   legend.text = element_text(size = 14),
  #   axis.title = element_text(size = 16),
  #   axis.text = element_text(size = 14, color = "black"),
  #   axis.text.x = element_text(angle = 0, hjust = 0.5),
  #   panel.grid.major.y = element_line(color = "gray90", linetype = "dashed", size = 0.3),
  #   panel.grid.minor = element_blank(),
  #   panel.border = element_rect(color = "black", fill = NA, size = 0.8),
  #   plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  # )
print(p)


# ggsave(paste0(output_dir, "Fig3e_", sub("g__", "", target_genus), "_TimeSeries.pdf"),
#        p, width = 4.5, height = 4.5, dpi = 300)

# # ===== 5. output =====
# write.csv(df_stat, 
#           paste0(output_dir, sub("g__", "", target_genus), "_abundance_stats.csv"), 
#           row.names = FALSE)
