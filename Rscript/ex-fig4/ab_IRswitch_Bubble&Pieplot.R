rm(list=ls())
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(scales)

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.4),
  axis.ticks = element_line(color = "black", size = 0.4),
  axis.text.x = element_text(color = "black", size = 7, face = "plain"), # hjust = 1 # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 7, face = 'plain', hjust = 0.5)
)

# ========== 1. setting phylogenetic level ==========
# Selection："Genus" or "Phylum"
tax_level <- "Phylum" 

# 1. load data
path.AnnotatedIR <- './data/ex-fig4/Annotated_IROptogenetics.xlsx'
raw.AnnotatedIR <- data.frame(read_excel(path.AnnotatedIR))

# 2. tarted samples
abundance_cols <- c("Total", "IR1", "IR2", "IR3", "Dark1", "Dark2", "Dark3")

# 3. statistic
summary.counts <- as.data.frame(table(raw.AnnotatedIR[[tax_level]]))
colnames(summary.counts) <- c(tax_level, 'Gene_counts')
path.save.counts <- './data/ex-fig4/IROptogenetics_Genus_Counts.xlsx'
writexl::write_xlsx(summary.counts, path.save.counts)

summary.abundance <- aggregate(raw.AnnotatedIR[, abundance_cols], 
                              by = setNames(list(raw.AnnotatedIR[[tax_level]]), tax_level), 
                              FUN = sum, na.rm = TRUE)

summary.df <- merge(summary.counts, summary.abundance, by = tax_level)
summary.df$GeneCounts_normed <- summary.df$Gene_counts / summary.df$Total

# 4. Select top20 for visualization
summary.df <- summary.df[order(summary.df$Gene_counts, decreasing = TRUE), ]
summary.df <- summary.df[1:min(20, nrow(summary.df)), ]
summary.df[[tax_level]] <- factor(summary.df[[tax_level]], levels = summary.df[[tax_level]])

# 5. custom colors
custom_colors <- colorRampPalette(c("#344893", "#fee090",'#FF4066', "#D53555", '#AA2B44'))(100)

p <- ggplot(summary.df, aes(x = Gene_counts, 
                            y = !!as.name(tax_level), 
                            size = Total, 
                            color = GeneCounts_normed)) +
  geom_point(alpha = 0.6) +
  geom_text(aes(label = !!as.name(tax_level)), 
            hjust = -0.1, 
            vjust = 0.5, 
            fontface = "italic", 
            size = 2.5,
            color = "black") +
  scale_size_continuous(range = c(1, 10), name = "Abundance (RPKM)") +
  scale_color_gradientn(colors = custom_colors, name = "Normalized gene counts") +
  labs(
    x = "IR gene counts",
    y = NULL,
    title = paste0("IR optogenetic switch under ", tax_level)
  ) +
  theme_cns +
  coord_cartesian(clip = "off")
p

# # 6. output bubbleplot
# ggsave(filename = paste0("ex-fig4a_IRswitch_Bubbleplot_", tax_level, ".pdf"), plot = p, width = 8, height = 8, limitsize = FALSE)

# 7. pie plot
pie_df_all <- raw.AnnotatedIR %>%
  group_by(!!as.name(tax_level)) %>%
  summarise(Gene_counts = n()) %>%
  arrange(desc(Gene_counts))

pie_df_all$Fraction <- pie_df_all$Gene_counts / sum(pie_df_all$Gene_counts)

top_n <- 20
if (nrow(pie_df_all) > top_n) {
  pie_df_top <- pie_df_all[1:top_n, ]
  other_sum <- sum(pie_df_all$Gene_counts[(top_n+1):nrow(pie_df_all)])
  other_fraction <- other_sum / sum(pie_df_all$Gene_counts)
  pie_df_other <- data.frame(
    label = "Other",
    Gene_counts = other_sum,
    Fraction = other_fraction
  )
  pie_df_top$label <- as.character(pie_df_top[[tax_level]])
  pie_df <- rbind(
    pie_df_top[, c("label", "Gene_counts", "Fraction")],
    pie_df_other
  )
} else {
  pie_df <- pie_df_all
  pie_df$label <- as.character(pie_df[[tax_level]])
  pie_df <- pie_df[, c("label", "Gene_counts", "Fraction")]
}

pie_df <- pie_df %>% arrange(desc(Fraction))

pie_df$Label <- paste0(pie_df$label, " (", percent(pie_df$Fraction, accuracy = 0.1), ")")

if (tax_level == "Phylum") {
  # Phylum
  pie_colors <- rev(colorRampPalette(c("#1b9e77", "#a6d854", "#673AB7"))(nrow(pie_df)))
} else if (tax_level == "Genus") {
  # Genus
  pie_colors <- rev(colorRampPalette(c("#344893", "#fee090", "#D53555"))(nrow(pie_df)))
} else {
  # Other
  pie_colors <- rev(colorRampPalette(c("#bdbdbd", "#8073ac", "#f1b6da"))(nrow(pie_df)))
}

pie_df$Label <- factor(pie_df$Label, levels = pie_df$Label)

pie_plot <- ggplot(pie_df, aes(x = "", y = Gene_counts, fill = Label)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = pie_colors) +
  labs(title = "", fill = tax_level) +
  theme_void(base_size = 7) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 7),
    legend.title = element_text(size = 7,face = "bold"),
    legend.text = element_text(size = 7)
  ) +
  guides(fill = guide_legend(override.aes = list(size = 5)))

pie_plot

ggsave(filename = paste0("Fig2b_IRswitch_", tax_level, "_Pieplot.pdf"), plot = pie_plot, width = 14.5, height = 7)
