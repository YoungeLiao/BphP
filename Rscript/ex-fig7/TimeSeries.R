rm(list=ls())

# ===== default setting =====
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(scales) 
library(RColorBrewer) 
library(showtext)
library(cowplot)

font_add("Arial", "/System/Library/Fonts/Arial.ttf") 
showtext_auto()

# 设置CNS风格的主题
theme_cns <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.5),
  axis.ticks = element_line(color = "black", size = 0.5),
  axis.text.x = element_text(color = "black", size = 7, face = "plain"), # hjust = 1 # angle = 30, 
  axis.text.y = element_text(color = "black", size = 7, face = "plain"),
  axis.title = element_text(color = "black", size = 7, face = "plain"),
  legend.position = "none", # "right",
  legend.title = element_blank(),
  legend.text = element_text(size = 7, face = "plain"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.key = element_rect(fill = "white", color = NA),
  plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  plot.title = element_text(size = 7, face = 'plain', hjust = 0.5)
)

# ===== 1. load data =====
path.rawdata <- './data/ex-fig7/asv_taxon.xlsx'
path.metadata <- './data/ex-fig7/metadata.xlsx'
output_dir <- "./output/ex-fig7/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rawdata <- data.frame(read_excel(path.rawdata))
metadata <- data.frame(read_excel(path.metadata))

# ===== 2. data processing =====

# 设置多个目标菌属
target_genus <- c("g__Candidatus_Scalindua", "g__Wenzhouxiangella", "g__Marinicella", "g__Nitrospira", "g__Rhodanobacter") 
NO. <- length(target_genus)
# ROUND 1: # Aliidiomarina # Vicingus # Halomonas # Kangiella  # Aequorivita
# Enhanced v2: c("g__Idiomarina", "g__Owenweeksia", "g__Vicingus")
# Enhanced: # Aliidiomarina  # Owenweeksia  # Vicingus # Idiomarina
# Inihibited v2: c("g__Nitrosomonas", "g__norank_f__Paracoccaceae", "g__Marinobacter", "g__Muriiphilus", "g__Acidiferrimicrobium") 
# Inhibited: # Nitrosomonas # g__norank_o__Actinomarinales # g__norank_f__Paracoccaceae # g__Acidiferrimicrobium # g__Muriiphilus # g__unclassified_f__Flavobacteriaceae
# Neutral
# Neutral: # Candidatus_Scalindua # unclassified_f__Paracoccaceae # Wenzhouxiangella # Marinicella # Nitrospira # Membranihabitans # unclassified_o__Aggregatilineales  # Aggregatilinea # Rhodanobacter # norank_f__A4b # IheB3-7

# 定义生成单个时序图的函数
generate_timeseries_plot <- function(genus_name, rawdata, metadata) {
  
  rawdata.sub <- rawdata[rawdata$Genus == genus_name, ]
  
  if(nrow(rawdata.sub) == 0) {
    warning("didnt find: ", genus_name)
    return(NULL)
  }
  
  cat("find", nrow(rawdata.sub), "items about", genus_name, "records\n")
  
  taxonomy_cols <- c("ASV_ID", "Domain", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  abundance_cols <- setdiff(colnames(rawdata.sub), taxonomy_cols)
  abundance_data <- rawdata.sub[, abundance_cols]
  
  abundance_by_sample <- colSums(abundance_data)
  
  df_long <- data.frame(Sample = names(abundance_by_sample), Abundance = as.numeric(abundance_by_sample))
  
  df_long <- merge(df_long, metadata, by.x = "Sample", by.y = "Sample", all.x = TRUE)
  
  df_long$Time <- as.numeric(df_long$Time)
  
  df_plot <- df_long[!is.na(df_long$Time) & !is.na(df_long$Group), ]
  
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
      se_abundance = sd_abundance / sqrt(n()),
      n = n(),
      .groups = "drop"
    )
  
  # ===== 4. Visualization =====
  main_colors <- c("Infrared" = "#D34947", "Dark" = "#023270")
  point_shapes <- c("Infrared" = 16, "Dark" = 17)
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
               size = 2, alpha = 0.4, position = position_dodge(width = 0.5)) +
    geom_line(data = df_stat, 
              aes(x = Time, y = mean_abundance, color = Group, 
                  group = Group, linetype = Group),
              size = 0.5) +
    geom_point(data = df_stat, 
               aes(x = Time, y = mean_abundance, color = Group, shape = Group, fill = Group),
               size = 2, stroke = 0.5) +
    scale_color_manual(values = main_colors, name = '') +
    scale_shape_manual(values = point_shapes, name = '') +
    scale_linetype_manual(values = line_types, name = '') +
    scale_fill_manual(values = main_colors, name = '') + 
    scale_x_continuous(breaks = sort(unique(df_stat$Time)),
                       expand = c(0.05, 0.05),
                       labels = function(x) paste0(x)) +
    labs(
      title = sub("g__", "", genus_name),  
      x = "Time (d)",
      y = paste("Abundance")
    ) +
    theme_cns
  
  # 保存单个菌的统计结果
  write.csv(df_stat, 
            paste0(output_dir, sub("g__", "", genus_name), "_abundance_stats.csv"), 
            row.names = FALSE)
  
  return(p)
}

# 遍历所有目标菌属生成绘图列表
plot_list <- lapply(target_genus, function(g) generate_timeseries_plot(g, rawdata, metadata))
names(plot_list) <- target_genus

# 移除未找到的NULL图
plot_list <- plot_list[!sapply(plot_list, is.null)]

if(length(plot_list) > 0) {
  # 保存各个子图
  for(g_name in names(plot_list)) {
    p_curr <- plot_list[[g_name]]
    ggsave(paste0(output_dir, "Fig3e_", sub("g__", "", g_name), "_TimeSeries.pdf"),
           p_curr, width = 4.5, height = 4.5, dpi = 300)
    ggsave(paste0(output_dir, "Fig3e_", sub("g__", "", g_name), "_TimeSeries.png"),
           p_curr, width = 4.5, height = 4.5, dpi = 300)
  }
  
  # 从第一个子图中提取统一图例
  legend <- get_legend(plot_list[[1]] + theme(legend.position = "right"))
  
  # 拼合所有子图（不包含图例），然后与图例组合
  combined_plots <- plot_grid(plotlist = plot_list, ncol = NO., align = "vh")
  final_plot <- plot_grid(combined_plots, legend, rel_widths = c(1, 0.15))
  
  # 保存拼合后的大图
  ggsave(paste0(output_dir, "Combined_TimeSeries.pdf"),
         final_plot, width = 9, height = 1.5, dpi = 300)
  ggsave(paste0(output_dir, "Combined_TimeSeries.png"),
         final_plot, width = 9, height = 1.5, dpi = 300)
  
  print(final_plot)
}

