############################################################################
# Índice de Sutrop: Experimento Rosa G. Pérez-Hernández -------------------

## Edición: Sat 08 July 2023

set.seed(123999)
options(scipen = 10000, digits = 3)
library(tidyverse, warn.conflicts = F)
library(patchwork)

## Plot settings for all plots
plot_setts <- ggplot2::theme(panel.grid = element_blank(), #panel.grid.major.y = element_line(colour = "gray20", linetype = "dashed", linewidth = 0.1),
			     # panel.grid.major.x = element_line(colour = "gray70", linetype = "dashed"),
			     legend.background = element_blank(),
			     legend.text = element_text(color = "black", size = 14),
			     axis.text = element_text(color = "black", size = 14),
			     # axis.text.x = element_text(size = 14, color = "black", angle = 270, hjust = 0.5, vjust = 1),
			     axis.text.y = element_text(size = 14, color = "black"),
			     axis.text.x = element_text(size = 14, color = "black"),
			     strip.text.x = element_text(size = 14, color = "black", face = "bold"),
			     legend.title = element_text(size = 14, color = "black"),
			     axis.title.x = element_text(size = 14, color ="black"))



# 1. Reading raw databae: excel data; check for additional details --------
	## Data were taken from the Excel database: check data out for any additional detail

	sutrop <- xlsx::read.xlsx (file = 'data/p_auritum_database.xlsx', sheetIndex = 3, startRow = 3)
	
	sutrop %>% dplyr::as_tibble()
	
	## Filter data and plot:
	## Uso culinario
	culinario <-
	sutrop %>%
		tidyr::pivot_longer(cols = c(-1:-2), names_to = "comunidad", values_to = "sutrop") %>%
		dplyr::filter(Uso == "Culinario") %>% 
		dplyr::mutate(sutrop = dplyr::if_else(sutrop==0, true = 0.01, false = sutrop)) %>%
		ggplot(aes(x = comunidad, y = sutrop, fill = Tipo)) + 
		theme_bw(base_size = 16) +
		labs (y = "Índice de Sutrop\n", x = element_blank()) +
		geom_bar(position = position_dodge(0.9), stat = "identity", width = 0.8) +
		scale_fill_manual(name = element_blank(), 
				  values = RColorBrewer::brewer.pal(n = 5, name = "Dark2"),
				  limits = c("vaporcitos", "con huevos", "con ibes", "Bebida", "cool"),
				  labels = c("Vaporcitos", "+Huevos", "+Íbes", "Bebida", "Co´ol")) +
		scale_y_continuous(expand=c(0,0), 
				   limits = c(0,1.05), 
				   breaks = seq(from = 0, to = 1, by = .1), 
				   labels = c("0.0", "", 0.2, "", 0.4, "", 0.6, "", 0.8, "", "1.0")) +
		scale_x_discrete(name = element_blank(),
				 limits = c("Ebtun", "Ek.Balam", "Hunuku", "Pixoy", "Santa.Rita", "Uayma"),
				 labels = c("Ebtún", "Ek Balam", "Hunuku", "Pixoy", "Santa Rita", "Uayma")) +
		facet_grid(cols = vars (factor(Uso, levels=c("Culinario"), labels = c("Uso culinario")))) + # guides (fill = guide_legend(nrow = 3, ncol = 3)) + 
		guides (fill = guide_legend(ncol = 3)) + 
		plot_setts + theme(legend.position = "bottom") + coord_flip() +
		theme(panel.grid.major.x = element_line(colour = "gray20", linetype = "dashed", linewidth = 0.1)); culinario
	

	## Uso Medicinal: plots
	medicinal <- sutrop %>%
		tidyr::pivot_longer(cols = c(-1:-2), names_to = "comunidad", values_to = "sutrop") %>%
		dplyr::filter(Uso == "Medicinal") %>%
		dplyr::mutate(sutrop = dplyr::if_else(sutrop==0, true = 0.005, false = sutrop)) %>%
		ggplot(aes(x = comunidad, y = sutrop, fill = Tipo)) + theme_bw() +
		labs (y = "Índice de Sutrop\n", x = element_blank()) +
		geom_bar(position = position_dodge(0.9), stat = "identity", width = 0.8) +
		scale_fill_manual(name = element_blank(),
				  values = RColorBrewer::brewer.pal(n = 12, name = "Paired"),
				  limits = c("Colesterol", "Diabetes", "Dolor", "Fiebre", "Producir leche", "Gastritis ", "Hemorroides", "Nervios", "Pelagra", "Presión", "Purgante", "Tos"),
				  labels = c("Colesterol", "Diabetes", "Dolor", "Fiebre", "Galactogogo", "Gastritis", "Hemorroides", "Nervios", "Pelagra", "Presión", "Purgante", "Tos")) +
		scale_y_continuous(expand=c(0,0),
				   limits = c(0, 0.31),
				   breaks = seq(0, 0.3, 0.03),
				   labels = c("0.0","", 0.06, "",0.12, "", 0.18, "",0.24, "", 0.3)) +
		scale_x_discrete(name = element_blank(),
				 limits = c("Ebtun", "Ek.Balam", "Hunuku", "Pixoy", "Santa.Rita", "Uayma"),
				 labels = c("Ebtún", "Ek Balam", "Hunuku", "Pixoy", "Santa Rita", "Uayma")) +
		facet_grid(cols = vars (factor(Uso, levels=c("Medicinal"), labels = c("Uso medicinal")))) + 
		guides (fill = guide_legend(ncol = 4)) + 
		theme(legend.position = "bottom") + coord_flip() + plot_setts +
		theme(panel.grid.major.x = element_line(colour = "gray20", linetype = "dashed", linewidth = 0.1)); medicinal
	
	
	## Merge into one plot with patchwork:
	library(patchwork)
	rosa_plots <- (culinario + theme(axis.title.x = element_text(size = 14))) | (medicinal + theme(axis.text.y = element_blank(),
												       axis.title.x = element_text(size = 14) ) ); rosa_plots
	

	## Save as a single plot
	
	grDevices::svg(filename = '/Users/mariohg/Library/CloudStorage/Dropbox/PROJECTS/lupe_data/Figures/sutrop.svg', width = 14, height = 6)
	rosa_plots + plot_annotation(tag_levels = 'A', tag_suffix = ") ") &
		theme(plot.tag = element_text(size = 14, face = "bold"))
	dev.off()
	
	ggplot2::ggsave(filename = 'Figures/Figure-2-Sutrop-Usos.png', dpi = 300, width = 14, height = 6)

# 2. MANEJOS Y USOS DE Piper autirum: Valores porcentuales ----------------


	## 1. Leemos la base de datos, convertimos a formato largo, y posteriormente graficamos
	data <- xlsx::read.xlsx (file = 'data/p_auritum_database.xlsx', sheetIndex = 4, startRow = 3) %>% 
		tidyr::pivot_longer(cols = c(-1:-2), names_to = "comunidad", values_to = "sutrop")
	
	data %>% dplyr::as_tibble()
	
	## Filter data and plot:
	## Origen
	origenes <- data %>%
		dplyr::filter(practice == "Origen") %>%
		dplyr::mutate(sutrop = dplyr::if_else(sutrop==0, true = 0.2, false = sutrop)) %>%
		ggplot(aes(x = comunidad, y = sutrop, fill = type)) + theme_bw() +
		labs (y = "\nValores porcentuales (%)\n", x = element_blank()) +
		geom_bar(position = position_dodge(0.9), stat = "identity", width = 0.8) +
		scale_fill_manual(name = element_blank(), 
				  values = RColorBrewer::brewer.pal(n = 5, name = "Dark2"),
				  limits = c("nacio_solo", "del_monte", "de_otro_huerto"),
				  labels = c("Nació sola", "Monte", "Otro huerto")) +
		scale_y_continuous(expand=c(0,0), limits = c(0,31), breaks = seq(0,30,3), 
				   labels = c("0.0","",6,"",12,"",18,"",24,"",30)) +
		scale_x_discrete(name = element_blank(),
				 limits = c("Ebtun", "Ek_Balam", "Hunuku", "Pixoy", "Santa_Rita", "Uayma"),
				 labels = c("Ebtún", "Ek Balam", "Hunuku", "Pixoy", "Santa Rita", "Uayma")) +
		facet_grid(cols = vars (factor(practice, levels=c("Origen"), labels = c("Origen")))) +
		guides (fill = guide_legend(nrow = 1)) + plot_setts + 
		theme(legend.position = "bottom",
		      panel.grid.major.y = element_blank(),
		      panel.grid.major.x = element_line(color = "gray40", linewidth = 0.1, linetype = "dashed")) + 
		coord_flip()
	
	## Manejos: Plotting:
	manejos <- data %>%
		dplyr::filter(practice == "Manejo") %>%
		dplyr::mutate(sutrop = dplyr::if_else(sutrop==0, true = 0.005, false = sutrop)) %>%
		ggplot(aes(x = comunidad, y = sutrop, fill = type)) + theme_bw() +
		labs (y = "\nÍndice de Sutrop\n", x = element_blank()) +
		geom_bar(position = position_dodge(0.9), stat = "identity", width = 0.8) +
		scale_fill_manual(name = element_blank(), 
				  values = RColorBrewer::brewer.pal(n = 5, name = "Dark2"),
				  limits = c("Riego", "Chapeo", "Poda"),
				  labels = c("Riego", "Deshierbe", "Poda")) +
		scale_y_continuous(expand=c(0,0), limits = c(0,1.1), breaks = seq(0,1,.1),
				   labels = c("0.0","",0.2,"",0.4,"",0.6,"",0.8,"","1.0")) +
		scale_x_discrete(name = element_blank(),
				 limits = c("Ebtun", "Ek_Balam", "Hunuku", "Pixoy", "Santa_Rita", "Uayma"),
				 labels = c("Ebtún", "Ek Balam", "Hunuku", "Pixoy", "Santa Rita", "Uayma")) +
		facet_grid(cols = vars (factor(practice, levels=c("Manejo"), labels = c("Manejos")))) +
		guides (fill = guide_legend(nrow = 1)) + plot_setts + 
		theme(legend.position = "bottom", 
		      panel.grid.major.y = element_blank(),
		      panel.grid.major.x = element_line(color = "gray40", linewidth = 0.1, linetype = "dashed")) +
		coord_flip(); manejos

	plots_manejos <- origenes | (manejos + theme(axis.text.y = element_blank())); plots_manejos
	
	grDevices::svg(filename = '/Users/mariohg/Library/CloudStorage/Dropbox/PROJECTS/lupe_data/Figures/Fig.4-Sutrop.svg', width = 10, height = 5)
	plots_manejos + plot_annotation(tag_levels = 'A', tag_suffix = ") ") &
		theme(plot.tag = element_text(size = 14, face = "bold"))
	dev.off()
	
	ggplot2::ggsave(file = 'Figures/Fig.4-Sutrop.png', dpi = 300, device = "png", width = 10, height = 5)

# 3. Guardando el ambiente para uso posterior --------------------------------


	# base::save.image(file = 'rdatabases/Figs_2-y-4_Usos-y-Manejos.RData')
	# base::load(file = 'rdatabases/Figs_2-y-4_Usos-y-Manejos.RData')
	