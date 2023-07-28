##---
## Analyzing data from essential oil composition of P. auritum ###
##---


# 1. Environment configuration --------------------------------------------

options(scipen = 10000, digits = 3, ggrepel.max.overlaps = Inf)
set.seed(9999)
library(vegan); library(FactoMineR); library(tidyverse); library(gridExtra)
base::load('rdatabases/Fig.5-PCA-aceites-esenciales.RData')


# 2. Loading database -----------------------------------------------------

	# Load the raw database

	data <- read.table(file = 'data/db_Fig.5_PCA_aceites_esenciales.tsv', row.names = 1, header = T, sep = '\t')
	
	metadata <- data %>% 
		tibble::rownames_to_column(var = "TRATAMIENTO") %>% 
		select(1:4); head(metadata)

# 3. Transform data and determine the Principal Component Analysis  -------

	head(data)
	oil.pca <- FactoMineR::PCA(data[,8:ncol(data)], ncp = 4)

	# 3.1. Get individuals coordinates and merge with their metadata
	
	sam.coords <- data.frame (oil.pca$ind$coord, check.names = F) %>% 
		tibble::rownames_to_column(var = "TRATAMIENTO") %>% 
		dplyr::inner_join(x = metadata, by = "TRATAMIENTO")
	str(sam.coords)
	sam.coords %>% tibble::as_tibble()
	
	# 3.2. Get variables (essential oils) coordinates of the top 10 contributors for PC1 and PC2

	topcontrib <- data.frame(oil.pca$var$contrib, check.names = F) %>%
		tibble::rownames_to_column(var = "oil") %>% 
		dplyr::arrange(-Dim.1) %>% 
		dplyr::filter(!grepl(pattern = "TR-", x = oil)) %>% 
		filter(Dim.1 > 4); topcontrib
	
	# 3.3. Get variables (essential oils), and ignore those with unknown names
	
	responsevar <- data.frame(oil.pca$var$coord, check.rows = F, check.names = F) %>%
		tibble::rownames_to_column(var = "ResponseVar"); responsevar


# 4. Draw the Principal Component Analysis with ggplot2: PCA --------------
	
	p <- 
	ggplot() + theme_bw ()+
		labs (x = paste ("PC1 ", paste(base::round (oil.pca$eig[1,2], digits = 2), "%", sep = "")),
		      y = paste ("PC2 ", paste(base::round (oil.pca$eig[2,2], digits = 2), "%", sep = ""))) +
		# adding vectors: variables
		geom_segment(data = responsevar,
			     aes(x = 0, y = 0, xend = Dim.1*5, yend = Dim.2*5),
			     arrow = arrow(length = unit(0.15, "cm")),
			     color = "black", show.legend = NA) +
		## adding vectors names: variable names in front of each vector
		## Plot only the top 10 contributors per PC
		ggrepel::geom_label_repel(data = responsevar %>% 
					  	dplyr::filter(!grepl(pattern = 'TR-', x = ResponseVar)) %>% 
					  	dplyr::filter(ResponseVar %in% topcontrib$oil),
					  aes(x=Dim.1*7, y=Dim.2*7, label = ResponseVar),
					  segment.colour = NA, col = 'black', fill= "#FFFFFF",
					  fontface = "italic",  box.padding = 0.6, size = 5) +
		geom_point(data = sam.coords, aes (x = Dim.1, y = Dim.2, shape = SITIO, colour = CATEGORIAS), size = 5, alpha = 0.7) +
		stat_ellipse(data = sam.coords, aes(x=Dim.1, y=Dim.2, color = CATEGORIAS), type = "norm") +
		scale_color_manual(name = element_blank(), values = c("#E69F00", "#56B4E9")) +
		scale_shape_manual(name = element_blank(), values=c(15,19)) +
		scale_x_continuous(limits = c(-10,10), breaks = seq(from = -10, to = 10, by = 5)) +
		scale_y_continuous(limits = c(-10,15), breaks = seq(from = -10, to = 15, by = 5)) +
		geom_hline(yintercept=0, linetype="dashed", color = "black") +
		geom_vline (xintercept = 0, linetype="dashed", color = "black") +
		guides(fill = guide_legend(nrow = 2)) +
		theme(panel.grid = element_blank(),
		      legend.text = element_text(size = 12, color = "black"),
		      axis.text = element_text(size = 14, color = "black"),
		      axis.title = element_text(color = "black", size = 14),
		      legend.key = element_blank(),
		      ## legend.position:legend.position = c(0.85,0.85)
		      legend.position = "top")
	# theme(legend.position = "top", plot.margin = unit(c(0.5, 1.5, 0.4, 0.1), "cm"), # plot border and minor gridlines editing: panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4), # facet grid format: strip.text.x = element_text(size = 12, colour = "black", face = "bold"), axis.text = element_text(color="black", size=14), axis.title.y = element_text (color="black", size=15, vjust = 1, hjust = 0.5), axis.title.x = element_text (color="black", size=15, vjust = 1, hjust = 0.5), legend.title = element_text(color="black", size=14))
	
	stats_data <- data.frame("Factor" = c("Sitio", "Categoría", "Interacción"),
	"P_value" = c(0.018, 0.140, 0.450),
	"R2" = c(0.080, 0.003, 0.110)); stats_data
	
	p + annotation_custom(tableGrob(stats_data, rows=NULL), 
			      xmin=3, xmax=10, ymin=10, ymax=15)
	ggplot2::ggsave(file = 'Figures/Fig.5.PCA-acei_esenciales.png', dpi = 300, device = "png", width = 7, height = 7)


# 5. Multivariate analysis: perMANOVA -------------------------------------

	## perMANOVA: Using the filtered data
	# sink (file = 'data/permanova-eoils.txt')
	# dist_essoils <- vegan::vegdist(x = data[,8:ncol(data)], method = "euclidean") 
	vegan::adonis2(formula = dist_essoils ~ SITIO * CATEGORIAS, data = data[,1:7], parallel = T, permutations = 9999)
	sink (file = NULL)
	

# 6. Saving the session of R environment ----------------------------------

	# base::save.image(file = 'rdatabases/Fig.5-PCA-aceites-esenciales.RData')
	