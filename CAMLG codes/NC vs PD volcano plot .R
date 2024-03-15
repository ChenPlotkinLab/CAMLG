library(ggplot2)

# Read data
msaAll <- read.csv("msa.csv")

# List of proteins
protein_names <- c("AP2B1", "APP", "C9", "CTSB", "CTSD", "CTSF", "CTSL", "CTSZ", "DPP7", "GM2A", "HEXB", "LAMP1", "LAMP2", "LYZ", "FUCA1", "TCN2", "TPP1", "Ubiquitin")

# Data frame to store Volcano plot data
volcano_data <- data.frame(Protein = character(), Log2FC = numeric(), Log10PValue = numeric())

# Loop over each protein and perform the analysis
for (protein in protein_names) {
  # Kruskal-Wallis test
  kruskal_test <- kruskal.test(as.formula(paste(protein, "~ SheetDx")), data = msaAll)
  
  # Calculate fold change: median(PD) / median(NC)
  fold_change <- median(msaAll[msaAll$SheetDx == "PD", protein], na.rm = TRUE) / median(msaAll[msaAll$SheetDx == "NC", protein], na.rm = TRUE)
  
  # Add data to the volcano plot data frame
  volcano_data <- rbind(volcano_data, data.frame(Protein = protein, Log2FC = log2(fold_change), Log10PValue = -log10(kruskal_test$p.value)))
}

# Create a Volcano plot
volcano_plot <- ggplot(volcano_data, aes(x = Log2FC, y = Log10PValue)) +
  geom_point(aes(color = Log10PValue < 1.3), size = 3) +  # Coloring points with p-value < 0.05
  geom_text_repel(aes(label = as.character(Protein)), box.padding = 0.5) +
  labs(title = "Volcano Plot: NC vs PD",
       x = "log2(Fold Change)",
       y = "-log10(p-value)") +
  theme_minimal()

# Save plot to a file
ggsave(filename = "NCvsPD_volcano_plot.png", plot = volcano_plot)
