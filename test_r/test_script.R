
#setwd("/test_r")

x="working!"

# Construct the absolute path to the output directory
output_dir <- file.path(getwd(), "test_r", "output_test")

# Create the directory (including any necessary parent directories)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save the object 'x' to the absolute path
save(x, file = file.path(output_dir, "test_script.RData"))
