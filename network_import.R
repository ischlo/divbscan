
library(reticulate)
# library(cppRnet)

# reticulate::py_run_file('network_import.py')

cppRosm::extract_graph(output_path,out=network_filename)
