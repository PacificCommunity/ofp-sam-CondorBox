
#setwd("/test_r")

x="working!"

dir.create("test_r/output_test")

save(x, file="test_r/output_test/test_script.RData")

