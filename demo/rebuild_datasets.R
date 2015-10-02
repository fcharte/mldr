emotions <- mldr_from_dataframe(mldr::emotions$dataset, mldr::emotions$labels$index, "emotions")
birds <- mldr_from_dataframe(mldr::birds$dataset, mldr::birds$labels$index, "birds")
genbase <- mldr_from_dataframe(mldr::genbase$dataset, mldr::genbase$labels$index, "genbase")

save(emotions, file = "emotions.rda", compress = "xz")
save(birds, file = "birds.rda", compress = "xz")
save(genbase, file = "genbase.rda", compress = "xz")
