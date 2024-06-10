idf[hhid == 7137302 & grepl("gole", rname), hhid := 7137305]
idf <- idf[!(hhid == 99081912418 & grepl("rezi", rname)), ]
idf[hhid == 7137316 & grepl("monoa", rname), hhid := 9997137316]
idf[hhid == 7137317 & grepl("afruz", rname), hhid := 9997137317]
table0(duplicated(idf[, .(rd, hhid)]))
