ux <- copy(get(FileNameForUD)) # need to copy as the following change original data
# demean Time, LY, LYear
ux[, paste0("UDTime.", 2:4) := .(Time.2, Time.3, Time.4)]
timecolumns <- paste0("Time.", 2:4)
ux[,  (timecolumns) := lapply(.SD, function(x) x - mean(x, na.rm = T)), 
  .SDcols = timecolumns]
if (any(grepl("LY3", colnames(ux)))) {
  ux[, paste0("UDLY", 2:4) := .(LY2, LY3, LY4)]
  timecolumns <- paste0("LY", 2:4)
  ux[,  (timecolumns) := lapply(.SD, function(x) x - mean(x, na.rm = T)), 
    .SDcols = timecolumns]
}
if (any(grepl("LYear", colnames(ux)))) {
  ux[, UDLYear := LYear]
  ux[, LYear := LYear - mean(LYear)]
}
# create UDAttributes and demeaned Attributes
for (aa in c(ArmsC2[-1], "LargeSize", "WithGrace", "InKind", "UltraPoor")) {
  ux[, paste0("UDdummy", aa) := eval(parse(text=paste0("dummy", aa)))]
  ux[, paste0("dummy", aa) := eval(parse(text=paste0("dummy", aa))) -
    eval(parse(text=paste0("mean(dummy", aa, ", na.rm = T)")))]
}
# create UDAttributes*TimeX interactions
for (aa in c(ArmsC2[-1], "LargeSize", "WithGrace", "InKind", "UltraPoor"))
  for (tt in 2:4) {
    ux[, paste0("UDdummy", aa, ".Time", tt) := 
      eval(parse(text=paste0("UDdummy", aa)))*
      eval(parse(text=paste0("UDTime.", tt)))
      ]
    if (any(grepl("LY3", colnames(ux)))) {
      ux[, paste0("UDdummy", aa, ".LY", tt) := 
        eval(parse(text=paste0("UDdummy", aa)))*
        eval(parse(text=paste0("UDLY", tt)))
        ]
      ux[, paste0("dummy", aa, ".LY", tt) := 
        eval(parse(text=paste0("dummy", aa)))*
        eval(parse(text=paste0("LY", tt)))
        ]
    }
  }
# create Attributes aa*UltraPoor interactions
for (aa in c(ArmsC2[-1], "LargeSize", "WithGrace", "InKind")) {
  ux[, (paste0("UDdummy", aa)) := eval(parse(text=paste0("dummy", aa)))]
#    ux[, (paste0("UDdummy", aa)) := aa]
  ux[, paste0("dummy", aa, ".UltraPoor") := 
    (eval(parse(text=paste0("dummy", aa)))-
      mean(eval(parse(text=paste0("dummy", aa))), na.rm = T))
#    aa-mean(aa, na.rm = T)
    * dummyUltraPoor]
  ux[, paste0("UDdummy", aa, ".UltraPoor") := 
    eval(parse(text=paste0("UDdummy", aa))) * UDdummyUltraPoor]
#    paste0("UD", aa) * UDdummyUltraPoor]
}
# create Attributes*UltraPoor*TimeT interactions (TimeX is already demeaned)
for (aa in c(ArmsC2[-1], "LargeSize", "WithGrace", "InKind"))
  for (tt in 2:4) {
    ux[, paste0("dummy", aa, ".UltraPoor.Time", tt) := 
      eval(parse(text=paste0("dummy", aa, ".UltraPoor")))*
      eval(parse(text=paste0("Time.", tt)))
      ]
    ux[, paste0("UDdummy", aa, ".UltraPoor.Time", tt) := 
      eval(parse(text=paste0("UDdummy", aa, ".UltraPoor")))*
      eval(parse(text=paste0("UDTime.", tt)))
      ]
    if (any(grepl("LY3", colnames(ux)))) {
      ux[, paste0("dummy", aa, ".UltraPoor.LY", tt) := 
        eval(parse(text=paste0("dummy", aa, ".UltraPoor")))*
        eval(parse(text=paste0("LY", tt)))
        ]
      ux[, paste0("UDdummy", aa, ".UltraPoor.LY", tt) := 
        eval(parse(text=paste0("UDdummy", aa, ".UltraPoor")))*
        eval(parse(text=paste0("UDLY", tt)))
        ]
    }
  }
# create HadCows*Attributes*Time interactions
if (grepl("^AL|lvo0|NeA", FileNameForUD)) {
  # clean up previously created iteractions
  ux[, grepout("^dummy.*dummyHad|HadCows\\.", colnames(ux)) := NULL]
  ux[, UDdummyHadCows := dummyHadCows]
  ux[, DemeanedDummyHadCows := UDdummyHadCows - mean(UDdummyHadCows, na.rm = T)]
  for (aa in c(ArmsC2[-1], "LargeSize", "WithGrace", "InKind")) {
    ux[, paste0("UDdummyHadCows.", aa) := 
       UDdummyHadCows * (eval(parse(text=paste0("UDdummy", aa))))]
    ux[, paste0("DemeanedDummy", aa) := 
      eval(parse(text=paste0("UDdummy", aa, "-mean(UDdummy", aa, ")")))]
    ux[, paste0("dummyHadCows.", aa) := 
      (eval(parse(text=paste0("DemeanedDummy", aa)))) * DemeanedDummyHadCows]
    for (tt in 2:4) {
      ux[, paste0("dummyHadCows.Time", tt) := 
        DemeanedDummyHadCows* eval(parse(text=paste0("Time.", tt)))
        ]
      ux[, paste0("UDdummyHadCows.Time", tt) := 
        UDdummyHadCows* eval(parse(text=paste0("UDTime.", tt)))
        ]
      ux[, paste0("dummyHadCows.", aa, ".Time", tt) := 
        DemeanedDummyHadCows* 
        (eval(parse(text=paste0("DemeanedDummy", aa)))) *
        (eval(parse(text=paste0("Time.", tt))))
        ]
      ux[, paste0("UDdummyHadCows.", aa, ".Time", tt) := 
        UDdummyHadCows* 
        (eval(parse(text=paste0("UDdummy", aa)))) *
        (eval(parse(text=paste0("UDTime.", tt))))
        ]
    }
  }
  ux[, dummyHadCows := as.numeric(dummyHadCows>0)]
  ux[, DemeanedDummyHadCows := NULL]
}
# create OwnCattle0*Attributes*Time AdiCattle0*Attributes*Time interactions
if (grepl("^NeAE", FileNameForUD)) {
  for (kk in c("OwnCattle0", "AdiCattle0"))
  {
    ux[, paste0("UDdummy", kk) := eval(parse(text=kk))]
    ux[, paste0("DemeanedDummy", kk) := 
      eval(parse(text=paste0("UDdummy", kk))) - 
      mean(eval(parse(text=paste0("UDdummy", kk))), na.rm = T)]
    for (aa in c(ArmsC2[-1], "LargeSize", "WithGrace", "InKind")) {
      ux[, paste0("UDdummy", kk, ".", aa) := 
        eval(parse(text=paste0("UDdummy", kk))) * (eval(parse(text=paste0("UDdummy", aa))))]
      ux[, paste0("DemeanedDummy", aa) := 
        eval(parse(text=paste0("UDdummy", aa, "-mean(UDdummy", aa, ")")))]
      ux[, paste0("dummy", kk, ".", aa) := 
        eval(parse(text=paste0("DemeanedDummy", aa))) * 
        eval(parse(text=paste0("DemeanedDummy", kk)))]
      for (tt in 2:4) {
        ux[, paste0("dummy", kk, ".Time", tt) := 
          eval(parse(text=paste0("DemeanedDummy", kk))) * 
          eval(parse(text=paste0("Time.", tt)))
          ]
        ux[, paste0("UDdummy", kk, ".Time", tt) := 
          eval(parse(text=paste0("UDdummy", kk))) * 
          eval(parse(text=paste0("UDTime.", tt)))
          ]
        ux[, paste0("dummy", kk, ".", aa, ".Time", tt) := 
          eval(parse(text=paste0("DemeanedDummy", kk))) *
          eval(parse(text=paste0("DemeanedDummy", aa))) *
          eval(parse(text=paste0("Time.", tt)))
          ]
        ux[, paste0("UDdummy", kk, ".", aa, ".Time", tt) := 
          eval(parse(text=paste0("UDdummy", kk))) * 
          eval(parse(text=paste0("UDdummy", aa))) *
          eval(parse(text=paste0("UDTime.", tt)))
          ]
      }
    }
    ux[, paste0("DemeanedDummy", kk) := NULL]
    ux[, paste0("dummy", kk) := eval(parse(text=paste0("UDdummy", kk)))]
  }
}
# restore: aa>0 is 1, aa<0 is 0
for (aa in paste0("dummy", c(ArmsC2[-1], "LargeSize", "WithGrace", "InKind")))
  ux[, (aa) := as.numeric(get(aa)>0)]
for (aa in paste0("Time.", 2:4))
  ux[, (aa) := as.numeric(get(aa)>0)]
if (any(grepl("LY3", colnames(ux))))
  for (aa in paste0("LY", 2:4))
    ux[, (aa) := as.numeric(get(aa)>0)]
assign(FileNameForUD, ux)
rm(ux)
