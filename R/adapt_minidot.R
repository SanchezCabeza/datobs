#### adapt minidot files ####
# jasc 20250818
# minidot samples are the result of combining woth proprietary software

rm(list = ls())
graphics.off()
library(lubridate)
wd <- "G:/Unidades compartidas/LCGC/1 SAMO/0.erddap/minidot/"

#### read files ####
setwd(wd)
#files <- list.files()
setwd("G:/Unidades compartidas/LCGC/1 SAMO/samo.20250423")
file.name <- "minidot samo 54m 20250423.TXT"

df <- read.table(file.name, header = TRUE, sep = ",", skip = 8)
colnames(df) <- c("time.unix", "dateutc", "time.local", "battery",
                  "tem", "oxy", "oxy.sat", "q")
df$dateutc <- ymd_hms(df$dateutc)
# clean end
aux <- df
auxx <- aux[100:4000,]
par(mar = c(4,4,1,1))
plot(auxx$dateutc, auxx$tem, las = 1, pch = 20)
plot(auxx$dateutc, auxx$oxy, las = 1, pch = 20)
hist(auxx$oxy, breaks = 30) #, xlim = c(0.075, 0.085))

# cut files. select first of each series
day.sel <- which(date(aux$dateutc) == "2021-11-23")[1]
df1 <- df[1:(day.sel-1), ]
df2 <- df[day.sel:nrow(df), ]
par(mar = c(4,4,1,1))
plot(df1$dateutc, df1$tem, las = 1, pch = 20)
legend("top", legend = df1$dateutc[1], bty = "n")
plot(df2$dateutc, df2$tem, las = 1, pch = 20)
legend("top", legend = df2$dateutc[1], bty = "n")
#df1 is old
write.csv(df2, "minidot samo 8m 20220427.csv", row.names = FALSE)

