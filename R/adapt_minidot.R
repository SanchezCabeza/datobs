#### adapt minidot files ####
# jasc 20250818
# minidot files  are the result of combining daily records with proprietary software
# This function adapts format to the package structure: dateuts, tem, oxy, oxy.sat

rm(list = ls())
graphics.off()
library(lubridate)
library(datobs)
wd <- "G:/Unidades compartidas/LCGC/1 SAMO/0.erddap/minidot/"
output.dir = "./csv"

file.name <- list.files()

i=1
df <- read.table(file.name[i], header = TRUE, sep = ",", skip = 8)
colnames(df) <- c("time.unix", "dateutc", "time.local", "battery",
                  "tem", "oxy", "oxy.sat", "q")
#dateutc,oxy,tem,oxy.flag,tem.flag,oxy.sat
df <- df[ , c("dateutc", "tem", "oxy", "oxy.sat")]
dateutc <- ymd_hms(df$dateutc)

# delete
plot(dateutc, df$tem)

aux <- clean_extremes_2var(df, columns = c("tem", "oxy"), n_check = 100, factor = 3)


df$dateutc <- ymd_hms(df$dateutc)
# clean end
aux <- df
auxx <- aux[100:4000,]
par(mar = c(4,4,1,1))
plot(auxx$dateutc, auxx$tem, las = 1, pch = 20)
plot(auxx$dateutc, auxx$oxy, las = 1, pch = 20)
hist(auxx$oxy, breaks = 30) #, xlim = c(0.075, 0.085))



#### new file names ####
setwd(wd)
i=1
parse_filename(files[i], observatory = "samo", stationNumber = "boya",
               depth = "8m", variables = "ot")




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

