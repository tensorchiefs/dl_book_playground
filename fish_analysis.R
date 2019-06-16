zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")

zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)