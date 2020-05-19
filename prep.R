# code used to get data frames in the datasets package
# (no need to re-run it every time)

# data()$results[,'Item'] %>% 
#   sub('\\s\\(.+\\)', '', x = .) %>% 
#   subset(sapply(., function(s) {
#     is.data.frame(get(s)) & any(sapply(get(s), is.numeric))
#   })) %>% 
#   dput()

set_choices <- c(
  "BOD", "CO2", "ChickWeight", "DNase", "Formaldehyde", "Indometh", 
  "InsectSprays", "LifeCycleSavings", "Loblolly", "Orange", "OrchardSprays", 
  "PlantGrowth", "Puromycin", "Theoph", "ToothGrowth", "USArrests", 
  "USJudgeRatings", "airquality", "anscombe", "attenu", "attitude", 
  "beaver1", "beaver2", "cars", "chickwts", "esoph", "faithful", 
  "freeny", "infert", "iris", "longley", "morley", "mtcars", "npk", 
  "pressure", "quakes", "randu", "rock", "sleep", "stackloss", 
  "swiss", "trees", "warpbreaks", "women"
)

help_main_text <- c(
  'A lot of parametric tests assume a normal distribution of the
statistic of interest. One way to test that assumption is by examining how
similar or (dissimilar) your sample is to the a normal distribution, and this is
exactly the purpose of this app: to provide some tools, graphical and
statistical, to test a variable\'s distribution against the normal.'
)
