# Necessary Packages ####
require(tidyr)    # for reshaping data (spread() and gather())
require(multcomp) # multiple comparisons (get Tukey comparisons for glm)
require(ggplot2)  # stupid ggplot
require(plyr)     # Tools for Splitting, Applying and Combining Data (mapvalues(), mutate(), count(), ddply)
require(dplyr)    # for data manipulation
require(reshape)  # melt()
require(reshape2) # melt(), merge_all()
require(corrplot) # for plotting correlations
require(Hmisc)    # has rcorr()

## I'm gonna fix that last joke by taking out all the words and adding new ones.
## Mitch Hedberg

################################## Question Analyses #####################################


# read in data for each section and each exam separately 
# all data sets include the 6 columns 
# ("Question","Version","Question.Text","Average.Score","Percent","Attempts" )

# df <- read.csv(FILENAME",stringsAsFactors = F)

### Format data
# get rid of actual question text. not necessary but cleaner
df <- df[ , c(1:2,4:6)]

# add v to version number so treated as a factor
df$Version <- paste('V',df$Version)

# add Correct and Incorrect columns to the data
df$Correct <- round((df$Percent*df$Attempts),0)
df$Incorrect <- df$Attempts-df$Correct

## this loop turns the data into longform raw data
# this is only necessary to do statistical comparisons between each question pair
# i.e. Tukey pairwise comparisons
# can be replicated for all exams and sections so long as columns names are the same
df.new <- data.frame(Question = character(0), Version = character(0), 
                     Response = character(0),stringsAsFactors = FALSE)
# in order to run loop, must have # correct and # incorrect in data set
# takes a few minutes to run
# change the 187 to the total number of rows for each data set (187 is Exam 1 section 001,002)
for (i in 1:187) {
  x = df$Correct[i]
  y = df$Incorrect[i]
  Quest = df$Question[i]
  Vers = df$Version[i]
  for (j in 1:x) {
    newrow <- c(Quest, Vers, '1')
    df.new<- rbind(df.new,newrow,stringsAsFactors=FALSE)
  }
  for (j in 1:y) {
    newrow <- c(Quest, Vers, '0')
    df.new<- rbind(df.new,newrow,stringAsFactors=FALSE)
  }
}
df.new<-df.new[df.new$X.Q1.!='FALSE',]

colnames(df.new) <- c("Question", "Version", "Response")
df.new$Response <- as.numeric(df.new$Response)
df.new$Version <- as.factor(df.new$Version)
df.new$Question <- as.factor(df.new$Question)

#### Model to test versions within question and get tukey pairwise comparisons
# takes a few minutes to run
listoftests <- dlply(df.new,.(Question), 
                     function(x) summary(glht(model =  glm(Response~Version,data=x, 
                                                           family = binomial()),
                                              linfct = mcp(Version = "Tukey"))
                     ))

# can call each question comparison individually 
# listoftests$`Q1 `

#### Visualizations (bar charts of each version for all 30 questions facet wrapped)
df$Question <- factor(df$Question, levels = unique((df$Question)))
df$Version <- factor(df$Version, levels = unique((df$Version)))
df <- df[complete.cases(df),]

p <-ggplot(df, aes(y=Percent, x=Version, fill = Version), na.omit = T) 
p + geom_bar(stat='identity') +
  facet_wrap(~Question)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  # Change the title of the plot based on the data set being used 
  labs(x = "Version of Each Question", y = "Percent Correct", 
       title = "Exam 1 Performance for Sections 003 and 004")+
  theme(legend.position = "none")+
  scale_fill_hue(l=60)  

#### Same analyses and visualizations for all sections combined
# will wash out any between instructor differences
# data set should have same structure as the individual section with an additional column for section
# i.e. ("Question","Version","Question.Text","Average.Score","Percent","Attempts","Section")

# Format data
df <- df[,c(1:2,4:7)]

df$Version <- paste('V',df$Version)

df$Correct <- round((df$Percent*df$Attempts),0)
df$Incorrect <- df$Attempts-df$Correct

################ visualizations hopefully
df$Question <- factor(df$Question, levels = unique((df$Question)))
df$Version <- factor(df$Version, levels = unique((df$Version)))

p <-ggplot(df, aes(y=(Percent/2), x=Version, fill = Version), na.omit = T) 
p + geom_bar(stat='identity') +
  facet_wrap(~Question)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Version of Each Question", y = "Percent Correct", 
       title = "Exam 1 Performance for All Sections")+
  theme(legend.position = "none")+
  scale_fill_hue(l=60)  

###################### Relationship between Homework and Exam performance ######################

# data sets will have to be cleaned after being downloaded from D2L
# all scores should be downloaded as p(c)
# this script is set up for Exams then Clicker then Homework
# i.e.
# [1] "SubID"           "E1"              "E2"              "E3"             
# [5] "E4"              "E.ave"           "C1"              "C2"             
# [9] "C3"              "C4"              "C5"              "C6"             
# [13] "C7"              "C8"              "C9"              "C10"            
# [17] "C11"             "C12"             "C13"             "C14"            
# [21] "C15"             "C16"             "C17"             "C18"            
# [25] "C19"             "C20"             "C21"             "C22"            
# [29] "C23"             "C24"             "C25"             "C26"            
# [33] "C27"             "C28"             "C29"             "C30"            
# [37] "C31"             "C32"             "C33"             "C34"            
# [41] "C.ave"           "C.count"         "Logistics"       "Lipids"         
# [45] "Membranes"       "Proteins"        "Cell.Diversity"  "Carbohydrates"  
# [49] "Photosynthesis"  "Respiration"     "Comparison"      "Mitosis"        
# [53] "Meiosis"         "Genetics"        "DNA"             "Gene.Expression"
# [57] "Gene.Regulation" "Viruses"         "HW.ave"          "HW.count"       

# note that there are additional colums for the number of clicker questions and homeworks completed
# i.e. C.count & HW.count
# also note that the sections will be different because there were different numbers of clicker questions

#### plot correlation matrix
# restrict data sets to interesting variables
df1 <- df[,c(2:42)] # clicker section 12
df2 <- df[,c(2:6,43:60)] # hw section 12

df1 <- df[,c(2:35)] # clicker section 34
df2 <- df[,c(2:6,36:53)] # hw section 34

# get correlation coefficients 
cor.mat.cli <- rcorr(as.matrix(df1))
cor.mat.hw <- rcorr(as.matrix(df2))

## plot for hw x exam correlations
corrplot.mixed(cor.mat.hw[[1]], upper = "color", number.cex = .5,cl.lim = c(0, 1), tl.col = "black",
               tl.cex = .5, 
               # change title for correct section
               title = "Correlation Matrix for Exams and Homework Performance, Sections 003 & 004",
               mar=c(2,1,2,5))


### plot for clicker x exam correlations
corrplot.mixed(cor.mat.cli[[1]], upper = "color",  number.cex = .4,cl.lim = c(0, 1), tl.col = "black",
               tl.cex = .4, 
               # change title for correct section
               title = "Correlation Matrix for Exams and Clicker Performance, Sections 001 & 002",
               mar=c(2,1,2,5))


## We don't make mistakes, just happy little accidents.
## Bob Ross