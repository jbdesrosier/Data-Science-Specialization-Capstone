#Question 3
blogs <- file(
  'C:/Users/desrjus/Documents/R Files/Coursera/Capstone/Data-Science-Specialization-Capstone/final/en_US/en_US.blogs.txt')

blogs_lines<-readLines(blogs)
close(blogs)
summary(nchar(blogs_lines))

#Question 4
twitter<-file(
  'C:/Users/desrjus/Documents/R Files/Coursera/Capstone/Data-Science-Specialization-Capstone/final/en_US/en_US.twitter.txt'
)
twitter_lines<-readLines(twitter)
love <- length(grep('love', twitter_lines))
hate <- length(grep('hate', twitter_lines))
love/hate

#Question 5
grep('biostats', twitter_lines, value=T)

#Question 6
length(grep('A computer once beat me at chess, but it was no match for me at kickboxing', twitter_lines))
