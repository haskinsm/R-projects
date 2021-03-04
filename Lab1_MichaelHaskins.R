#summary(Lab1$AGE) #see summary statistics

############### PART 1
animals = c('Shark', 'Whale', 'Stingray') ## my animal vector

## Below are my animal facts for each animal contained in a vector
sharkF = c('typically works a stable 9-5 job', 'swims about')
whaleF = c('is often known to have a whale of a time', 'is quite big')
StingrayF = c('can kill', 'is the class teacher in finding Nemo')



################# PART 2
print(sharkF[2]) ##Test print of shark fact vector
# paste( animals[2], sharkF[2]) ## merge two strings
# cat( animals[2], "\n", sharkF[2]) ## line break merge




################ PART 3, 4 & 5
for( i in 1:100){ ## For loop that will run 100 times
  
  randomInd = sample( 1:3, size=1, replace=TRUE) ## random index generator for animal vector
  randomFactInd = sample( 1:2, size=1, replace=TRUE) ## randomindex generator for fact vector
  
  if( randomInd == 1){ ## IF true proceed to random shark fact
    answerString = paste('Did you know that a ', animals[randomInd], ' ', sharkF[randomFactInd], '?', sep = "" ) ## Merges strings and stors them in answer String which will later be printed
  } else if( randomInd == 2){ ## If true proceed to random whale fact
    answerString = paste('Did you know that a ', animals[randomInd], ' ', whaleF[randomFactInd], '?', sep = "" )
  } else if( randomInd == 3){ ## If true proceed to random stingray fact
    answerString = paste('Did you know that a ', animals[randomInd], ' ', StingrayF[randomFactInd], '?', sep = "")
  }
  cat(answerString, "\n") ##Answer String printed and move to next line
}

