# LR Multiclass tests

# Load the letter data
#########################
letter <- read.table("R_homework_functions/Data/letter-train.txt", header = F, colClasses = "numeric")
Y <- letter[, 1]
X <- as.matrix(letter[, -1])

sourceCpp('src/LRMultiClass.cpp')