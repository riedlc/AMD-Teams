############################################################
##			      Information Diversity
##
## Code to compute Information Diversity as used in Riedl & Woolley (2017).
##
## Please cite: Riedl, C., Woolley, A. (2017). "Teams vs. Crowds: A Field Test of the Relative Contribution of Incentives, Member Ability, and Emergent Collaboration to Crowd-Based Problem Solving Performance," Academy of Management Discoveries, in press.
############################################################

# First time you run this, ensure you install all the required packages by running the following
# install.packages(c("topicmodels", "RTextTools", "slam", "lsa", "tm"))

# Load required libraries
library(topicmodels)		# For LDA()
library(RTextTools)			# For create_matrix()
library(slam)				# For row_sums()
library(lsa)				# For cosine()
library(tm)					# For stopwords()

# Configuration
k <- 25				# Number of topics to model
SEED <- 02138		# Seed for random number generation to ensure reproducibility

############################################################
## Information Diversity
##
## lda_gibbs: an object of class "LDA", returned from LDA(x)
## groupIDs: indicators of which text documents belong together (e.g., team IDs)
##           Information Diversity is computed across the messages within a group
############################################################
getInformationDiversity <- function(lda_gibbs, groupIDs) {
	# Extract posterior distributions
	lda_inf <- posterior(lda_gibbs)

	# Get the topic weights from the lda object
	weights <- as.data.frame(lda_inf$topics)
	
	# How many topics are in the model?
	k <- ncol(weights)
	
	# Add groupIDs as a new column
	weights$GroupID <- groupIDs

	# Create group averages (M)
	M   <- aggregate( weights[as.character(1:k)], weights[c("GroupID")], mean)

	# Full Information Diversity equation from Wu2013, p38, right column
	id <- data.frame()
	for(i in M$GroupID) {							# For each group
		Mi <- M[M$GroupID==i, as.character(1:k)]
		mj <- weights[weights$GroupID==i, as.character(1:k)]
	
		# For each message within group
		sum <- 0
		for(j in 1:nrow(mj)) {
			sum <- sum + ( 1 - cosine( as.numeric(mj[j,]), as.numeric(Mi) ) )^2
		}
		
		id <- rbind(id, data.frame(GroupID=i, InformationDiversity=(sum/nrow(mj)) ) )
	}
	return(id)
}


###############################
## Illustration 1: 
##
## LDA Example: As running example, we'll use the NYTimes dataset part of the {RTextTools}
## The text consists of Title and Subject of NYTimes articles. Each article is manually coded with a subject. We will compute the 
## Information Diversity of different topics. To speed up computations, we'll use only a random sample of 100 documents from the dataset.
###############################

# Set random seed so that the example is reproducible
set.seed(SEED)

# Load NYTimes dataset and pull a random sample of 100 articles
data(NYTimes)
data <- NYTimes[sample(1:nrow(NYTimes), size=100, replace=FALSE),]

# Turn the 100 articles into a document-term matrix
matrix <- create_matrix(cbind(data["Title"], data["Subject"]), language="english", removeNumbers=TRUE, stemWords=FALSE, weighting=weightTf)

# Fit the LDA topic model to the documents in a document matrix
gibbs <- LDA(matrix, k=k, method="Gibbs", control = list(seed=SEED, burnin=1000, thin=100, iter=1000))

# Print the 10 most likely terms from each topic
terms <- terms(gibbs, 10)
terms

infoDiv <- getInformationDiversity(gibbs, data$Topic.Code)
infoDiv$NumDocuments <- table(data$Topic.Code)
infoDiv

# Note: ID for GroupID=4 is 0 because the group contains only a single document. 
# Hence the distance between that one document and the average in the group (which is the same document) is zero.

###############################
## Illustration 2: 
##
## To create a document-term matrix for your own documents, follow the below steps.
###############################

# Say your documents are contained in the vector "documents"
documents <- c("This is the text of the first document.", "This is the text of the second document.", "This is the text of the third document. Numbers and punctuation are dropped $123,456.")

# Turn documents into a document-term matrix, removing stop words, performing stemming, and removing numbers and punctuation
matrix <- create_matrix( documents, language="english", removePunctuation=TRUE, removeStopwords=stopwords("en"), minWordLength=2, removeNumbers=TRUE, stemWords=TRUE, toLower=TRUE, weighting=weightTf)
matrix <- removeSparseTerms(matrix, 0.999)
matrix

