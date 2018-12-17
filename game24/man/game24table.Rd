\name{game24table}
\alias{game24table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{game24table
%%  ~~function to do ... ~~
}
\description{
game24table gives users the table of all the combinations of cards as well as the counts for their number of methods. Users should select any single integer between 1 and 13. And the table is based on all of the combinations that include at least one of the user selected number.
}
\usage{
game24table(x,b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x is an user defined integer between 1 and 13. Each combinations of cards will include at least one of x.
}
  \item{b} {
b is the target. Here it should be 24 if user interested in game24.  
}
}
\details{
Results will be a dataframe with the first 4 columns as the cards and the last column as the counts of methods. Count 0 means there is no way to get the cards to 24.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
table = game24table(7)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
