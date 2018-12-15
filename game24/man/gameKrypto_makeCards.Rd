\name{gameKrypto_makeCards}
\alias{gameKrypto_makeCards}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
gameKrypto_makeCards
}
\description{
This function allows you to play the famous card game Krypto on the console.It gives the user four random cards  along with a fifth random objective card between 1-13 and allows the user to input when they would like to see solutions.
}
\usage{
gameKrypto_makeCards()
}
%- maybe also 'usage' for other objects documented here.
\details{
 If you enter the number other than the objective card when you want the solution, the warning of "That was not the objective card. Please enter the objective card." will pop up. If there is no solution for the random four numbers to get the objective card, you will get "You cannot make x with these cards." where x is the number of objective card.
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
gameKrypto_makeCards()
  "Make the objective card from these cards: 11 9 12 10"
  "Your objective card is: 8"
  Enter the objective card when you want solutions: 8
  "Method  1 : ( 11 - 9 ) = 2 then 2 * 10 = 20 then ( 20 - 12 ) = 8"
}
