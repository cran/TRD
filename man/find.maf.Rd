\name{find.maf}
\alias{find.maf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find MAF of sample of case-trios and control-trios
}
\description{
Compuate MAF in case child, control child, parents of cases and controls, all affected, all unaffected, and whole sample
}
\usage{
find.maf(sample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{a dataset with case- and control-trios with first column as case status (1=case, 0=control), 2nd maternal genotype, 3rd paternal genotype, 4th child genotype.
%%     ~~Describe \code{sample} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{case-child}{MAF of case children only}
\item{ctrl-child}{MAF of control childrens only}
\item{case-parent}{MAF of parents of cases only}
\item{ctrl-parent}{MAF of parents of controls only}
\item{case}{MAF of affected only}
\item{ctrl}{MAF of unaffected only}
\item{sample}{MAF of whole sample}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Lam Opal Huang
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rtrios}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
trios=rtrios(1000,50,0.1,0.2,0.3,0.5,0.5,0.1)
find.maf(rbind(trios[[1]],trios[[2]]))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
