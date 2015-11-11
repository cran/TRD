\name{find.t}
\alias{find.t}
\title{
Find transmission ratio of minor alleles in mother, father or both parents.
}
\description{
Calculate the transmission ratio of the minor allele from parents to offspring for case- and control-trios, in mother, father, or both parents. Minor allele normally refers to the disease allele, which has lower frequency than the alternative allele(s). Note that trios with missing data are discarded.
}
\usage{
find.t(sample)
}
\arguments{
  \item{sample}{
case- and/or control-trios dataset with first column as disease status (1=case, 0=control), the second, third and forth columns as maternal, paternal and child genotypes. The fifth column, if entered as part of the input data, is the parent-of-origin indicator of disease allele in a heterozygous child (a value of 1 indicates the mother transmitted the disease allele and 0 indicates the father transmitted). The parent-of-origin indicator is only meaningful for a heterozygous child.
}
}
\value{
\item{cases}{  transmission ratio of minor allele from mother, father and both parents of case-trios to the offspring}
\item{ctrls}{  transmission ratio of minor allele from mother, father and both parents of control-trios to the offspring}
}
\author{
Lam Opal Huang
}
\seealso{
\code{\link{rtrios}}
}
\examples{
trios=rtrios(100000,500,0.1,0.2,0.3,1,0.5,0.5,0.1)
find.t(trios$case)
find.t(trios$ctrl)
find.t(rbind(trios$case,trios$ctrl))
}
