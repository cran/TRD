\name{TRD-package}
\alias{TRD-package}
\alias{TRD}
\docType{package}
\title{
Transmission Ratio Distortion
}
\description{
Transmission Ratio Distortion (TRD) is a genetic phenomenon where two alleles from either parent are not transmitted to the offspring at the expected 1:1 ratio under Mendelian inheritance. Occurrence of TRD in general population can lead to spurious association signals for the study on case populations, including genotype relative risk (GRR)  and imprinting effect. Therefore, it is necessary to adjust for TRD in association studies when TRD is suspected to be present.


This package has implemented the loglinear model [Weinberg et al. 1998], the adjusted loglinear model for non-sex-of-parent-specific TRD (NST) [Huang et al. 2014] and sex-of-parent-specific TRD (ST) [Huang et al. 2015]; Transmission Disequilibrium Test(TDT)[Spielman et al. 1993], and adjusted TDT [Labbe et al. 2013].


A simulation function is included in the package, which generates a population of case- and control-trios, with parent-of-origin information for disease allele in heterozygous child (rtrios), based on penetrance, ratio of maternal to paternal-induced disease risk for a heterozygous child, transmission probability of minor allele for mother and father, and minor allele frequency. Population and sample sizes can be specified by user. The data can be used directly to fit loglinear model (ll) or to perform a TDT (tdt) with or without adjustment for TRD.  Count data for the 15/16 mother-father-child category can also be computed (strata.cnt).


The loglinear model can take input as trios or count data with or without parent-of-origin information. Types of loglinear model which can be fitted include genotype relative risk, gene-dosage, dominant models, with (grri, gdi, domi) or without (grr, gd, dom) imprinting effect. Real dataset of same structure can also be used to fit all types of loglinear model. Other functions are available to calculate population information, such as genotype counts (gcount), minor allele frequency (find.maf), transmission probability of minor allele (find.t), and theoretical prevalence of disease (prev). Note that trios with missing data are discarded when fitting the loglinear models.
}
\details{
\tabular{ll}{
Package: \tab TRD\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2015-11-10\cr
License: \tab 3.2.2\cr
}
}
\author{
Lam Opal Huang

Maintainer: Lam Opal Huang <opal.huang@mail.mcgill.ca>
}
\references{
Huang LO, Infante-Rivard C, Labbe A, (2015). Modeling sex-of-parent-specific Transmission Ratio Distortion and imprinting effect in loglinear model using case-trios.(Working manuscript)

Huang LO, Infante-Rivard C, Labbe A, (2015). Adjustment for Transmission Ratio Distortion in the analysis of case-parent trios using a loglinear model.(Working manuscript)

Labbe A, Huang LO, Infante-Rivard C (2013). Transmission Ratio Distortion: A Neglected Phenomenon with Many Consequences in Genetic Analysis and Population Genetics:Naumova AK and Greenwood CMT (eds.), Epigenetics and Complex Traits: 265-85, Springer.

Huang LO, Labbe A, Infante-Rivard C (2012). Transmission ratio distortion: review of concept and implications for genetic association studies. Human Genetics 132: 245-63.

Weinberg CR, Wilcox AJ, Lie RT (1998) A log-linear approach to case-parent-triad data: assessing effects of disease genes that act either directly or through maternal effects and that may be subject to parental imprinting. American Journal of Human Genetics 62: 969-78.

Spielman RS, McGinnis RE, Ewens WJ (1993) Transmission test for linkage disequilibrium: the insulin gene region and insulin-dependent diabetes mellitus (IDDM). American Journal of Human Genetics 52: 506.

}
