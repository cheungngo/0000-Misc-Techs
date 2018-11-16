# Tutorial on meta-analysis in R
# R useR! Conference 2013
# Stephanie Kovalchik


###

# Searching the literature with R

###


# Importing PubMed Data with RISmed


install.packages('RISmed')
library(RISmed)


# Create EUtilsSummary object for specified query
# EUtilsSummary(), An introduction

?EUtilsSummary
# EUtilsSummary(query,type="esearch",db="pubmed",url=NULL,encoding="unknown",...)

# db=''
# - Entrez Unique Identifiers (UIDs) for selected databases
# Entrez Database	UID common name	E-utility Database Name
# BioProject	BioProject ID	bioproject
# BioSample	BioSample ID	biosample
# Biosystems	BSID	biosystems
# Books	Book ID	books
# Conserved Domains	PSSM-ID	cdd
# dbGaP	dbGaP ID	gap
# dbVar	dbVar ID	dbvar
# Epigenomics	Epigenomics ID	epigenomics
# EST	GI number	nucest
# Gene	Gene ID	gene
# Genome	Genome ID	genome
# GEO Datasets	GDS ID	gds
# GEO Profiles	GEO ID	geoprofiles
# GSS	GI number	nucgss
# HomoloGene	HomoloGene ID	homologene
# MeSH	MeSH ID	mesh
# NCBI C++ Toolkit	Toolkit ID	toolkit
# NCBI Web Site	Web Site ID	ncbisearch
# NLM Catalog	NLM Catalog ID	nlmcatalog
# Nucleotide	GI number	nuccore
# OMIA	OMIA ID	omia
# PopSet	PopSet ID	popset
# Probe	Probe ID	probe
# Protein	GI number	protein
# Protein Clusters	Protein Cluster ID	proteinclusters
# PubChem BioAssay	AID	pcassay
# PubChem Compound	CID	pccompound
# PubChem Substance	SID	pcsubstance
# PubMed	PMID	pubmed
# PubMed Central	PMCID	pmc
# SNP	rs number	snp
# SRA	SRA ID	sra
# Structure	MMDB-ID	structure
# Taxonomy	TaxID	taxonomy
# UniGene	UniGene Cluster ID	unigene
# UniSTS	STS ID	unists

# Limits that can be supplied to ... to refine the query include:

# reldate	 Limits search results to be within the specified number of days from current date.
# mindate	 Minimum of date range for search results (examples: 2002; 2002/01/01); must be supplied with maxdate.
# maxdate	 Maximum of date range for search results; must be supplied with mindate.
# datetype	 Which date field to use in setting date limits. Possible choices are edat, Entrez date, which is the date article was added to Entez, or ppdt, the article publication date. Default is edat.
# retstart	 Where in the sequence of returned results to begin retrieving, default is 0.
# retmax	 Maximum number of records to retrieve, default is 1000.


# Query in the EUtilsSummary()

query = '(TMS OR transcranial magnetic stimulation) AND (OCD OR obsessive compulsive disorder)'
fit = EUtilsSummary(query=query,db='pubmed')


# Basic stats for the EUtilsSummary object

class(fit)
# [1] "EUtilsSummary"
# attr(,"package")
# [1] "RISmed"

QueryTranslation(fit)
# [1] "((\"Symp Theory Model Simul\"[Journal] OR \"tms\"[All Fields]) OR (\"transcranial magnetic stimulation\"[MeSH Terms] OR (\"transcranial\"[All Fields] AND \"magnetic\"[All Fields] AND \"stimulation\"[All Fields]) OR \"transcranial magnetic stimulation\"[All Fields])) AND (OCD[All Fields] OR (\"obsessive-compulsive disorder\"[MeSH Terms] OR (\"obsessive-compulsive\"[All Fields] AND \"disorder\"[All Fields]) OR \"obsessive-compulsive disorder\"[All Fields] OR (\"obsessive\"[All Fields] AND \"compulsive\"[All Fields] AND \"disorder\"[All Fields]) OR \"obsessive compulsive disorder\"[All Fields]))"

QueryCount(fit)
# [1] 192


# Extracting the data
# EUtilsGet()

fetch = EUtilsGet(fit)
fetch
# PubMed query: ...
# Records:  192

# fetch being a Medline Object
# methods for an medline object:
getSlots('Medline')
# ...


# Accessing the medline object
# Author(), YearPubmed(), PMID(), AbstractText()

Author(fetch)[1]
# [[1]]
# LastName  ForeName Initials order
# 1    Singh Swarndeep        S     1
# 2    Kumar   Saurabh        S     2
# 3    Gupta     Ankit        A     3
# 4    Verma     Rohit        R     4
# 5    Kumar      Nand        N     5
auth_fetch = Author(fetch)
auth_fetch
LastFirst = sapply(auth_fetch, function(x) paste(x$LastName,x$ForeName))
LastFirst

year_fetch = YearPubmed(fetch)
table(year_fetch)
# 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 
#    1    2    1    2    4    3    6    3    3    8    3    4    5    8    7    9   10 
# 2013 2014 2015 2016 2017 2018 
#   18   19   22   18   23   13
plot(table(year_fetch))
# plot 01

abs_fetch = AbstractText(fetch)
abs_fetch
write.table(abs_fetch, '~/OCDTMS.txt', sep="\t")

PMID_fetch = PMID(fetch)

title_fetch = Title(fetch)
title_fetch[1:3]
# [1] "The journal of ECT"                          
# [2] "NeuroImage. Clinical"                        
# [3] "The primary care companion for CNS disorders"
# ...

Art_ti_fetch = ArticleTitle(fetch)
Art_ti_fetch[1]
# [1] "Effectiveness and Predictors of Response to 1-Hz Repetitive Transcranial Magnetic Stimulation 
# in Patients With Obsessive-Compulsive Disorder."  


# Getting a summary text file for manual screening

sum_fetch = data.frame(Art_ti_fetch, title_fetch, year_fetch, PMID_fetch, abs_fetch)
sum_fetch_l = list(sum_fetch)
write.table(sum_fetch_l, '~/OCDTMS.txt', sep="\t")


# Other techniques in the tutorial

# 1. The first year a matching article appeared
min(year_fetch)
# [1] 1996

# 2. What was the title of this article
ArticleTitle(fetch)[which(YearPubmed(fetch)==1996)]
# [1] "Transcranial magnetic stimulation: a neuropsychiatric tool for the 21st century."

# 3. Do some authors have multiple matching records?
sort(table(unlist(LastFirst)),dec=T)[1:5]
#       Hollander Eric    Mantovani Antonio    Fitzgerald Paul B      Lisanby Sarah H 
#                    9                    9                    7                    7 
# Daskalakis Zafiris J 
#                    6


###