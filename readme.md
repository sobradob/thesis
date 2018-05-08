

# Missing data in GPS measurements

## Metadata

Author: Boaz Sobrado - [www.boazsobrado.com]
Supervisor: Dr. Peter Lugtig - [https://www.uu.nl/staff/plugtig]

## Abstract

This is the research archive necessary to make my thesis reproducible. The abstract of my thesis can be read below:

>   Personal mobility, or how people move in their environment, is associated with a vast range of behavioural traits and outcomes, such as socioeconomic status, personality and mental health. The widespread adoption of location-sensor equipped smartphones has generated a wealth of objective personal mobility data. Nonetheless, smartphone collected personal mobility data has remained underutilised in behavioural research, partly due to the practical difficulties associated with obtaining the data and partly because of the methodological complexity associated with analysing it. Recent changes in European regulation have made it easier for researchers to obtain this data, but the methodological difficulties remain. The difficulty lies in that smartphone location data is irregularly sampled, sparse and often inaccurate.  This results in a high proportion of missing data and significant noise. In this paper we present a method called Personal Map Matched Imputation (PPMI) to deal with missing data and noise in smartphone location logs. The main innovation of PPMI is that it creates a personalised spatial map for each individual based on all the available data. In doing so PPMI leverages the regularity of human mobility in order to smoothen noisy measurements and impute missing data values. By simulating missing periods in real data we find that a simple implementation of PPMI performs as well as existing methods for short (5 minute) missing intervals and substantially better for longer (1 day) missing intervals. When imputing a subset of real missing data where travel logs are available as a reference points, we find that PPMI performs substantially better than existing models.

## Contents

This archive contains:

1. The files needed to render the pdf version of the manuscript.
2. The scripts and functions needed to reproduce or replicate the results.

This archive does not contain:

1. Sensitive personal data, including smartphone location logs.
2. Scripts written by others but nonetheless used in preparing the thesis, such as the MATLAB scripts of Palmius & the functions prepared by Barnett & Onnella. 


## Structure

Files are generally structured to contain a main scripts, a folder of functions and a folder of scripts. The main scripts and the functions are the ones necessary to reproduce all results. The folder of scripts contains exploratory scripts written to wrangle data, test sub-hypotheses and contain research dead-ends that did not lead to desired results. What did work was prepared in the form of functions within the functions folders. 

## Organisation 

The files in this repository are:

1. Correspondence: emails exchanged about the state of the thesis at different points in time.
2. Palmius Implementation: Implementing the method of Palmius and cross-validation results.
3. PPMI: PPMI implementation and cross-validation results. 
4. Barnett & Onnella implementation: implementation and cross-validation results.
5. Objective results: all implementations of objective results.
6. Aggregate results:: all implementations of the aggregation example (time spent at home).
7. Scripts: used to extract features, create images, process data, etc. . 
8. Thesis Manuscript: the files necessary to produce the thesis text as well as the thesis text.
9. Thesis Report: the files necessary to reproduce the thesis report submitted in 2017
10. Pre-processing: the script needed to take raw JSON data from Google Location Services and convert it to an R data frame.

## Privacy:

The data used in this study is highly sensitive personal data, as it contains all location measurements of one individual's smartphone for a span of several years. This data can be used to extract personal information, or can be used to link publicly available anonymised information to an individual. The individual at risk has given a full informed consent to the researchers to process the data for research purposes.  There is no need for this to be documented as the individual providing the data is also the author. Given that it is not feasible to anonymise personal location data, and the subject does not consent to the publication of his data, the data is not been shared. We'd like to point  prospective researchers to [Google takeout](https://takeout.google.com/) where they can download their own location history or that of volunteers.

## Storage responsibilities 

The supervisor is responsible for storing the archive. In addition this archive is publicly available on an [online repository](https://github.com/sobradob/thesis) indefinitely. 