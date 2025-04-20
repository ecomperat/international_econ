
   clear
   capture log close
   set matsize 800
   set mem 300m
   set more off
   version 7.0

   *** This file is based on exports data from a reporter to a partner for a single year
   *** N.B. There are no observations where reporter==partner in the dataset used below

 //  log using "intecon_h1.log", replace

   * Read data

   clear
   u "C:/Users/etien/OneDrive/Documents/GitHub/international_econ/data/biltrade.dta"
   
   cd "C:/Users/etien/OneDrive/Documents/GitHub/international_econ/data"
 
 
   * Rename columns for coding
   rename iso_o reporter
   rename iso_d partner
   rename flow exports
   rename distw distance
   rename contig border
   
   
   so reporter partner year

   di "*****************************************"
   di "/* Transform variables for regressions */"
   di "*****************************************"

   egen totexp=sum(exports),by(reporter year)
   so reporter partner year

   gen lexport=ln(exports+1)

   replace distance=distance/1000
   gen ldist=ln(distance)
   

   di "***********************************************************************"
   di "**** Investigate sample and define appropriate indicator variables ****"
   di "***********************************************************************"

   encode reporter, gen(cty)
   encode partner, gen(ptn)

   di "******************************************************************"
   di "**** Count number of reporters and partners for dummies later ****"
   di "******************************************************************"

   egen temp=count(cty),by(partner year)
   so reporter partner year
   egen n_cty=max(temp)
   drop temp

   egen temp=count(ptn),by(reporter year)
   so reporter partner year
   egen n_ptn=max(temp)
   drop temp

   di "**********************"
   di "/* Generate Dummies */"
   di "**********************"

   tab reporter, gen(cc)
   tab partner, gen(pp)

   di "***************************************************"
   di "/* Run regressions                               */"
   di "/* Select the specification that you want to run */"
   di "***************************************************"

   di "************************"
   di "**** Normalizations ****"
   di "************************"

   * Add 1 to n_ptn and n_cty because of no cty==ptn observations in egen commands above

   global ncty = n_cty+1
   global nptn = n_ptn+1

   di "********************************************************"
   di "***** Normalise relative to one country (here USA) *****"
   di "********************************************************"

   list cty cc1 cc2 cc3 if reporter=="AUS"&partner=="GBR"
   list ptn pp1 pp2 pp3 if partner=="AUS"&reporter=="GBR"
   * Dropping the USA: cc196
   drop cc196
   reg lexport ldist border cc1-cc195 cc197-cc$ncty pp1-pp$nptn , robust noconstant

   di "***************************************************"
   di "**** Predictions based on chosen specification ****"
   di "***************************************************"

   gen sam=0
   replace sam=1 if e(sample)

   predict lpred , xb
   gen pred=exp(lpred) 

   di "**********************************************"
   di "**** Construct Market and Supplier Access ****"
   di "**********************************************"

   * Save coefficient on distance
   gen de1=_b[ldist]
   gen de2=_b[border]

   * dm_p is domestic market capacity prior to own trade cost weighting
   * ds_c is domestic supply capacity prior to own trade cost weighting
   * _p indicates a variable that varies in partner space
   * _c indicates a variable that varies in reporter space

   gen trans=0
   gen dm_p=0
   gen suppl=0
   gen ds_c=0

   local x = 1
   
   while `x'<=$nptn {

   di `x'

   gen ppp`x'= _b[pp`x']*pp`x'
   replace trans=trans+ppp`x'
   replace dm_p=dm_p+ppp`x'

   if `x'~=196 {gen pcc`x'= _b[cc`x']*cc`x'}
   if `x'~=196 {replace suppl=suppl+pcc`x'}
   if `x'~=196{replace ds_c=ds_c+pcc`x'}

   local x = `x'+1
   }

   di "******************************************************************"
   di "**** Continue with construction of market and supplier access ****"
   di "******************************************************************"

   replace trans=trans+(_b[ldist]*ldist)+(_b[border]*border)
   gen edm_p=exp(dm_p)

   replace suppl=suppl+(_b[ldist]*ldist)+(_b[border]*border)
   gen eds_c=exp(ds_c)

   * Generate transportation weights

   gen t_rs=(ldist^_b[ldist])*(border^_b[border])

   * For foreign market access, sum across importers (partners's) for each exporter

   gen etrans=exp(trans)

   egen fma=sum(etrans),by(reporter year)
   so reporter partner year

   * For foreign supplier access, sum across exporters (reporters's) for each importer

   gen esuppl=exp(suppl)

   egen fsa=sum(esuppl),by(partner year)
   so reporter partner year

   di "*************************************************"
   di "**** Transpose variables into reporter space ****"
   di "*************************************************"

   * Need to transpose the foreign supplier access vector (sa) into reporter space *
   * Need to transpose the domestic market demand vector (dm_p) into reporter space *
   * The variables fma and ds_c are already in reporter space *

   so reporter partner year
   sa trade.dta,replace

   keep reporter partner year fsa dm_p edm_p 
   ren reporter steve
   ren partner reporter
   ren steve partner
   ren dm_p dm_c
   ren edm_p edm_c 
   so reporter partner year
   sa temp.dta,replace

   clear
   u trade.dta
   drop fsa 
   merge reporter partner year using temp.dta
   so reporter partner year
   tab _merge
   drop _merge

   * Transform variables

   gen lfma=ln(fma)
   gen lfsa=ln(fsa)

   
   drop cc* pp* pcc* ppp* cty ptn 
 

  
   ** N.B. The dataset is still bilateral reporter-partner
   ** But the market and supplier access variables below only 
   ** vary in reporter space

   lab var fma "Foreign Market Access"
   lab var fsa "Foreign Supplier Access"
   
   ** Drop for size
   drop n_cty-etrans
   drop esuppl-edm_c
   drop fsa lfsa

   di "*******************"
   di "**** Save data ****"
   di "*******************"


   so reporter partner year
   sa trade.dta,replace

   capture log close