 
  
  clear
  cd "C:/Users/etien/OneDrive/Documents/GitHub/international_econ/data"
  
  
*****************************************************************
* Homework 1 - Empirical part: market potential and development *
*****************************************************************
 
  use "trade.dta"

* a. Calculate	FMP	of	each	country	in	2016	and	regress	it	on	GDP	per	capita	(in	log-log)	

   
   * Compute log of GDP per capita
   gen gdppc_o = gdp_o / pop_o
   gen gdppc_d = gdp_d / pop_d
   
   gen lgdppc_o = ln(gdppc_o)
   gen lgdppc_d = ln(gdppc_d)
   
   * Regress FMA on GDP per capita in 2016 (log-log)
   
   reg lfma lgdppc_o if year == 2016, robust
   
   
 * b. Replicate	the	exercise	with	all	years	since	2004.	Which	fixed	effects	can	you	now	introduce	in	the	second	step?	does	it	change	results	?		
 
   reg lfma lgdppc_o if year >= 2004, robust
   
   * Adding fixed effects
   
   * COUNTRY (reporter) FE
		* Convert reporter into a labeled numeric variable (factor)
   encode reporter, gen(reporter_id) 
		* Run the regression
   reg lfma lgdppc_o i.reporter_id if year >= 2004, robust
   
   * YEAR FE
		* Run the regression
   reg lfma lgdppc_o i.year if year >= 2004, robust
   
   * COUNTRY + YEAR FE
     reg lfma lgdppc_o i.reporter_id i.year if year >= 2004, robust