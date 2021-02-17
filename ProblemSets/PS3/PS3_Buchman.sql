/* A) Reading the CSV into SQL*/
mode csv
.import FL_insurance_sample.csv Insurance

/* B) Printing out first 10 rows*/
SELECT *FROM Insurance ORDER BY policyID LIMIT 10;

/* C) Unique counties in the dataset*/
SELECT DISTINCT county 
   ...> FROM Insurance;

/* D) Computing average property appreciation*/
SELECT AVG(tiv_2012 - tiv_2011) as Difference FROM Insurance;

/* E) Creating a frequency table*/
SELECT construction, COUNT(construction) AS Frequency
   ...> FROM Insurance
   ...> GROUP BY construction
   ...> ORDER BY
   ...> COUNT(construction) DESC;