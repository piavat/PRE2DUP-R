# PRE2DUPR 0.2.0
* Added new argument to define the function output: users can now choose to return exposure periods, updated package parameters, or both. Argument calc_pack_dur_usual removed.
  * Replaced interactive prompt for handling ATC codes lacking DDD records or package parameters coverage with a function argument, allowing automated processing.
  * Improved handling of short purchase sequences: for drug use periods based on fewer than three purchases, purchase duration estimates are now always derived from package-based parameters.
  * Improved usual package duration calculation: the algorithm now prefers combining with the closest package duration rather than the one with the highest frequency within two days.  
- Improvements to documentation.

# PRE2DUPR 0.1.2
- Bug fixes: 
  * In correction of overlapping hospitalizations program could not handle cases where there were multiple consecutive hospitalizations for the same person. A package `intervals` was added for merging.
  * A correction in calculation of hospital days in the drug use periods (the result) data.  
- Improvements to introduction.

# PRE2DUPR 0.1.1
- Bug fixes and improvements: 
  * Drug exposure end for 1–2 purchase periods now always uses estimated refill length (no dynamic adjustment like 3+). 
  * Hospital days between current and following period, that did not start during purchase exposure time, are now excluded from period's last purchase duration.


# PRE2DUPR 0.1.0
- First release of the package.
- Provides tools for creating drug use periods from medication purchase data.
- Includes also functions for checking and validating the data program uses.

---

Planned updates: Documentation of the PRE2DUPR methodology

