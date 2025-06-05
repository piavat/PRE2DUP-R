---
title: "News"
author: "Pia Vattulainen"
date: "2025-05-28"
output: html_document
---


# PRE2DUPR 0.1.0

## Initial release

- First release of the package.
- Provides tools for creating drug use periods from medication purchase data.
- Includes also functions for checking and validating the data program uses.


# PRE2DUPR 0.1.1
- Bug fixes and improvements: 
  * Drug exposure end for 1–2 purchase periods now always uses estimated refill length (no dynamic adjustment like 3+). 
  * Hospital days between current and following period, that did not start during purchase exposure time, are now excluded from period's last purchase duration.

---

Planned updates: Documentation of the PRE2DUPR methodology

