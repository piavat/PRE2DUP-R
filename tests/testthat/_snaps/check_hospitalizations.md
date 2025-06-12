# check_hospitalizations detects missing arguments

    Code
      check_hospitalizations(hospital_data, date_range = c("2026-01-01", "2024-02-01"))
    Condition
      Error:
      ! arguments 'hosp_person_id', 'hosp_admission', 'hosp_discharge' are missing from function call. 

---

    Code
      check_hospitalizations(hospital_data, hosp_person_id = "PID")
    Condition
      Error:
      ! arguments 'hosp_admission', 'hosp_discharge' are missing from function call. 

---

    Code
      check_hospitalizations(hospital_data, hosp_admission = "Entry", hosp_discharge = "Leave",
        date_range = c("2026-01-01", "2024-02-01"))
    Condition
      Error:
      ! argument 'hosp_person_id' is missing from function call. 

# check_hospitalizations detects missing data columns

    Code
      check_hospitalizations(hospital_data, hosp_person_id = "PID", hosp_admission = "Entry",
        hosp_discharge = "Leave")
    Condition
      Error:
      ! column 'Entry' is missing from the dataset. 

---

    Code
      check_hospitalizations(hospital_data, hosp_person_id = "PID", hosp_admission = "Entry",
        hosp_discharge = "Leave")
    Condition
      Error:
      ! columns 'Entry', 'Leave' are missing from the dataset. 

# check_hospitalizations detects errorneus date ranges

    Code
      check_hospitalizations(hospital_data, hosp_person_id = "PID", hosp_admission = "Entry",
        hosp_discharge = "Leave", date_range = c("2026-01-01", "2024-02-01"))
    Condition
      Error:
      ! starting date must be before ending date in date range.

---

    Code
      check_hospitalizations(hospital_data, hosp_person_id = "PID", hosp_admission = "Entry",
        hosp_discharge = "Leave", date_range = c(""))
    Condition
      Error:
      ! two valid dates required for date range.

---

    Code
      check_hospitalizations(hospital_data, hosp_person_id = "PID", hosp_admission = "Entry",
        hosp_discharge = "Leave", date_range = c("2026-01-01"))
    Condition
      Error:
      ! two valid dates required for date range.

# check_hospitalizations detects dates outside date ranges

    Code
      check_hospitalizations(hospital_data, hosp_person_id = "PID", hosp_admission = "Entry",
        hosp_discharge = "Leave", date_range = c("2020-01-01", "2024-02-01"))
    Message
      Column 'Entry' has values outside date range in 1 rows: 1
      Column 'Leave' has values outside date range in 1 rows: 4
    Condition
      Error:
      ! Errors in dataset assigned to 'hospital_data'. See listing above for details.

# check_hospitalizations detects dates in wrong order

    Code
      check_hospitalizations(hospital_data, hosp_person_id = "PID", hosp_admission = "Entry",
        hosp_discharge = "Leave", date_range = c("2020-01-01", "2024-02-01"))
    Message
      Column 'Entry' has admission date later or in a same date than discharge date in 1 rows: 3
    Condition
      Error:
      ! Errors in dataset assigned to 'hospital_data'. See listing above for details.

# check_hospitalizations detects overlapping hospitalizations

    Code
      check_hospitalizations(hospital_data, hosp_person_id = "PID", hosp_admission = "Entry",
        hosp_discharge = "Leave", date_range = c("2020-01-01", "2024-02-01"))
    Message
      Checks passed for 'hospital_data'

---

    Code
      check_hospitalizations(hospital_data, hosp_person_id = "PID", hosp_admission = "Entry",
        hosp_discharge = "Leave")
    Message
      Checks passed for 'hospital_data'

---

    Code
      check_hospitalizations(test_data, hosp_person_id = "pid", hosp_admission = "admission",
        hosp_discharge = "discharge")
    Message
      Checks passed for 'test_data'

---

    Code
      outdata <- check_hospitalizations(test_data, hosp_person_id = "pid",
        hosp_admission = "admission", hosp_discharge = "discharge", return_data = TRUE)
    Message
      Checks passed for 'test_data'
      Preparing hospitalization data and merging overlapping hospitalizations.
      Number of overlapping hospitalizations detected and and will be merged:  7

# check_hospitalizations reports error if the hospitalization data has not records

    Code
      check_hospitalizations(hospital_data, hosp_person_id = "PID", hosp_admission = "Entry",
        hosp_discharge = "Leave")
    Condition
      Error:
      ! No data in the dataset 'hospital_data'.

