# fcl 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* `bond_result()` supports vector inputs.
* `bond_result()` try to return NA for unexpected input or failing to calulcate irr.
* Implement `bond_cf()` which give the forecasting cashflows of a bond.
* Implement `rtn()` which allows for fast TWRR or Modified Return.
* Better handle the case when the deno is zero, for return calculation.
* Dietz return now uses the same day count handling as TWRR, which is assuming positive cash flow occurs at the BOP of the day while negative cash flow occurs at the EOP.
* Use `fixed_bond()` to create bond objects with `$cf()` and `$ytm_dur()` methods.
