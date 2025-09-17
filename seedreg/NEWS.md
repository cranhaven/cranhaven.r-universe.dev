# seedreg 1.0.3

* Fixed error in `multicurve` function 

* Add `seeds`, `correl`, `curve`, `lineplot`, `iv`, `aac`, `tml` and `tm` function

* Add `substrate` dataset

# seedreg 1.0.2

* `pointsize`, `linesize` and `pointshape` was added in `LM_model`, `BC_model`, `CD_model`, `loess_model`, `normal_model`, `LL_model` and `piecewise_model`

* `geom` argument was added in `quali_model`

* Minimal temperature and Minimal temperature response was added.

* The coefficients now have an asterisk (`*`) symbol in the case of significant and `ns` in the case of not significant.

* The multicurve function has been improved. The `method` argument was added and the `gray` argument was discontinued. See in detail to define the `method`.


# seedreg 1.0.1

* Fixed error bar width failure. It is now possible to define the width of the error bars by the `width.bar` argument. By default, it makes the width equivalent to 0.01 times the mean of the independent variable.

* RMSE (root mean squared error) value was added.

* Added caption for `N_model` function

* Added the variable `vel` to the `aristolochia` dataset. It represents the variable germination speed. The variable `resp` was changed to `germ`.
