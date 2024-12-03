# **NEWS about the FuzzyStatTraEOO package for R**

## **FuzzyStatTraEOO 1.0**

### *Breaking changes*

-   `FuzzyNumber` class' `fnAlphaLevels` private field has been replaced by three attributes `alphaLevels`, `infimums` and `supremums` which corresponds with the first, second third column of a fuzzy number respectively.

-   `FuzzyNumber` class' `getFnAlphaLevels` public method has been replaced by three methods `getAlphaLevels`, `getInfimums` and `getSupremums` which returns the corresponding class' attributes.

-   `FuzzyNumber` class' `checkValidity` private method has a new parameter `fnLevels` that corresponds with the numbers specified and given by the current constructor (`initialize`). That parameter is used as the previous attribute `fnAlphaLevels` in the body method.

-   `FuzzyNumber` class' `checkValidity` private method does not generate a new object of class `FuzzyNumber` if it is not valid, in that case an error is thrown.

-   `FuzzyNumber` and `TrapezoidalFuzzyNumber` classes' `valid` private field has been removed as no invalid objects can be generated. Therefore, the public method `is_valid` has been removed too.

-   `FuzzyNumber` class' current constructor (`initialize`) now sets all its attributes if the `checkValidity` method does not thrown an error.

-   The public field `numbers` at `FuzzyNumberList` and `TrapezoidalFuzzyNumberList` classes has become a private field.

-   The public method `checking` at `TrapezoidalFuzzyNumberList` and `StatList` classes has been removed. That method at `FuzzyNumberList` class has become a private method. In that last class, its parameter has changed from `verbose` to `numbers`, which corresponds with the numbers specified and given by the current constructor (`initialize`).  method only checks if the alpha-levels of the numbers are equal, apart from setting the class' attributes.

-   `FuzzyNumberList` class' current constructor (`initialize`) instead of setting the class' attributes, it calls the private `checking` method.

-   `TrapezoidalFuzzyNumber` class' `checkValidity` private method has been removed. Its functionality has been moved to the current constructor (`initialize`). In case of trying to generate a new invalid object of class `TrapezoidalFuzzyNumber`, an error is thrown.

-   The programming of classes `FuzzyNumberList`, `TrapezoidalFuzzyNumberList`, `Simulation` and `Utils` have changed in accordance with the current `numbers` **private** field and the **new** `getLength` method.

-   Titles and descriptions of the `FuzzyNumber`, `TrapezoidalFuzzyNumber`, `FuzzyNumberList` and `TrapezoidalFuzzyNumberList` has been updated and modified in accordance with the validity of the numbers.

-   The documentation of the public fields and methods have been updated or modified in accordance with the changes made.

-   All tests and the required examples have been updated or modified in accordance with the previous changes.

-   The import from `testhat` in the `FuzzyStatTraEOO-package` file was removed.

-   `testhat` in the `DESCRIPTION` file was moved from `Imports` to `Suggests`.

### *New features*

-   `DESCRIPTION` file was updated as follows:

    -   `Version` field was updated with value 1.0.
    -   `Date` field was updated with the starting date of the new version.
    -   The **vdiffr** package was added on `Suggests` field.
    
-   `FuzzyStatTraEOO-package` file was updated as follows:

    -   `Version` field was updated with value 1.0.
    -   `Date` field was updated with the starting date of the new version.

-   The package can only work with **valid** `FuzzyNumber` and `TrapezoidalFuzzyNumber` object. Therefore, the `FuzzyNumberList` and `TrapezoidalFuzzyNumberList` object can only contain valid numbers.

-   `FuzzyNumber`, `TrapezoidalFuzzyNumber`, `FuzzNumberList` and `TrapezoidalFuzzyNumberList` classes have a new method `plot` in order to show it/them in a graph. The color used can be specified. Therefore, that method has been added to the class `StatList` too.

-   `FuzzNumberList`, `TrapezoidalFuzzyNumberList` and `StatList` classes has a new public method `getLength` to counteract the visibility usage of the `numbers` attribute. It provides a counter of how many `numbers` the classes have.

-   Tests for code lines were developed through the **testthat** and **vdiffr** (used specifically for `plot` methods) packages. These tests provide a 100% coverage as it was calculated by `package_coverage()` function from *covr* package.

### *Bug fixes*

-   The parameter `nl` of `medianWabl`, `media1Norm` and `transfTra` methods of the class `TrapezoidalFuzzyNumberList` was specified and restricted to be greater than 0 but it must be greater or equal to two. 

## **FuzzyStatTraEOO 0.1.0**

Initial release of the **FuzzyStatTraEOO** package on the Comprehensive **R** Archive Network (**CRAN**).
