
<!-- README.md is generated from README.Rmd. Please edit that file -->

# thaipdf <a href="https://lightbridge-ks.github.io/thaipdf/"><img src="man/figures/logo.png" align="right" height="138"/></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Lightbridge-KS/thaipdf/workflows/R-CMD-check/badge.svg)](https://github.com/Lightbridge-KS/thaipdf/actions)
[![codecov](https://codecov.io/gh/Lightbridge-KS/thaipdf/branch/main/graph/badge.svg?token=QPhwQZt2vf)](https://app.codecov.io/gh/Lightbridge-KS/thaipdf)
[![pkgdown](https://github.com/Lightbridge-KS/thaipdf/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Lightbridge-KS/thaipdf/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

> R package :package: ช่วยสร้าง PDF document จาก R Markdown ที่ใช้
> **ภาษาไทย :thailand:**

| 1                                                   | 2                                                   |
|:----------------------------------------------------|:----------------------------------------------------|
| ![Thai book example 1](man/figures/book-th-ex1.png) | ![Thai book example 2](man/figures/book-th-ex2.png) |

# Goal

R package นี้ช่วย**สร้าง PDF เป็นภาษาไทย :thailand: จาก [R
Markdown](https://rmarkdown.rstudio.com) ได้โดยง่าย**

-   **ผู้เริ่มต้น** สามารถ **knit** เอกสาร R Markdown เป็น PDF
    ได้ทันทีโดยใช้ template ที่มีให้ และ

-   **ผู้ที่ใช้งาน [LaTeX](https://www.latex-project.org) เป็น**
    สามารถเรียกใช้ LaTeX preamble ที่มีการตั้งค่าภาษาไทยมาให้แล้ว มา
    customize เพิ่มเติมอย่างที่ต้องการได้

โดยจะพยามคง option ต่างๆ ของ R Markdown ไว้ให้ได้มากที่สุด
ซึ่งสามารถส่งต่อ YAML parameter ไปยัง R Markdown หรือ bookdown ได้ด้วย

# Install

**Install thaipdf:** ติดตั้ง development version ของ thaipdf จาก
[GitHub](https://github.com/Lightbridge-KS/thaipdf) ด้วย:

``` r
# install.packages("remotes")
remotes::install_github("Lightbridge-KS/thaipdf")
```

**Install XeLaTeX และ LaTeX package** ที่จำเป็นสำหรับการใช้ภาษาไทย
อาจทำได้อย่างง่ายโดยใช้ [{tinytex}](https://yihui.org/tinytex/) R
package:

``` r
install.packages("tinytex")

# For using R Markdown
tinytex::install_tinytex() 

# Install LaTeX PKG required by `thaipdf`
tinytex::tlmgr_install(c(
  "fontspec", "ucharclasses", "polyglossia"
))
```

**Check font:** ตรวจสอบว่าในเครื่องมีฟอนท์ [TH Sarabun
New](https://www.f0nt.com/release/th-sarabun-new/)
ถ้าไม่มีให้ติดตั้งด้วย (เนื่องจาก thaipdf ใช้ font นี้เป็นตัวหลัก)

# Usage

วิธีใช้ที่ง่ายที่สุด คือสร้าง R Markdown ที่รองรับภาษาไทยจาก template
ที่ package นี้มีให้

ถ้าท่านใช้ RStudio ให้ไปที่
`File -> New File -> R Markdown... -> From Template`

<img src="man/figures/rmd-from-temp.png" alt="New R Markdown Template" width="500"/>

เลือก template **Thai PDF R Markdown** หรือ **Thai PDF Bookdown**
จากเมนู

กดปุ่ม **`OK`** จะเปิดไฟล์ R Markdown template ที่รองรับกับภาษาไทย

<img src="man/figures/rstudio-rmd.png" alt="thaipdf R Markdown in RStudio" width="600"/>

จากนั้นกดปุ่ม **`Knit`** (cmd/ctr + shift + K) ได้เลย จะได้เอกสารเป็น
PDF ภาษาไทยที่มี เนื้อหา โค้ด และ กราฟ อยู่รวมกัน

![ตัวอย่าง R Markdown PDF ภาษาไทย](man/figures/pdf-doc-1.png)

# Learn more

-   [**Get Started with
    thaipdf**](https://lightbridge-ks.github.io/thaipdf/articles/thaipdf.html)

------------------------------------------------------------------------

Last Updated: 2022-04-12
