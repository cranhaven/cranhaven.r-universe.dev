# data outputs are equal

    {
      "type": "list",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["model", "r.squared", "adj.r.squared", "MSE", "RMSE", "MAE"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["stats::lm(formula = mpg ~ cyl + disp + hp, data = mtcars)", "stats::lm(formula = mpg ~ hp + drat + wt, data = mtcars)", "stats::lm(formula = mpg ~ ., data = mtcars)"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.76788774, 0.83687905, 0.86901576]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.74301857, 0.81940181, 0.80664232]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [8.16779228, 5.74005896, 4.60920094]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2.85793497, 2.39584201, 2.14690497]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2.31868126, 1.91089499, 1.72274016]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["model", "deviance", "AIC", "BIC"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["stats::glm(formula = vs ~ drat + hp, data = mtcars)", "stats::glm(formula = vs ~ wt + qsec, data = mtcars)", "stats::glm(formula = vs ~ ., data = mtcars)"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [3.62521808, 2.04356732, 1.58410397]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [29.12177948, 10.7788202, 18.62912292]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [34.98472309, 16.64176381, 36.21795376]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["model", "deviance", "AIC", "BIC"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["lme4::lmer(formula = Sepal.Length ~ (1 | Species), data = iris)", "lme4::lmer(formula = Sepal.Length ~ (1 | Species) + Petal.Length, data = iris)"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [239.545803, 119.79301491]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [245.545803, 127.79301491]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [254.57770888, 139.83555609]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["coefficient", "model_name", "estimate", "std_error", "p_value", "lower_ci", "upper_ci"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["(Intercept)", "cyl", "disp", "hp", "(Intercept)", "hp", "drat", "wt", "(Intercept)", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["mpg ~ cyl + disp + hp", "mpg ~ cyl + disp + hp", "mpg ~ cyl + disp + hp", "mpg ~ cyl + disp + hp", "mpg ~ hp + drat + wt", "mpg ~ hp + drat + wt", "mpg ~ hp + drat + wt", "mpg ~ hp + drat + wt", "mpg ~ .", "mpg ~ .", "mpg ~ .", "mpg ~ .", "mpg ~ .", "mpg ~ .", "mpg ~ .", "mpg ~ .", "mpg ~ .", "mpg ~ .", "mpg ~ ."]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [34.18491917, -1.22741994, -0.01883809, -0.01467933, 29.39493446, -0.0322304, 1.61504854, -3.22795405, 12.30337416, -0.11144048, 0.01333524, -0.02148212, 0.78711097, -3.71530393, 0.82104075, 0.31776281, 2.52022689, 0.65541302, -0.19941925]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2.59077758, 0.79727631, 0.01040369, 0.01465087, 6.15630337, 0.00892454, 1.22698266, 0.79639831, 18.71788443, 1.04502336, 0.0178575, 0.02176858, 1.63537307, 1.8944143, 0.7308448, 2.10450861, 2.05665055, 1.49325996, 0.8287525]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.53719807e-13, 0.13490438, 0.08092901, 0.32495185, 0.00005134, 0.00117841, 0.19875539, 0.0003643, 0.5181244, 0.91608738, 0.46348865, 0.33495531, 0.6352779, 0.06325215, 0.27394127, 0.88142347, 0.23398971, 0.66520643, 0.81217871]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [28.87795186, -2.86056643, -0.04014908, -0.04469028, 16.78431868, -0.0505115, -0.8983115, -4.85930204, -26.62259745, -2.28468553, -0.02380146, -0.06675236, -2.6138335, -7.65495413, -0.69883421, -4.05880242, -1.75681208, -2.44999107, -1.92290442]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [39.49188647, 0.40572655, 0.00247291, 0.01533161, 42.00555024, -0.0139493, 4.12840857, -1.59660607, 51.22934576, 2.06180457, 0.05047194, 0.02378812, 4.18805545, 0.22434628, 2.34091571, 4.69432805, 6.79726585, 3.76081711, 1.52406591]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["coefficient", "model_name", "estimate", "std_error", "p_value", "lower_ci", "upper_ci"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["(Intercept)", "drat", "hp", "(Intercept)", "wt", "qsec", "(Intercept)", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "am", "gear", "carb"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["vs ~ drat + hp", "vs ~ drat + hp", "vs ~ drat + hp", "vs ~ wt + qsec", "vs ~ wt + qsec", "vs ~ wt + qsec", "vs ~ .", "vs ~ .", "vs ~ .", "vs ~ .", "vs ~ .", "vs ~ .", "vs ~ .", "vs ~ .", "vs ~ .", "vs ~ .", "vs ~ ."]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.65555262, 0.13666451, -0.00483732, -2.19907328, -0.22572774, 0.18840512, -0.86436336, 0.0034128, -0.15872721, -0.0008945, 0.00288676, 0.02040477, -0.06355946, 0.12388279, -0.21334142, 0.0286401, -0.03578364]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.56631004, 0.13290016, 0.0010364, 0.53702105, 0.04948853, 0.0270979, 1.9505718, 0.02260262, 0.10264305, 0.00186487, 0.00222005, 0.17035498, 0.21309926, 0.07314783, 0.21566085, 0.15533566, 0.08565039]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.25647857, 0.31229817, 0.00006388, 0.00030896, 0.00008572, 1.21292432e-07, 0.66220071, 0.88142347, 0.13694618, 0.63642813, 0.20758341, 0.90579759, 0.76843416, 0.10512491, 0.33380531, 0.85548773, 0.68034357]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.45439466, -0.12381502, -0.00686864, -3.25161521, -0.32272347, 0.1352942, -4.68741384, -0.04088752, -0.3599039, -0.00454959, -0.00146445, -0.31348486, -0.48122632, -0.01948432, -0.63602891, -0.2758122, -0.20365533]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.76549991, 0.39714404, -0.00280601, -1.14653136, -0.12873201, 0.24151604, 2.95868713, 0.04771312, 0.04244948, 0.00276058, 0.00723797, 0.35429439, 0.35410741, 0.26724989, 0.20934607, 0.33309239, 0.13208805]
        }
      ]
    }

