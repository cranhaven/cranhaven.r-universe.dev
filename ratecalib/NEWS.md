# ratecalib 0.3.0

首个 CRAN 提交版本。本版相对 0.2.1 的主要新增与变更如下。

- **新增中英文免责声明与使用条款**，针对本工具可能被滥用于学术造假的风险：随包分发
  `inst/DISCLAIMER.md`（安装后 `system.file("DISCLAIMER.md", package="ratecalib")` 可取），项目根
  同备一份；README 与 `docs/PLAIN-GUIDE.md` 置顶醒目声明。新增 `.onAttach` 加载提示（英文 ASCII，
  指向完整声明），`R CMD check` 确认其可被抑制、不影响测试。声明涵盖正当用途、禁止滥用与学术诚信、
  不提供担保、责任限制、使用者责任、接受条款，作为 MIT 许可证的补充。
- **mean/total 目标现支持 soft 模式**：不再限 exact。为解决「率 vs 数值量级」惩罚不可比的问题，
  mean/total 的目标行惩罚按目标量级 `|target|` 归一化（即惩罚相对误差），与率的绝对误差可比；
  proportion 仍用绝对率误差。total 的非零右端在 chi2 soft 下补一个线性项 `q -= 2 RᵀW·rhs`。
  至此 proportion/mean/total × soft/exact × chi2/raking/logit 组合全部可用。
- **`check_calibration_data()` 与 `calibration_feasibility()` 兼容进阶目标类型**：二者原只针对单维
  proportion-on-outcome 目标，遇交互（冒号 key）、mean/total、非-outcome 占比目标会误判；现改为只分析
  简单目标、跳过其余并在 `note`/`reason` 中说明，不再误报。
- **`make_rate_targets()` 新增 `proportions=` 便捷接口**（对称于 `means=`/`totals=`）：data.frame 列
  variable/level/value_var/value/target，构造任意分类取值的占比目标。
- **新增重复权重方差估计（方法论路线图 §七 + §五 进度条）**：`calibrate_replicate_weights(fit,
  repweights, scale, rscales, progress)` 对每套重复权重（bootstrap/jackknife/BRR，外部生成）以 `fit`
  的同一目标与设置重新校准（目标自 `fit$target_check` 重建），得校准后重复权重矩阵；`replicate_variance(
  object, x, statistic)` 用通用重复方差公式 `Var = scale·Σ rscales_r·(θ̂_r − θ̂_0)²` 估计任意 total/mean
  的方差/SE，与 `survey` 的 `svrepdesign` 对齐（scale/rscales 由用户按重复方案设定）。`progress=TRUE` 在
  重复循环上显示进度条（`utils::txtProgressBar`，无新依赖）。配 `print.replicate_calibration`。
  （未做 design-based 线性化方差——需引入本包尚不建模的抽样设计信息。）
- **raking / logit 距离现支持 soft 模式（惩罚校准）**：不再限 exact。边际仍为硬约束，目标行按
  `Omega^{-1} = size^2/(2*lambda*grand_total*priority)` 软化（与 chi2 soft 惩罚强度语义一致，
  lambda 越大越接近 exact）。对偶 Newton 求解器加一个按约束的 ridge 向量 `reg` 即实现
  （`F = Ax - t + reg*lambda`，`J` 加 `diag(reg)`，正定更稳）。soft 下目标不可达（如全合格组）也总有解、
  不再报错。（mean/total 的 soft 模式见上方条目。）
- **新增标准 S3 提取方法**：`weights()` 取校准权重向量、`as.data.frame()` 取含校准权重列的数据框，
  避免用户手挖 `fit$data$weight_calibrated`。
- **修复**：`stats` 之前虽全程以 `stats::` 调用却未声明在 `DESCRIPTION` 的 Imports（潜在依赖缺漏），
  现已补上并 `importFrom(stats, weights)`。
- **新增目标统计量泛化：均值 / 总量（方法论路线图 §二，连续变量部分）**。目标表新增可选列
  `statistic`（`"proportion"`/`"mean"`/`"total"`，缺省 `"proportion"`，**向后兼容**）与 `value_var`
  （数值列名）。均值/总量用单元级充分统计量 `w̄_c = Σ(d·W)/D_c` 在现有单元上加线性目标行实现，数学正确、
  不改聚合结构。`make_rate_targets()` 新增 `means` / `totals` 参数（data.frame：variable/level/value_var/
  target）。`target_check` 增 `statistic`/`value_var` 列；`target_rate` 的 0–1 校验仅对 proportion 生效。
  当前均值/总量仅 `mode = "exact"`（避免 soft 模式下「率 vs 数值」惩罚尺度不可比）。
- **占比泛化到任意分类变量（方法论路线图 §二 比例部分）**：proportion 目标现可分离「分组」（variable/level，
  定义 mask）与「被测量」（新列 `value_var`/`value`，即「value_var==value 的占比」）。为保证完全控制力，
  被测量的分类变量会被加入聚合键**拆单元**（每单元在该指示变量上纯）；遗留合格率即 `value_var=outcome,
  value=1` 的特例，**逐字节兼容**（无非 outcome 占比时不增加任何拆分）。`target_check` 增 `value` 列；
  重复行判定改为按 variable/level/statistic/value_var/value 全键（故同一变量不同取值的两个占比目标可共存）。
  这修正了路线图原草图「按分组单元聚合 + 存内部比例 ā_c、不拆单元」的设计隐患（那样无法复现 0/1 合格率
  的控制力）：连续量用充分统计量（凸包控制），分类占比按指示变量拆纯单元（完全控制）。
- **新增交互（cross-classification）目标（方法论路线图 §六）**：支持校准「城镇×男性」这类交叉分组的
  合格率。目标表用冒号连接的复合 key——`variable = "sex:residence"`、`level = "M:Urban"`，内部按 `:`
  拆分对各分量取交集 mask。`make_rate_targets()` 新增 `interactions`（与 `interaction_priority`）参数：
  `interactions = list("sex:residence" = c("M:Urban" = 0.7))`。交互目标**只新增目标行、不自动新增边际
  等式**，soft/exact 均可用；各分量变量须在 `group_vars` 中，分量数须与水平数一致（否则报错）。
  注意：水平值本身不可含冒号。
- **新增距离函数族（方法论路线图 §一）：`distance` 参数**。`calibrate_pass_rates()` 与
  `calibrate_rates()` 新增 `distance = c("chi2", "raking", "logit")`。`"chi2"`（默认）为原线性/卡方
  距离，走 OSQP，**行为与旧版完全一致**（非破坏性）。`"raking"` 为熵距离 `g log g - g + 1`，解
  `g = exp(eta)` **天然恒正**（上方无界，`lower`/`upper` 不强制，越界倍数在诊断中报告）。`"logit"`
  为有界 logit 距离，倍数**解析地恒在 `(lower, upper)` 开区间内**（要求 `lower < 1 < upper`），适合
  需要硬性封顶极端权重的场景。raking 与 logit 均用对偶 Newton 迭代（纯 R + Matrix，含回溯线搜索）
  求解，目标不可达时给出明确不收敛报错；二者当前仅支持 `mode = "exact"`。`settings` 记录 `distance`。
  （soft 版 raking/logit 留作后续；"默认改 raking"已决定放弃——默认永久保持 chi2，raking/logit 仅 opt-in。）
- **新增 Excel 输入/输出（方法论路线图 §四）**：`read_calibration_data()`、`read_targets_xlsx()`
  （表头容错，支持英文别名与中文表头）、`calibrate_from_excel()`（一步读数据+目标并求解，自动从
  目标表推断分组变量）、`export_calibration_xlsx()`（导出 data/target_check/margin_check/
  diagnostics/settings 多工作表）。依赖 `openxlsx` 走 `Suggests`，运行时 `requireNamespace()`
  守卫，缺失即报安装提示；不引入任何核心依赖。中文表头别名在源码中以 Unicode 码点构造，保持 R 代码纯 ASCII。
- **新增 `calibration_feasibility()`：求解前目标可行性预检**（方法论路线图 §三，收窄版）。
  做两件确定性、闭式的检查：(1) **总体–分组一致性恒等式**——某分组变量的每个水平都被目标覆盖时，
  总体率被唯一确定（`Σ W_ℓ·r_ℓ / W`），据此抓出与显式总体目标或另一完整变量互相矛盾的目标；
  (2) **单目标边际可达区间**——组总量固定 + 倍数箱界下，组内加权率的闭式可达区间（两段 water-filling），
  目标落区间外即必不可行。配套 `print.ratecalib_feasibility()`。返回值含明确边界声明：
  单目标筛查是**必要非充分**条件，联合可行仍以求解器为准。
- **`check_calibration_data()` 接入一致性预检**：求解前若分组目标隐含的总体率与显式总体目标
  实质性不一致，会发出告警（`calibrate_rates(check=TRUE)` 默认路径也会触发）。新增参数
  `consistency_tol`（默认 0.01）——仅在不一致超过该容差时告警，避免约数目标无法整除连续权重边际
  导致的亚个百分点噪音；需要精确（exact 模式）分析请直接调用 `calibration_feasibility()`。
- **R 代码全面英文化以满足 CRAN ASCII 可移植性要求**：报错信息、`print`/`summary`/`plot`
  输出、`example_rate_data()` 的类别值（现为 M/F、Urban/Rural、Edu1-5、Age1-5）均改为英文。
- **移除中文函数别名**（`校准合格率`、`生成目标表`、`检查校准数据`、`生成演示数据`）；所有函数仅保留英文名。
- CRAN 准备：删除 `DESCRIPTION` 的 `LazyData`、补 `methods` 到 Imports、改写英文 Description、
  修正 `URL`/`BugReports` 为真实仓库、`as()` 改用新版 Matrix 推荐写法、中文 PDF 手册移至
  `inst/manual/` 并改 ASCII 文件名。`R CMD check --as-cran` 现为 0 ERROR / 0 WARNING（仅余首次提交的 New submission NOTE）。
- **修复**：`make_rate_targets()` 仅给 `overall`（不给任何分组）时会误报 “groups must be a named list”
  与 “group_priority must be a scalar or a named vector”——现已支持仅总体目标的用法。
- 大幅扩充测试：新增 exact 模式达标与边际保持、soft 模式 achieved≈target、触界、错误路径、
  `check_calibration_data()` 各分支、以及 `print`/`summary`/`plot`/`calibration_diagnostics` 等 S3 方法的覆盖。

# ratecalib 0.2.1

- 全面改写中文README，加入理论、算法、参数、诊断、案例和常见问题。
- 新增一步式函数 `calibrate_rates()`。
- 新增中文别名：`校准合格率()`、`生成目标表()`、`检查校准数据()`、`生成演示数据()`。
- 新增 `check_calibration_data()`，在求解前检查权重、0/1结果、分组缺失、空类别及全0/全1类别。
- 新增 `example_rate_data()` 演示数据生成器。
- 中文化打印、摘要和绘图标题。
- 保留 `calibrate_pass_rates()` 作为完整专业接口。

# ratecalib 0.1.0

- 初始版本。
