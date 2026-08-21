(function () {
  const builtInTranslations = {
    "Download APA 7 Word": "APA 7 Word \u0130ndir",
    "Dynamic Fit Index": "Dinamik Uyum \u0130ndeksi",
    "Generate model- and sample-specific CFI and RMSEA cutoffs through simulation. Conventional cutoffs remain visible for comparison.": "Benzetim yoluyla modele ve \u00f6rnekleme \u00f6zg\u00fc CFI ve RMSEA kesme de\u011ferleri \u00fcretin. Kar\u015f\u0131la\u015ft\u0131rma i\u00e7in geleneksel kesme de\u011ferleri de g\u00f6sterilir.",
    "The optional dynamic package (version 1.1.0 or later) must be installed separately from its source archive or GitHub repository.": "\u0130ste\u011fe ba\u011fl\u0131 dynamic paketi (1.1.0 veya daha yeni) kaynak ar\u015fivinden ya da GitHub deposundan ayr\u0131ca kurulmal\u0131d\u0131r.",
    "Simulation scale:": "Benzetim \u00f6l\u00e7e\u011fi:",
    "Automatic": "Otomatik",
    "Normal continuous": "Normal s\u00fcrekli",
    "Non-normal continuous": "Normal olmayan s\u00fcrekli",
    "Categorical / ordinal": "Kategorik / s\u0131ral\u0131",
    "Simulation replications:": "Benzetim tekrar\u0131:",
    "MAD levels:": "MAD d\u00fczeyleri:",
    "Run Dynamic Fit Index": "Dinamik Uyum \u0130ndeksini \u00c7al\u0131\u015ft\u0131r",
    "Conventional Cutoffs": "Geleneksel Kesme De\u011ferleri",
    "Model-Specific Dynamic Cutoffs": "Modele \u00d6zg\u00fc Dinamik Kesme De\u011ferleri",
    "Reference: McNeish and Wolf (2023), Dynamic Fit Index Cutoffs for Confirmatory Factor Analysis Models; dynamic R package version 1.1.0 (AGPL-3).": "Kaynak: McNeish ve Wolf (2023), Dynamic Fit Index Cutoffs for Confirmatory Factor Analysis Models; dynamic R paketi s\u00fcr\u00fcm 1.1.0 (AGPL-3).",
    "Open article": "Makaleyi a\u00e7",
    "Package repository": "Paket deposu",
    "Select negatively worded items, then confirm the response scale or let the program detect item limits.": "Ters ifadeli maddeleri se\u00e7in; ard\u0131ndan yan\u0131t \u00f6l\u00e7e\u011fini belirtin veya madde s\u0131n\u0131rlar\u0131n\u0131 program\u0131n belirlemesine izin verin.",
    "Select numeric items...": "Say\u0131sal maddeleri se\u00e7in...",
    "Detect minimum and maximum separately for each item": "Her madde i\u00e7in alt ve \u00fcst s\u0131n\u0131r\u0131 ayr\u0131 ayr\u0131 belirle",
    "For a 1-5 item this becomes: 1 + 5 - score = 6 - score.": "1-5 aral\u0131\u011f\u0131ndaki bir madde i\u00e7in: 1 + 5 - puan = 6 - puan.",
    "Observed limits are optional; fixed scale limits are safer when the sample does not contain every response category.": "G\u00f6zlenen s\u0131n\u0131rlar iste\u011fe ba\u011fl\u0131d\u0131r; \u00f6rneklem her yan\u0131t kategorisini i\u00e7ermiyorsa sabit \u00f6l\u00e7ek s\u0131n\u0131rlar\u0131 daha g\u00fcvenlidir.",
    "No items have been reverse-scored.": "Hen\u00fcz ters puanlanan madde yok.",
    "Analyze missing data patterns and apply advanced imputation strategies.": "Kay\u0131p veri \u00f6r\u00fcnt\u00fclerini inceleyin ve geli\u015fmi\u015f veri atama y\u00f6ntemlerini uygulay\u0131n.",
    "Note: Use 'RF (Categorical)' for Likert scales (1,2,3...) to preserve integer structure. Use 'RF (Continuous)' for decimal values.": "Not: Tam say\u0131 yap\u0131s\u0131n\u0131 korumak i\u00e7in Likert \u00f6l\u00e7eklerinde (1, 2, 3...) 'RF (Kategorik)' se\u00e7ene\u011fini, ondal\u0131kl\u0131 de\u011ferlerde 'RF (S\u00fcrekli)' se\u00e7ene\u011fini kullan\u0131n.",
    "Test whether missing data is Missing Completely at Random (MCAR).": "Kay\u0131p verilerin tamamen rastgele kay\u0131p (MCAR) olup olmad\u0131\u011f\u0131n\u0131 s\u0131nay\u0131n.",
    "Random Forest (Categorical / Integer)": "Rastgele Orman (Kategorik / Tam Say\u0131)",
    "Random Forest (Continuous / Decimal)": "Rastgele Orman (S\u00fcrekli / Ondal\u0131kl\u0131)",
    "Optimal Parallel Analysis (MRFA)": "Optimal Paralel Analiz (MRFA)",
    "Traditional Parallel Analysis": "Geleneksel Paralel Analiz",
    "Parallel Analysis (Lubbe, 2019)": "Paralel Analiz (Lubbe, 2019)",
    "The marginal distribution of each item is preserved through permutation. The same automatic correlation estimator is used for the observed and reference eigenvalues.": "Her maddenin marjinal da\u011f\u0131l\u0131m\u0131 perm\u00fctasyonla korunur. G\u00f6zlenen ve referans \u00f6zde\u011ferlerde ayn\u0131 otomatik korelasyon kestiricisi kullan\u0131l\u0131r.",
    "Permutations:": "Perm\u00fctasyon say\u0131s\u0131:",
    "Reference quantile:": "Referans y\u00fczdeli\u011fi:",
    "Reference:": "Kaynak:",
    "Hull Method": "Hull Y\u00f6ntemi",
    "MAP (Original)": "MAP (Orijinal)",
    "MAP (Revised)": "MAP (D\u00fczeltilmi\u015f)",
    "Empirical Kaiser (EKC)": "Ampirik Kaiser (EKC)",
    "Comparison Data (CD)": "Kar\u015f\u0131la\u015ft\u0131rma Verisi (CD)",
    "Minimum Residuals": "En K\u00fc\u00e7\u00fck Art\u0131klar",
    "Minimum Residual": "En K\u00fc\u00e7\u00fck Art\u0131k",
    "Maximum Likelihood": "En \u00c7ok Olabilirlik",
    "Principal Axis": "Temel Eksen",
    "Unweighted Least Squares": "A\u011f\u0131rl\u0131ks\u0131z En K\u00fc\u00e7\u00fck Kareler",
    "Weighted Least Squares": "A\u011f\u0131rl\u0131kl\u0131 En K\u00fc\u00e7\u00fck Kareler",
    "Minimum Rank": "En K\u00fc\u00e7\u00fck S\u0131ra",
    "Minimum Chi-Square": "En K\u00fc\u00e7\u00fck Ki-Kare",
    "Generalized Least Squares": "Genelle\u015ftirilmi\u015f En K\u00fc\u00e7\u00fck Kareler",
    "None": "Yok",
    "Oblique": "E\u011fik",
    "Orthogonal": "Dik",
    "Show correlation values": "Korelasyon de\u011ferlerini g\u00f6ster",
    "Blue - White - Red": "Mavi - Beyaz - K\u0131rm\u0131z\u0131",
    "Black - White": "Siyah - Beyaz",
    "Purple - White - Green": "Mor - Beyaz - Ye\u015fil",
    "Orange - White - Blue": "Turuncu - Beyaz - Mavi",
    "Teal - White - Rose": "Turkuaz - Beyaz - G\u00fcl Kurusu",
    "To apply this strategy, go to Exclude Variables, remove the listed items, and continue the analysis with the updated data.": "Bu stratejiyi uygulamak i\u00e7in De\u011fi\u015fken \u00c7\u0131karma ad\u0131m\u0131na gidin, listelenen maddeleri \u00e7\u0131kar\u0131n ve analize g\u00fcncellenmi\u015f veriyle devam edin.",
    "Correlation Method:": "Korelasyon Y\u00f6ntemi:",
    "Extraction Method:": "\u00c7\u0131kar\u0131m Y\u00f6ntemi:",
    "Rotation Method:": "D\u00f6nd\u00fcrme Y\u00f6ntemi:",
    "Auto": "Otomatik",
    "Walktrap (default)": "Walktrap (varsay\u0131lan)",
    "Fast Greedy": "H\u0131zl\u0131 A\u00e7g\u00f6zl\u00fc",
    "Edge Betweenness": "Kenar Aras\u0131ndal\u0131\u011f\u0131",
    "Label Propagation": "Etiket Yay\u0131l\u0131m\u0131",
    "Project & Reports": "Proje ve Raporlar",
    "Select Data": "Veri Seçimi",
    "Exclude Variables": "Değişken Çıkarma",
    "Recode Variables": "Değişkenleri Yeniden Kodla",
    "Missing Values": "Kayıp Veriler",
    "Split Dataset": "Veriyi Böl",
    "Manage Outliers": "Aykırı Değerleri Yönet",
    "Assumptions": "Varsayım Kontrolleri",
    "Factor Retention": "Faktör Sayısı Belirleme",
    "EFA Setup & Analysis": "AFA Ayarları ve Analiz",
    "Item Drop Out": "Madde Çıkarma",
    "EFA Reporting": "AFA Raporlama",
    "Exploratory Graph Analysis": "Keşfedici Grafik Analizi",
    "Confirmatory Factor Analysis": "Doğrulayıcı Faktör Analizi",
    "Measurement Invariance": "Ölçme Değişmezliği",
    "Reliability Analysis": "Güvenirlik Analizi",
    "Item Weighting": "Madde Ağırlıklandırma",
    "About": "Hakkında",
    "Data & Wrangling": "Veri ve Düzenleme",
    "Assumption Checks": "Varsayım Kontrolleri",
    "Exploratory Factor Analysis": "Keşfedici Faktör Analizi",
    "Advanced Analysis": "İleri Analizler",
    "Info": "Bilgi",
    "Data Upload": "Veri Yükleme",
    "Project Settings": "Proje Ayarları",
    "About FAfA": "FAfA Hakkında",
    "What's New in FAfA": "FAfA'da Neler Yeni?",
    "Version 1.2 expands FAfA from an analysis interface into a reproducible psychometric workflow. The main additions are summarized below.": "S\u00fcr\u00fcm 1.2, FAfA'y\u0131 bir analiz aray\u00fcz\u00fcnden yeniden \u00fcretilebilir bir psikometrik i\u015f ak\u0131\u015f\u0131na d\u00f6n\u00fc\u015ft\u00fcrmektedir. Ba\u015fl\u0131ca yenilikler a\u015fa\u011f\u0131da \u00f6zetlenmi\u015ftir.",
    "Projects and reproducibility:": "Projeler ve yeniden \u00fcretilebilirlik:",
    "Save the dataset, preprocessing decisions, factor definitions, and interface settings in a single .fafa project file. Generate R, HTML, PDF, and anonymized diagnostic reports together with a workflow audit log.": "Veri setini, \u00f6n i\u015fleme kararlar\u0131n\u0131, fakt\u00f6r tan\u0131mlar\u0131n\u0131 ve aray\u00fcz ayarlar\u0131n\u0131 tek bir .fafa proje dosyas\u0131nda saklay\u0131n. \u0130\u015flem g\u00fcnl\u00fc\u011f\u00fcyle birlikte R, HTML, PDF ve anonimle\u015ftirilmi\u015f tan\u0131lama raporlar\u0131 olu\u015fturun.",
    "Turkish interface and safer data handling:": "T\u00fcrk\u00e7e aray\u00fcz ve daha g\u00fcvenli veri i\u015fleme:",
    "Switch between English and Turkish without restarting the application. Turkish and special characters in variable names are normalized safely, while categorical grouping variables are preserved.": "Uygulamay\u0131 yeniden ba\u015flatmadan \u0130ngilizce ve T\u00fcrk\u00e7e aras\u0131nda ge\u00e7i\u015f yap\u0131n. De\u011fi\u015fken adlar\u0131ndaki T\u00fcrk\u00e7e ve \u00f6zel karakterler g\u00fcvenli bi\u00e7imde d\u00fczenlenirken kategorik gruplama de\u011fi\u015fkenleri korunur.",
    "Data preparation and reliability:": "Veri haz\u0131rlama ve g\u00fcvenirlik:",
    "Reverse-score negatively worded items automatically, inspect every scoring formula, exclude variables consistently from downstream analyses, split datasets with a reproducible seed, and reuse saved dimensions for Stratified Alpha and other reliability coefficients.": "Ters ifadeli maddeleri otomatik puanlay\u0131n, her puanlama form\u00fcl\u00fcn\u00fc inceleyin, de\u011fi\u015fkenleri sonraki analizlerden tutarl\u0131 bi\u00e7imde \u00e7\u0131kar\u0131n, veri setlerini sabit bir rastgelelik tohumuyla b\u00f6l\u00fcn ve kaydedilmi\u015f boyutlar\u0131 Tabakal\u0131 Alfa ile di\u011fer g\u00fcvenirlik katsay\u0131lar\u0131nda yeniden kullan\u0131n.",
    "Factor retention and EFA reporting:": "Fakt\u00f6r say\u0131s\u0131 belirleme ve AFA raporlama:",
    "Use Lubbe's permutation parallel analysis for ordinal items, a standard eigenvalue scree plot, improved item-removal guidance, and correlation heatmaps with selectable palettes and optional numerical labels.": "S\u0131ral\u0131 maddeler i\u00e7in Lubbe perm\u00fctasyon paralel analizini, standart \u00f6zde\u011fer yama\u00e7 birikinti grafi\u011fini, geli\u015ftirilmi\u015f madde \u00e7\u0131karma y\u00f6nlendirmelerini ve se\u00e7ilebilir renklerle say\u0131sal etiketler sunan korelasyon \u0131s\u0131 haritalar\u0131n\u0131 kullan\u0131n.",
    "Expanded CFA model builder:": "Geni\u015fletilmi\u015f DFA model olu\u015fturucu:",
    "Define first-order, second-order, bifactor, and complex cross-loading models. Save or load lavaan syntax, reuse factor-indicator definitions, report chi-square/df, and export model-specific path diagrams.": "Birinci d\u00fczey, ikinci d\u00fczey, bifakt\u00f6r ve karma\u015f\u0131k \u00e7apraz y\u00fck modellerini tan\u0131mlay\u0131n. lavaan s\u00f6zdizimini kaydedip y\u00fckleyin, fakt\u00f6r-g\u00f6sterge tan\u0131mlar\u0131n\u0131 yeniden kullan\u0131n, ki-kare/sd de\u011ferini raporlay\u0131n ve modele \u00f6zg\u00fc yol diyagramlar\u0131n\u0131 d\u0131\u015fa aktar\u0131n.",
    "Dynamic Fit Index:": "Dinamik Uyum \u0130ndeksi:",
    "Generate model- and sample-specific CFI and RMSEA cutoffs using the McNeish and Wolf procedure while retaining conventional cutoffs for comparison.": "McNeish ve Wolf y\u00f6ntemiyle modele ve \u00f6rnekleme \u00f6zg\u00fc CFI ve RMSEA kesme de\u011ferleri \u00fcretirken kar\u015f\u0131la\u015ft\u0131rma i\u00e7in geleneksel kesme de\u011ferlerini de koruyun.",
    "Bootstrap Exploratory Graph Analysis:": "Bootstrap Ke\u015ffedici Grafik Analizi:",
    "Estimate dimension and item stability with bootEGA, download stability tables and publication-ready plots, and receive a clearly reported Pearson fallback when an ordinal correlation matrix cannot be estimated.": "bootEGA ile boyut ve madde kararl\u0131l\u0131\u011f\u0131n\u0131 kestirin, kararl\u0131l\u0131k tablolar\u0131n\u0131 ve yay\u0131na haz\u0131r grafikleri indirin; ordinal korelasyon matrisi hesaplanamad\u0131\u011f\u0131nda a\u00e7\u0131k\u00e7a bildirilen Pearson geri d\u00f6n\u00fc\u015f\u00fcn\u00fc kullan\u0131n.",
    "Publication-ready output:": "Yay\u0131na haz\u0131r \u00e7\u0131kt\u0131lar:",
    "Download APA 7 Word reports and export supported figures as 300 dpi PNG or JPG and scalable SVG files.": "APA 7 Word raporlar\u0131n\u0131 indirin; desteklenen grafikleri 300 dpi PNG veya JPG ve \u00f6l\u00e7eklenebilir SVG dosyalar\u0131 olarak d\u0131\u015fa aktar\u0131n.",
    "Performance and stability:": "Performans ve kararl\u0131l\u0131k:",
    "Session-level result caching reduces repeated calculations, dependent results reset when data change, and safer error handling prevents a failed analysis from closing the application.": "Oturum d\u00fczeyinde sonu\u00e7 \u00f6nbellekleme yinelenen hesaplamalar\u0131 azalt\u0131r; veri de\u011fi\u015fti\u011finde ba\u011f\u0131ml\u0131 sonu\u00e7lar s\u0131f\u0131rlan\u0131r ve daha g\u00fcvenli hata y\u00f6netimi ba\u015far\u0131s\u0131z bir analizin uygulamay\u0131 kapatmas\u0131n\u0131 \u00f6nler.",
    "Acknowledgements & Version": "Teşekkür ve Sürüm Bilgisi",
    "Analysis Settings": "Analiz Ayarları",
    "Choose Data File:": "Veri dosyası seçin:",
    "Please upload your data file. Supported formats:": "Lütfen veri dosyanızı yükleyin. Desteklenen biçimler:",
    "Ensure missing values are represented as NA.": "Kayıp verilerin NA olarak kodlandığından emin olun.",
    "No file selected": "Dosya seçilmedi",
    "My data has a header row": "Verimin başlık satırı var",
    "Analyze Data": "Veriyi Analiz Et",
    "Summary Statistics": "Özet İstatistikler",
    "Variables": "Değişkenler",
    "Sample Size": "Örneklem Büyüklüğü",
    "Min Value": "En Küçük Değer",
    "Max Value": "En Büyük Değer",
    "Range": "Değer Aralığı",
    "Data Preview": "Veri Önizleme",
    "Available Variables": "Kullanılabilir Değişkenler",
    "Excluded Variables": "Çıkarılan Değişkenler",
    "Exclude Selected": "Seçilenleri Çıkar",
    "Recover Selected": "Seçilenleri Geri Al",
    "Reset All": "Tümünü Sıfırla",
    "Reverse-score Items": "Ters Puanlanacak Maddeler",
    "Items to reverse-score:": "Ters puanlanacak maddeler:",
    "Scale minimum:": "Ölçek alt sınırı:",
    "Scale maximum:": "Ölçek üst sınırı:",
    "Reverse & Update": "Ters Puanla ve Güncelle",
    "Reset": "Sıfırla",
    "Download Recoded Data": "Yeniden Kodlanan Veriyi İndir",
    "Applied Scoring Rules": "Uygulanan Puanlama Kuralları",
    "How it is calculated": "Nasıl hesaplanır?",
    "New score = scale minimum + scale maximum - original score.": "Yeni puan = ölçek alt sınırı + ölçek üst sınırı - özgün puan.",
    "Imputation Settings": "Atama Ayarları",
    "Choose Imputation Method:": "Atama yöntemi seçin:",
    "Visualize Only (No Action)": "Yalnızca Görselleştir (İşlem Yapma)",
    "Listwise Deletion (Remove Rows)": "Liste Bazında Silme (Satırları Çıkar)",
    "Mean Imputation (Simple)": "Ortalama ile Atama (Basit)",
    "Median Imputation (Simple)": "Ortanca ile Atama (Basit)",
    "Download Processed Data": "İşlenmiş Veriyi İndir",
    "Missing Data Summary & Diagnostics": "Kayıp Veri Özeti ve Tanılaması",
    "Missingness by Variable": "Değişkene Göre Kayıp Veri",
    "Imputation Method:": "Atama yöntemi:",
    "Apply Imputation": "Atamayı Uygula",
    "Missing Data Diagnostics": "Kayıp Veri Tanılaması",
    "Little's MCAR Test": "Little MCAR Testi",
    "Run MCAR Test": "MCAR Testini Çalıştır",
    "Missingness Map": "Kayıp Veri Haritası",
    "Missing Value Summary": "Kayıp Veri Özeti",
    "Random seed:": "Rastgelelik tohumu:",
    "Split Data": "Veriyi Böl",
    "Split Percentage:": "Bölme yüzdesi:",
    "First Subset %:": "İlk alt küme %:",
    "Save Part 1": "1. Bölümü Kaydet",
    "Save Part 2": "2. Bölümü Kaydet",
    "Outlier Detection": "Aykırı Değer Belirleme",
    "Outlier Results": "Aykırı Değer Sonuçları",
    "P-value Threshold:": "P-değeri eşiği:",
    "Find Outliers": "Aykırı Değerleri Bul",
    "Remove & Update": "Çıkar ve Güncelle",
    "Download Clean Data": "Temiz Veriyi İndir",
    "Descriptive Statistics": "Betimsel İstatistikler",
    "Calculate Descriptives": "Betimsel İstatistikleri Hesapla",
    "Collinearity": "Doğrusal Bağlantı",
    "Run Collinearity Check": "Doğrusal Bağlantıyı Kontrol Et",
    "Multivariate Normality": "Çok Değişkenli Normallik",
    "Run Normality Tests": "Normallik Testlerini Çalıştır",
    "Retention Methods": "Faktör Belirleme Yöntemleri",
    "Select Method:": "Yöntem seçin:",
    "Run Analysis": "Analizi Çalıştır",
    "Scree Plot": "Yamaç Birikinti Grafiği",
    "Scree Plot (Eigenvalues)": "Yamaç Birikinti Grafiği (Özdeğerler)",
    "Results": "Sonuçlar",
    "EFA Configuration": "AFA Ayarları",
    "Correlation:": "Korelasyon:",
    "No. of Factors:": "Faktör sayısı:",
    "Extraction:": "Çıkarım yöntemi:",
    "Rotation:": "Döndürme:",
    "Run EFA": "AFA'yı Çalıştır",
    "Sampling Adequacy & Sphericity": "Örneklem Yeterliği ve Küresellik",
    "KMO Measure of Sampling Adequacy": "KMO Örneklem Yeterliği Ölçüsü",
    "KMO Measure of Sampling Adequacy:": "KMO Örneklem Yeterliği Ölçüsü:",
    "Bartlett's Test of Sphericity": "Bartlett Küresellik Testi",
    "Factor Solution & Visualisation": "Faktör Çözümü ve Görselleştirme",
    "Heatmap": "Isı Haritası",
    "Loadings": "Faktör Yükleri",
    "Variance Explained": "Açıklanan Varyans",
    "Factor Correlations (Phi)": "Faktör Korelasyonları (Phi)",
    "Colour palette:": "Renk paleti:",
    "Download PNG (300 dpi)": "PNG İndir (300 dpi)",
    "Download SVG": "SVG İndir",
    "Download CSV": "CSV İndir",
    "Download Results": "Sonuçları İndir",
    "Run ItemRest Analysis": "Madde Çıkarma Analizini Çalıştır",
    "Comparative Removal Strategies": "Karşılaştırmalı Madde Çıkarma Stratejileri",
    "Optimal Strategy Result": "En Uygun Strateji Sonucu",
    "Automated item removal strategies for EFA.": "AFA için otomatik madde çıkarma stratejileri.",
    "Leave empty to determine automatically via Parallel Analysis.": "Paralel analizle otomatik belirlenmesi için boş bırakın.",
    "The strategy above represents the cleanest factor structure found.": "Yukarıdaki strateji, belirlenen en temiz faktör yapısını gösterir.",
    "This table compares model fit and structure across different item removal thresholds.": "Bu tablo, farklı madde çıkarma eşiklerinde model uyumunu ve faktör yapısını karşılaştırır.",
    "Optimal strategy details not found in result object or console output.": "En uygun stratejinin ayrıntıları sonuç nesnesinde veya konsol çıktısında bulunamadı.",
    "Summary table not returned. Please check the text output above.": "Özet tablo oluşturulamadı. Lütfen yukarıdaki metin çıktısını kontrol edin.",
    "Message": "İleti",
    "Number of Factors (Optional):": "Faktör Sayısı (İsteğe Bağlı):",
    "Model Builder": "Model Oluşturucu",
    "CFA Analysis Setup": "DFA Analiz Ayarları",
    "Define Factor (=~)": "Faktör Tanımla (=~)",
    "Factor Name:": "Faktör adı:",
    "Indicators:": "Göstergeler:",
    "Add to Syntax": "Sözdizimine Ekle",
    "Add Covariance (~~)": "Kovaryans Ekle (~~)",
    "Select Indicators:": "Göstergeleri seçin:",
    "Select 2 Variables:": "2 değişken seçin:",
    "Load syntax (.txt/.lav)": "Sözdizimi yükle (.txt/.lav)",
    "Save Syntax": "Sözdizimini Kaydet",
    "Estimator:": "Kestirim yöntemi:",
    "Indicator Type:": "Gösterge türü:",
    "Run CFA Analysis": "DFA Analizini Çalıştır",
    "Run CFA": "DFA'yı Çalıştır",
    "Model Syntax (lavaan)": "Model Sözdizimi (lavaan)",
    "Fit Measures": "Uyum İndeksleri",
    "Factor Loadings": "Faktör Yükleri",
    "Modification Indices": "Modifikasyon İndeksleri",
    "Path Diagram": "Yol Diyagramı",
    "Plot Settings": "Grafik Ayarları",
    "Layout Style:": "Yerleşim biçimi:",
    "Box Width:": "Kutu genişliği:",
    "Label Size:": "Etiket boyutu:",
    "Show Estimates": "Kestirimleri Göster",
    "Download Diagram": "Diyagramı İndir",
    "Exploratory Graph Analysis (EGA)": "Keşfedici Grafik Analizi (KGA)",
    "Run EGA": "KGA'yı Çalıştır",
    "EGA Setup": "KGA Ayarları",
    "Network Model:": "Ağ modeli:",
    "Community Detection:": "Topluluk belirleme:",
    "Network Matrix": "Ağ Matrisi",
    "Network Plot": "Ağ Grafiği",
    "Dimensionality Summary": "Boyutluluk Özeti",
    "Item-Community Assignments": "Madde-Topluluk Eşleştirmeleri",
    "Dimensions & Structure": "Boyutlar ve Yapı",
    "Item Allocation": "Madde Dağılımı",
    "Adjacency Matrix": "Komşuluk Matrisi",
    "Download format:": "İndirme biçimi:",
    "Download Plot": "Grafiği İndir",
    "Bootstrap Exploratory Graph Analysis (bootEGA)": "Bootstrap Keşfedici Grafik Analizi (bootEGA)",
    "Estimate dimensional stability with EGAnet::bootEGA(), dimensionStability(), and itemStability().": "Boyut kararlılığını EGAnet::bootEGA(), dimensionStability() ve itemStability() ile tahmin eder.",
    "Bootstrap EGA Setup": "Bootstrap EGA Ayarları",
    "Bootstrap type:": "Bootstrap türü:",
    "Parametric": "Parametrik",
    "Resampling (non-parametric)": "Yeniden örnekleme (parametrik olmayan)",
    "Bootstrap samples (iter):": "Bootstrap örneklem sayısı (iter):",
    "Processor cores (ncores):": "İşlemci çekirdeği (ncores):",
    "Random seed:": "Rastgelelik tohumu:",
    "Estimate the typical network structure": "Tipik ağ yapısını tahmin et",
    "Run Bootstrap Exploratory Graph Analysis": "Bootstrap Keşfedici Grafik Analizini Çalıştır",
    "Bootstrap EGA Results": "Bootstrap EGA Sonuçları",
    "Bootstrap Summary": "Bootstrap Özeti",
    "Bootstrap distribution summary": "Bootstrap dağılım özeti",
    "Dimension frequency": "Boyut sıklığı",
    "Download Summary CSV": "Özet CSV'yi İndir",
    "Download Frequency CSV": "Sıklık CSV'sini İndir",
    "Dimension Stability": "Boyut Kararlılığı",
    "Structural consistency reports exact dimension replication; average item stability summarizes item assignment stability within each dimension.": "Yapısal tutarlılık boyutun tam olarak tekrarlanmasını; ortalama madde kararlılığı ise her boyuttaki madde atamalarının kararlılığını gösterir.",
    "Download Dimension Stability CSV": "Boyut Kararlılığı CSV'sini İndir",
    "Item Stability": "Madde Kararlılığı",
    "Download Item Stability CSV": "Madde Kararlılığı CSV'sini İndir",
    "Download Item Stability Plot": "Madde Kararlılığı Grafiğini İndir",
    "Reliability Setup": "Güvenirlik Ayarları",
    "Model Definition": "Model Tanımlama",
    "Dimension name:": "Boyut adı:",
    "Items in dimension:": "Boyuttaki maddeler:",
    "Add Dimension": "Boyut Ekle",
    "Select items...": "Maddeleri seçin...",
    "Dimensions added above appear here automatically.": "Yukarıda eklenen boyutlar burada otomatik olarak görünür.",
    "For Stratified Alpha, strata codes are generated automatically from these dimensions.": "Tabakalı Alfa için strata kodları bu boyutlardan otomatik olarak üretilir.",
    "Select Items (leave empty for all):": "Maddeleri seçin (tümü için boş bırakın):",
    "Use saved CFA dimensions:": "Kaydedilmiş DFA boyutlarını kullan:",
    "Coefficient:": "Katsayı:",
    "CFA Model Syntax (lavaan):": "DFA Model Sözdizimi (lavaan):",
    "Data Type:": "Veri türü:",
    "Result": "Sonuç",
    "Calculate": "Hesapla",
    "Calculate Scores": "Puanları Hesapla",
    "Item Weighting (Kılıç, 2026)": "Madde Ağırlıklandırma (Kılıç, 2026)",
    "Applies psychometric weighting based on item difficulty and discrimination.": "Madde güçlüğü ve ayırt ediciliğine dayalı psikometrik ağırlıklandırma uygular.",
    "Reference:": "Kaynak:",
    ". Mitigating the slipping effect in polytomous scales: The Generalized Conditional Reliability Weighting (G-CRW) Algorithm and the WeightMyItems R package.": ". Çok kategorili ölçeklerde kayma etkisinin azaltılması: Genelleştirilmiş Koşullu Güvenirlik Ağırlıklandırma (G-CRW) Algoritması ve WeightMyItems R paketi.",
    "Preview": "Önizleme",
    "Download Weighted Data": "Ağırlıklandırılmış Veriyi İndir",
    "Aim:": "Amaç:",
    "The FAfA (Factor Analysis for All) Shiny application is a powerful and user-friendly tool designed to simplify Exploratory Factor Analysis (EFA), Confirmatory Factor Analysis (CFA), and Measurement Invariance workflows for researchers. Developed with R and Shiny, FAfA aims to unify these psychometric procedures within a single, intuitive interface, reducing the need for multiple software tools or complex manual preprocessing steps. It enables users to diagnose and handle missing data, validate assumptions, perform random dataset splits, conduct comprehensive reliability analyses (including Stratified Alpha), apply automated item drop-out strategies, and utilize item weighting techniques to enhance construct validity.": "FAfA (Factor Analysis for All) Shiny uygulaması, araştırmacıların Açımlayıcı Faktör Analizi (AFA), Doğrulayıcı Faktör Analizi (DFA) ve Ölçme Değişmezliği iş akışlarını kolaylaştırmak için geliştirilmiş güçlü ve kullanıcı dostu bir araçtır. R ve Shiny ile geliştirilen FAfA, bu psikometrik işlemleri tek ve sezgisel bir arayüzde birleştirerek farklı yazılımlara veya karmaşık elle veri ön işleme adımlarına duyulan gereksinimi azaltmayı amaçlar. Kullanıcıların kayıp verileri inceleyip işlemesine, varsayımları sınamasına, veri setlerini rastgele bölmesine, Tabakalı Alfa dâhil kapsamlı güvenirlik analizleri yapmasına, otomatik madde çıkarma stratejilerini uygulamasına ve yapı geçerliğini güçlendirmek için madde ağırlıklandırma tekniklerinden yararlanmasına olanak tanır.",
    "Overview": "Genel Bakış",
    "FAfA provides a comprehensive suite of tools for psychometric analysis. It leverages established R packages such as 'psych', 'lavaan', 'missForest', and 'EGAnet', ensuring that statistical analyses are accurate and reliable. Users can easily upload their datasets (e.g., CSV, Excel, SAV, DAT), perform advanced diagnostics for missing values and outliers, configure key parameters for their analyses (like estimator types, rotation methods), and interpret results through a responsive user interface.": "FAfA, psikometrik analizler için kapsamlı bir araç takımı sunar. 'psych', 'lavaan', 'missForest' ve 'EGAnet' gibi yerleşik R paketlerinden yararlanarak istatistiksel analizlerin doğru ve güvenilir biçimde yürütülmesini sağlar. Kullanıcılar veri setlerini (ör. CSV, Excel, SAV, DAT) kolayca yükleyebilir; kayıp veriler ve aykırı değerler için gelişmiş tanılamalar yapabilir; kestirim ve döndürme yöntemleri gibi temel analiz seçeneklerini belirleyebilir ve sonuçları uyumlu bir kullanıcı arayüzü üzerinden yorumlayabilir.",
    "Key Features and Technical Details": "Temel Özellikler ve Teknik Ayrıntılar",
    "Unified EFA, CFA & Invariance Workflow:": "Bütünleşik AFA, DFA ve Değişmezlik İş Akışı:",
    "Conduct EFA, CFA, and Measurement Invariance testing within the same environment using a seamless workflow.": "AFA, DFA ve Ölçme Değişmezliği analizlerini kesintisiz bir iş akışıyla aynı ortamda yürütün.",
    "Advanced Missing Data Handling:": "Gelişmiş Kayıp Veri İşleme:",
    "Analyze missingness patterns, test for MCAR, and apply robust imputation methods such as MICE and missForest (Random Forest).": "Kayıp veri örüntülerini inceleyin, MCAR testini uygulayın ve MICE ile missForest (Rastgele Orman) gibi güçlü atama yöntemlerini kullanın.",
    "Item Drop Out Analysis:": "Madde Çıkarma Analizi:",
    "Utilize automated strategies to identify and remove problematic items, optimizing scale length and factor structure.": "Sorunlu maddeleri belirleyip çıkarmak, ölçek uzunluğunu ve faktör yapısını iyileştirmek için otomatik stratejilerden yararlanın.",
    "Reliability Analysis:": "Güvenirlik Analizi:",
    "Evaluate internal consistency using Cronbach's Alpha, McDonald's Omega, Armor's Theta, and Stratified Alpha for multidimensional scales.": "İç tutarlılığı Cronbach Alfa, McDonald Omega, Armor Teta ve çok boyutlu ölçekler için Tabakalı Alfa ile değerlendirin.",
    "Interactive Model Builder:": "Etkileşimli Model Oluşturucu:",
    "Easily define factor structures and covariances for CFA and Measurement Invariance without manually writing complex syntax.": "Karmaşık sözdizimlerini elle yazmadan DFA ve Ölçme Değişmezliği için faktör yapılarını ve kovaryansları kolayca tanımlayın.",
    "Assumption Checking & Wrangling:": "Varsayım Kontrolü ve Veri Düzenleme:",
    "Built-in diagnostics for multivariate normality, outliers (Mahalanobis Distance), and multicollinearity.": "Çok değişkenli normallik, aykırı değerler (Mahalanobis Uzaklığı) ve çoklu doğrusal bağlantı için yerleşik tanılamaları kullanın.",
    "Random Dataset Splitting:": "Rastgele Veri Seti Bölme:",
    "Supports rigorous validation by allowing users to randomly split datasets (e.g., for EFA on one half, CFA on the other).": "Veri setini rastgele bölerek (ör. bir yarıda AFA, diğer yarıda DFA) güçlü doğrulama çalışmaları yürütmenizi sağlar.",
    "Interactive and Reproducible Results:": "Etkileşimli ve Yeniden Üretilebilir Sonuçlar:",
    "Provides real-time updates with outputs like path diagrams, scree plots, and fit statistics (CFI, TLI, RMSEA). Supports exporting results for publication.": "Yol diyagramları, yamaç birikinti grafikleri ve uyum istatistikleri (CFI, TLI, RMSEA) gibi çıktıları gerçek zamanlı olarak günceller. Sonuçların yayın için dışa aktarılmasını destekler.",
    "Invariance Setup": "Değişmezlik Ayarları",
    "Empty categories within groups:": "Gruplardaki boş kategoriler:",
    "Merge with the nearest category shared by all groups": "Tüm gruplarda ortak olan en yakın kategoriyle birleştir",
    "Stop and report affected variables": "Durdur ve etkilenen değişkenleri bildir",
    "Merging is applied consistently in every group and only to the analysis copy.": "Birleştirme her grupta tutarlı biçimde ve yalnızca analiz kopyasına uygulanır.",
    "The original dataset is not changed, and every recoding is reported.": "Özgün veri seti değiştirilmez ve yapılan her yeniden kodlama raporlanır.",
    "Ordinal Data Check": "Ordinal Veri Kontrolü",
    "Empty categories detected before analysis": "Analiz öncesinde belirlenen boş kategoriler",
    "Categories merged for this analysis": "Bu analiz için birleştirilen kategoriler",
    "Empty_Categories": "Boş_Kategoriler",
    "Frequencies": "Frekanslar",
    "Original_Category": "Özgün_Kategori",
    "Recoded_Category": "Birleştirilen_Kategori",
    "Missing_In_Groups": "Bulunmadığı_Gruplar",
    "Grouping Variable:": "Gruplama değişkeni:",
    "Levels:": "Düzeyler:",
    "Comparison (LRT)": "Karşılaştırma (LRT)",
    "Language:": "Dil:",
    "English": "İngilizce",
    "Turkish": "Türkçe",
    "Save FAfA Project": "FAfA Projesini Kaydet",
    "Load FAfA Project": "FAfA Projesini Yükle",
    "Project file:": "Proje dosyası:",
    "Include data in project file": "Veriyi proje dosyasına dahil et",
    "Load Project": "Projeyi Yükle",
    "Reproducible Reports": "Yeniden Üretilebilir Raporlar",
    "Download R Script": "R Kodunu İndir",
    "Download HTML Report": "HTML Raporu İndir",
    "Download PDF Report": "PDF Raporu İndir",
    "Download Diagnostic Report": "Tanılama Raporunu İndir",
    "Workflow Audit": "İşlem Günlüğü",
    "Clear Audit": "Günlüğü Temizle",
    "No project file has been loaded.": "Henüz bir proje dosyası yüklenmedi.",
    "Workflow audit cleared.": "İşlem günlüğü temizlendi.",
    "Diagnostic reports contain package and system information only; uploaded data, variable names, model syntax, and file paths are excluded.": "Tanılama raporları yalnızca paket ve sistem bilgilerini içerir; yüklenen veri, değişken adları, model sözdizimi ve dosya yolları rapora eklenmez."
  };
  const originalText = new WeakMap();
  const originalAttributes = new WeakMap();
  let language = "en";
  let translations = {};
  let handlersRegistered = false;
  let observerStarted = false;

  function translateTextNode(node) {
    if (!originalText.has(node)) originalText.set(node, node.nodeValue);
    const original = originalText.get(node);
    const match = original.match(/^(\s*)(.*?)(\s*)$/s);
    if (!match) return;
    const key = match[2];
    const translated = language === "tr" && translations[key] ? translations[key] : key;
    node.nodeValue = match[1] + translated + match[3];
  }

  function translateTree(root) {
    if (!root) return;
    if (root.nodeType === Node.ELEMENT_NODE) translateAttributes(root);
    if (root.querySelectorAll) {
      root.querySelectorAll("[placeholder],[title],[aria-label]").forEach(translateAttributes);
    }
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    const nodes = [];
    while (walker.nextNode()) nodes.push(walker.currentNode);
    nodes.forEach(translateTextNode);
    document.documentElement.lang = language;
  }

  function translateAttributes(element) {
    const attributes = ["placeholder", "title", "aria-label"];
    let saved = originalAttributes.get(element);
    if (!saved) {
      saved = {};
      attributes.forEach(function (attribute) {
        if (element.hasAttribute(attribute)) saved[attribute] = element.getAttribute(attribute);
      });
      originalAttributes.set(element, saved);
    }
    Object.keys(saved).forEach(function (attribute) {
      const original = saved[attribute];
      element.setAttribute(
        attribute,
        language === "tr" && translations[original] ? translations[original] : original
      );
    });
  }

  function triggerChange(element) {
    if (window.jQuery) window.jQuery(element).trigger("change");
    else element.dispatchEvent(new Event("change", { bubbles: true }));
  }

  function restoreInput(id, value) {
    const element = document.getElementById(id);
    if (!element || element.type === "file" || element.type === "button") return;

    if (element.selectize) {
      element.selectize.setValue(value, false);
      return;
    }

    const slider = window.jQuery ? window.jQuery(element).data("ionRangeSlider") : null;
    if (slider) {
      slider.update({ from: value });
      return;
    }

    const checks = element.querySelectorAll ? element.querySelectorAll("input[type='checkbox']") : [];
    const radios = element.querySelectorAll ? element.querySelectorAll("input[type='radio']") : [];
    if (checks.length) {
      const selected = Array.isArray(value) ? value.map(String) : [String(value)];
      checks.forEach(function (item) {
        item.checked = selected.includes(String(item.value));
      });
      triggerChange(checks[0]);
      return;
    }
    if (radios.length) {
      radios.forEach(function (item) {
        item.checked = String(item.value) === String(value);
      });
      const selectedRadio = Array.from(radios).find(function (item) { return item.checked; });
      if (selectedRadio) triggerChange(selectedRadio);
      return;
    }
    if (element.type === "checkbox") {
      element.checked = Boolean(value);
      triggerChange(element);
      return;
    }
    element.value = value == null ? "" : value;
    triggerChange(element);
  }

  function applyLanguage(selectedLanguage, suppliedTranslations) {
    language = selectedLanguage || "en";
    translations = Object.assign(
      {},
      builtInTranslations,
      suppliedTranslations || {}
    );
    translateTree(document.body);
  }

  function registerShinyHandlers() {
    if (handlersRegistered || !window.Shiny) return;
    handlersRegistered = true;

    Shiny.addCustomMessageHandler("fafa-language", function (message) {
      applyLanguage(message.language, message.translations);
    });

    Shiny.addCustomMessageHandler("fafa-restore-inputs", function (values) {
      Object.keys(values || {}).forEach(function (id) {
        restoreInput(id, values[id]);
      });
    });
  }

  function startTranslationObserver() {
    if (observerStarted || !document.body) return;
    observerStarted = true;
    const observer = new MutationObserver(function (mutations) {
      mutations.forEach(function (mutation) {
        mutation.addedNodes.forEach(function (node) {
          if (node.nodeType === Node.TEXT_NODE) translateTextNode(node);
          else if (node.nodeType === Node.ELEMENT_NODE) translateTree(node);
        });
      });
    });
    observer.observe(document.body, { childList: true, subtree: true });
  }

  function chooseOpeningLanguage(selectedLanguage) {
    applyLanguage(selectedLanguage);
    const gate = document.getElementById("fafa-language-gate");
    if (gate) gate.style.display = "none";
    restoreInput("project-app_language", selectedLanguage);
  }

  function initializeLanguageControls() {
    const turkishButton = document.getElementById("fafa-start-turkish");
    const englishButton = document.getElementById("fafa-start-english");
    if (turkishButton && !turkishButton.dataset.fafaBound) {
      turkishButton.dataset.fafaBound = "true";
      turkishButton.addEventListener("click", function () {
        chooseOpeningLanguage("tr");
      });
    }
    if (englishButton && !englishButton.dataset.fafaBound) {
      englishButton.dataset.fafaBound = "true";
      englishButton.addEventListener("click", function () {
        chooseOpeningLanguage("en");
      });
    }

    document.addEventListener("change", function (event) {
      if (event.target && event.target.id === "project-app_language") {
        applyLanguage(event.target.value);
      }
    });
  }

  function initialize() {
    initializeLanguageControls();
    startTranslationObserver();
    registerShinyHandlers();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialize, { once: true });
  } else {
    initialize();
  }

  document.addEventListener("shiny:connected", registerShinyHandlers);

  const shinyRetry = window.setInterval(function () {
    registerShinyHandlers();
    if (handlersRegistered) window.clearInterval(shinyRetry);
  }, 100);
})();
