#' @title Preprocess T1-weighted MRI scan for one patient
#'
#' @description This function preprocesses a raw T1-w MRI scan and
#' generates a segmentation MRI scan using the FAST algorithm.
#' The preprocesising steps comprises imhomogeneity correction
#' 'N4', registration to the MNI152 template with isotropic voxel size of 1mm^3
#' using the 'SyN' transformation, and skull stripping.
#'
#' @param mri.patient path of the T1-weighted scan.
#' @param folder.patient folder containing the T1-weighted scan. This folder usually refers to the patient.
#' @param atlas atlas template in NifTI format to spatially register the T1-weighted scans. By default the MNI152 atlas template is used.
#' @param mask brain mask in NifTI format of the atlas template to performed the skull stripping.
#' @param inhomogeneity inhomogeneity correction algorithm to be applied. The correction by default is the 'N4' bias correction.
#' @param transformation non-linear transformation for registering the T1-w MRI scan to the reference template. 'SyN' transformation is used by default.
#' @return paths of preprocessed MRI scans.
#' @author David Payares
#' @references Nicholas J. Tustison, Brian B. Avants, Philip A. Cook, Yuanjie Zheng, Alexander Egan, Paul A. Yushkevich, and James C. Gee. N4ITK: Improved N3 Bias Correction. IEEE Trans Med Imaging, 29:1310–1320, 2010.
#' @references B. B. Avants, C. L. Epstein, M Grossman, J. C. Gee Symmetric diffeomorphic image registration with cross-correlation: evaluating automated labeling of elderly and neurodegenerative brain. Medical Image Analysis, 12:1310–1320, 2008.
#' @references Evans, A.C., Fox, P.T., Lancaster, J., Zilles, K., Woods, R., Paus, T., Simpson, G., Pike, B., Holmes, C., Collins, D.L., Thompson, P., MacDonald, D., Iacoboni, et al. A probabilistic atlas and reference system for the human brain: International Consortium for Brain Mapping (ICBM). Philos. Trans. R. Soc. London B Biol, 356:1293-1322, 2001.
#' @references Yongyue Zhang, J. Michael Brady, Stephen Smith Hidden Markov random field model for segmentation of brain MR image. Medical Imaging 2000: Image Processing, 2000.
#' @references Jean-Philippe Fortin, Elizabeth M Sweeney, John Muschelli, Ciprian M Crainiceanu, Russell T Shinohara, Alzheimer’s Disease Neuroimaging Initiative, et al. Removing inter-subject technical variability in magnetic resonance imaging studies. NeuroImage, 132:198–212, 2016.
#' @examples
#'
#' \dontrun{
#' # Get general folder
#' folder <- system.file("extdata", package = "neurodata")
#' # Get covariates
#' covariates <- system.file("covariates.txt", package = "neurodata")
#' # Read covariates information
#' clinical_info <- read.csv(file = covariates, sep = ';')
#'
#' # Folder and T1-weighted file of the patient
#' patient_folder <- file.path(folder,"patient01")
#' patient_T1 <- file.path(patient_folder,"patient01_T1.nii.gz")
#'
#' # Getting preferred atlas template and template mask
#' # Using the MNI152 template available in the MNITemplate package
#' library(MNITemplate)
#' atlas <- getMNIPath()
#' atlas_mask <- readMNI("Brain_Mask")
#'
#' # Preprocessing the patient's sequences
#' patient_preprocessed_mri <- preprocess_modality_t1(mri.patient = patient_T1,
#'                                                   folder.patient = patient_folder,
#'                                                   atlas = atlas, mask = atlas_mask,
#'                                                   inhomogeneity = 'N4',
#'                                                   transformation = 'SyN')
#'
#'
#' }
#'
#' @export
preprocess_modality_t1 <- function(mri.patient, folder.patient, atlas, mask, inhomogeneity = "N4", transformation = "SyN"){

  # Empty list to save paths
  mri_paths <- list()

  # Inhomogeneity Correction: N4
  message(paste0('*********************************************\n****** Inhomogeneity Correction: ', inhomogeneity ,' *********\n*********************************************\n--Running...\n'))
  bias_files <- file.path(folder.patient, 'T1_bias.nii.gz')
  bias_mri <- extrantsr::bias_correct(mri.patient, correction = inhomogeneity, outfile = bias_files[[1]], verbose = FALSE)
  mri_paths[['bias']] <- bias_files
  message('--Complete.\n')

  # Registration to Template (SyN: Non-linear)
  message(paste0('*********************************************\n******* Spatial Registration : ',transformation , ' **********\n*********************************************\n--Running...\n'))
  syn_file <-file.path(folder.patient, 'T1_SyN.nii.gz')
  syn_mri <- extrantsr::ants_regwrite(filename = bias_mri, outfile = syn_file, template.file = atlas, typeofTransform = transformation, verbose = FALSE)
  mri_paths[['registered']] <- syn_file
  message('--Complete.\n')

  # Brain Mask
  message('*********************************************\n*************** Brain Mask ******************\n*********************************************\n--Running...\n')
  mask_file <- file.path(folder.patient, 'T1_masked_SyN')
  mask_mri = fslr::fslmask(syn_file, mask, verbose = FALSE)
  oro.nifti::writeNIfTI(mask_mri, mask_file)
  mri_paths[['striped']] <- paste0(mask_file,'.nii.gz')
  message('--Complete.\n')

  # Spatially Informed layer (Segmentation layer)
  message('*********************************************\n******** Segmentation (HMRF) ***********\n*********************************************\n--Running...\n')
  spatial_file <- file.path(folder.patient, 'T1_spatially_informed.nii.gz')
  spatial_mri = fslr::fast(file = mask_mri, outfile = spatial_file, opts = "--nobias", verbose = FALSE)
  mri_paths[['segmented']] <- file.path(folder.patient, 'T1_spatially_informed_pveseg.nii.gz')
  message('--Complete.\n')

  # CSF tissue mask for RAVEL algorithm
  message('*********************************************\n********* CSF Tissue Mask (RAVEL) ***********\n*********************************************\n--Running...\n')
  tissue_mask <- spatial_mri
  tissue_mask[tissue_mask != 1] <- 0
  tissue_mask_file = file.path(folder.patient, 'T1_CSF_tissue')
  oro.nifti::writeNIfTI(tissue_mask, tissue_mask_file)
  mri_paths[['csf_mask']] <- paste0(tissue_mask_file,'.nii.gz')
  message('--Complete.\n\n')

  # Path for RAVEL
  ravel_file = file.path(folder.patient, paste0(folder.patient, '_T1_Ravel_norm'))
  mri_paths[['ravel']] <- paste0(ravel_file ,'.nii.gz')

  return(mri_paths)
}


#' @title Preprocess group of MRI scan for one patient
#'
#' @description This function preprocesses raw T1-weighted, T2-weighted and
#' FLAIR MRI scans and generates a segmentation MRI scan using the FAST algorithm.
#' The preprocesising steps comprises imhomogeneity correction 'N4', coregistration
#' of other sequences to the T1-weighted scan, non-linear registration to the MNI152
#' template with an isotropic voxel size of 1mm, using the 'SyN' transformation,
#' skull stripping, brain segmentation and intensity normalization using the RAVEL
#' or White Stripe algorithms.
#'
#' @param mri.patient path of the MRI scans.
#' @param folder.patient folder containing the MRI scans. This folder usually refers to the patient.
#' @param atlas atlas template in NifTI format to spatially register the MRI scans. By default the MNI152 atlas template is used.
#' @param modalities vector of strings containing the modalities to be preprocessed. It must always contains the T1-weighted sequence scan.
#' @param mask brain mask in NifTI format of the atlas template to performed the skull stripping.
#' @param inhomogeneity inhomogeneity correction algorithm to be applied. The correction by default is the 'N4' bias correction.
#' @param transformation non-linear transformation for registering the T1-w MRI scan to the reference template. 'SyN' transformation is used by default.
#' @return paths of preprocessed MRI scans.
#' @author David Payares
#' @references Nicholas J. Tustison, Brian B. Avants, Philip A. Cook, Yuanjie Zheng, Alexander Egan, Paul A. Yushkevich, and James C. Gee. N4ITK: Improved N3 Bias Correction. IEEE Trans Med Imaging, 29:1310–1320, 2010.
#' @references B. B. Avants, C. L. Epstein, M Grossman, J. C. Gee Symmetric diffeomorphic image registration with cross-correlation: evaluating automated labeling of elderly and neurodegenerative brain. Medical Image Analysis, 12:1310–1320, 2008.
#' @references Evans, A.C., Fox, P.T., Lancaster, J., Zilles, K., Woods, R., Paus, T., Simpson, G., Pike, B., Holmes, C., Collins, D.L., Thompson, P., MacDonald, D., Iacoboni, et al. A probabilistic atlas and reference system for the human brain: International Consortium for Brain Mapping (ICBM). Philos. Trans. R. Soc. London B Biol, 356:1293-1322, 2001.
#' @references Yongyue Zhang, J. Michael Brady, Stephen Smith Hidden Markov random field model for segmentation of brain MR image. Medical Imaging 2000: Image Processing, 2000.
#' @references Jean-Philippe Fortin, Elizabeth M Sweeney, John Muschelli, Ciprian M Crainiceanu, Russell T Shinohara, Alzheimer’s Disease Neuroimaging Initiative, et al. Removing inter-subject technical variability in magnetic resonance imaging studies. NeuroImage, 132:198–212, 2016.
#' @examples
#'
#' \dontrun{
#' # Get general folder
#' folder <- system.file("extdata", package = "neurodata")
#' # Get covariates
#' covariates <- system.file("covariates.txt", package = "neurodata")
#' # Read covariates information
#' clinical_info <- read.csv(file = covariates, sep = ';')
#'
#' # Folder of the patient
#' patient_folder <- file.path(folder,"patient01")
#'
#' # Getting the paths of the MRI scan sequences for one patient
#' # the NeuroNorm built-in function load_mri_patient() can be used for this.
#' sequences <- load_mri_patient(patient_folder)
#'
#' # Getting preferred atlas template and template mask
#' # Using the MNI152 template available in the MNITemplate package
#' library(MNITemplate)
#' atlas <- getMNIPath()
#' atlas_mask <- readMNI("Brain_Mask")
#'
#' # Preprocessing the patient's sequences
#' patient_preprocessed_mri <- preprocess_modalities(mri.patient = sequences,
#'                                                   folder.patient = patient_folder,
#'                                                   modalities = c('T1','T2','FLAIR'),
#'                                                   atlas = atlas, mask = atlas_mask,
#'                                                   inhomogeneity = 'N4',
#'                                                   transformation = 'SyN')
#'
#'
#' }
#'
#'@export
preprocess_modalities <- function(mri.patient, folder.patient, modalities, atlas, mask, inhomogeneity = "N4", transformation = "SyN"){

  # empty list to save paths
  mri_paths <- list()

  # Inhomogeneity Correction: N4
  message(paste0('*********************************************\n****** Inhomogeneity Correction: ', inhomogeneity ,' *********\n*********************************************\n--Running...\n'))
  bias_files <- lapply(modalities, function(x) file.path(folder.patient, paste0( x, '_bias.nii.gz')))
  bias_mri <- mapply(extrantsr::bias_correct, file = mri.patient, correction = inhomogeneity, outfile = bias_files, verbose = FALSE, SIMPLIFY = FALSE)
  mri_paths[['bias']]<- unlist(bias_files)
  message('--Complete.\n')

  # Coregistration to T1-weighted image : Rigid Transformation
  if (length(modalities) > 1){
    message(paste0('*********************************************\n****** Coregistration to T1 sequence ********\n*********************************************\n--Running...\n'))
    coregisteredImg <- extrantsr::within_visit_registration(fixed = mri.patient$T1, moving = bias_files[2:length(mri.patient)], typeofTransform = "Rigid", interpolator = "Linear", verbose = FALSE)
    bias_mri_comp <- coregistration_images(coregisteredImg)
    message('--Complete.\n')
  }

  # Registration to Template (SyN: Non-linear)
  message(paste0('*********************************************\n******* Spatial Registration : ',transformation , ' **********\n*********************************************\n--Running...\n'))
  syn_files <- lapply(modalities, function(x) file.path(folder.patient, paste0( x, '_SyN_MNI152.nii.gz')))
  if (length(modalities) > 1){
    bias_mris <- create_bias_list(modalities, bias_mri$T1, bias_mri_comp)
    syn_mri <- mapply(extrantsr::ants_regwrite, filename = bias_mris, outfile = syn_files, template.file = atlas, typeofTransform = transformation,  verbose = FALSE)
  }else{
    syn_mri <- extrantsr::ants_regwrite(filename = bias_files[[1]], outfile = syn_files[[1]], template.file = atlas, typeofTransform = transformation, verbose = FALSE)
  }
  mri_paths[['registered']] <- unlist(syn_files)
  message('--Complete.\n')

  # Brain Mask
  message('*********************************************\n*************** Brain Mask ******************\n*********************************************\n--Running...\n')
  mask_files <- lapply(modalities, function(x) file.path(folder.patient, paste0( x, '_masked')))
  mask_mri <- lapply(syn_files, fslr::fslmask, mask, verbose = FALSE)
  mapply( oro.nifti::writeNIfTI, nim = mask_mri, filename = mask_files)
  mri_paths[['stripped']] <- paste0(mask_files,'.nii.gz')
  message('--Complete.\n')

  # Spatially Informed layer
  message('*********************************************\n******** Brain Segmentation ***********\n*********************************************\n--Running...\n')
  spatial_file <- file.path(folder.patient, 'T1_spatially_informed.nii.gz')
  spatial_mri = fslr::fast(file = mask_mri[[1]], outfile = spatial_file, opts = "--nobias", verbose = FALSE)
  mri_paths[['segmented']] <- file.path(folder.patient, 'T1_spatially_informed_pveseg.nii.gz')
  message('--Complete.\n')

  # CSF tissue mask for RAVEL algorithm
  message('*********************************************\n********* CSF Tissue Mask (RAVEL) ***********\n*********************************************\n--Running...\n')
  tissue_mask <- spatial_mri
  tissue_mask[tissue_mask != 1] <- 0
  tissue_mask_file = file.path(folder.patient, 'T1_CSF_tissue')
  oro.nifti::writeNIfTI(tissue_mask, tissue_mask_file)
  mri_paths[['csf_mask']] <- paste0(tissue_mask_file,'.nii.gz')
  message('--Complete.\n\n')

  # Path for RAVEL
  ravel_file = file.path(folder.patient, 'T1_Ravel_norm')
  mri_paths[['ravel']] <- paste0(ravel_file ,'.nii.gz')

  return(mri_paths)
}


#' @title Wrapper function for RAVEL normalization of T1-weighted images
#'
#' @description Ravel intensity normalization using control voxels and clinical covariates.
#' @param masked.paths list or vector of paths of the preprocessed input NIfTI images to be normalized.
#' @param csf.paths NIfTI image paths for the binary control region masks.
#' @param ravel.paths list or vector of paths of the output NIfTI images.
#' @param demographics table of covariates associated to the MRI scans. Number of rows should be equal to the number of images.
#' @param brain.mask NIfTI image path for the binary brain mask. Must have value 1 for the brain tissue and 0 otherwise.
#' @param patients.folder folder to save the output control mask.
#' @param modality string describing the modality to perform the normalization. It should be one of T1, T2 or FLAIR.
#' @return RAVEL-corrected images are saved in disk.
#' @author David Payares
#' @references Jean-Philippe Fortin, Elizabeth M Sweeney, John Muschelli, Ciprian M Crainiceanu, Russell T Shinohara, Alzheimer’s Disease Neuroimaging Initiative, et al. Removing inter-subject technical variability in magnetic resonance imaging studies. NeuroImage, 132:198–212, 2016.
#' @importFrom stats model.matrix
#' @examples
#'
#' \dontrun{
#' # Get general folder
#' folder <- system.file("extdata", package = "neurodata")
#' # Get covariates
#' covariates <- system.file("covariates.txt", package = "neurodata")
#' # Read covariates information
#' clinical_info <- read.csv(file = covariates, sep = ';')
#'
#' # Defining the RAVEL output files for the patients
#' # with a T2-weighted sequence (patient 1,2 and 4)
#' patients <- c(1,2,4)
#' output_files <- lapply(patients, function(x) {
#'            file.path(folder, paste0("patient0",x),"T2_ravel.nii.gz")})
#'
#' # Getting the files of the preprocessed images (without intensity normalization)
#' # and the CSF masks computed by the preprocessing.
#' csf_paths <- lapply(paths_preprocess_patients[patients], function(x){x$csf_mask})
#' masked_paths <- lapply(paths_preprocess_patients[patients], function(x){x$stripped[2]})
#'
#' Subseting covariares info
#' cov_pat <- clinical_info[clinical_info$patient %in% patients,]
#'
#' Normalizing T2 sequences with RAVEL
#' image_normalization_ravel(masked.paths = masked_paths, csf.paths = csf_paths,
#'                          ravel.paths = output_files, demographics = cov_pat,
#'                          brain.mask = atlas_mask, patients.folder = folder,
#'                          modality = "T2")
#'
#' }
#'
#' @export
### Ravel Normalization
image_normalization_ravel <- function(masked.paths, csf.paths, ravel.paths, demographics, brain.mask, patients.folder, modality = 'T1'){

  ### Control region mask for all patients (masks intersect)
  mask_intersect_path <- file.path(patients.folder, 'CSF_control_mask.nii.gz')
  intersect_mask <- RAVEL::maskIntersect(csf.paths, output.file = mask_intersect_path , prob = 0.8)

  # Control for biological covariates
  mod_cov <- stats::model.matrix(~., demographics[,-1])

  #RAVEL
  RAVEL::normalizeRAVEL(input.files = masked.paths, output.files = ravel.paths, brain.mask = brain.mask,
                 control.mask = intersect_mask, mod = mod_cov , WhiteStripe_Type	= modality,
                 k = 1, returnMatrix = FALSE , writeToDisk = TRUE)
}


#' @title Preprocess MRI scans for multiple patients
#'
#' @description This function preprocesses raw T1-weighted, T2-weighted and/or FLAIR MRI scans and generates a brain segmentation MRI scans using the FAST algorithm.
#' The preprocessing steps comprise imhomogeneity correction 'N4', linear coregistration of T2-weighted and/or FLAIR to the T1-weighted, registration of all available modalities to the MNI152 template with an isotropic voxel size of 1mm^3
#' using the 'SyN' transformation, skull stripping, and RAVEL intensity normalization.
#'
#' @param patients.folder general folder containing sub-folders per patient with raw MRI images.
#' @param clinical.covariates data.frame of covariates associated to the MRI scans. Number of rows should be equal to the number of images.
#' @return paths of preprocessed MRI scans. MRI preprocessed images are stored in the patient's folder.
#' @author David Payares
#' @references Nicholas J. Tustison, Brian B. Avants, Philip A. Cook, Yuanjie Zheng, Alexander Egan, Paul A. Yushkevich, and James C. Gee. N4ITK: Improved N3 Bias Correction. IEEE Trans Med Imaging, 29:1310–1320, 2010.
#' @references B. B. Avants, C. L. Epstein, M Grossman, J. C. Gee Symmetric diffeomorphic image registration with cross-correlation: evaluating automated labeling of elderly and neurodegenerative brain. Medical Image Analysis, 12:1310–1320, 2008.
#' @references Evans, A.C., Fox, P.T., Lancaster, J., Zilles, K., Woods, R., Paus, T., Simpson, G., Pike, B., Holmes, C., Collins, D.L., Thompson, P., MacDonald, D., Iacoboni, et al. A probabilistic atlas and reference system for the human brain: International Consortium for Brain Mapping (ICBM). Philos. Trans. R. Soc. London B Biol, 356:1293-1322, 2001.
#' @references Yongyue Zhang, J. Michael Brady, Stephen Smith Hidden Markov random field model for segmentation of brain MR image. Medical Imaging 2000: Image Processing, 2000.
#' @references Jean-Philippe Fortin, Elizabeth M Sweeney, John Muschelli, Ciprian M Crainiceanu, Russell T Shinohara, Alzheimer’s Disease Neuroimaging Initiative, et al. Removing inter-subject technical variability in magnetic resonance imaging studies. NeuroImage, 132:198–212, 2016.
#' @examples
#'
#' \dontrun{
#' # Get general folder
#' folder <- system.file("extdata", package = "neurodata")
#' # Get covariates
#' covariates <- system.file("covariates.txt", package = "neurodata")
#' # Read covariates information
#' clinical_info <- read.csv(file = covariates, sep = ';')
#' # Preprocess MRI scans: 'N4' inhomogeneity correction,
#' # 'SyN' non-linear transformation to MNI152 atlas template
#'
#' # Brain extraction, Spatial informed MRI scan , a.k.a., brain segmentation
#' # and RAVEL intensity normalization only for T1-w images.
#' paths_preprocess_patients <- preprocess_patients(folder, clinical_info)
#' # Outputs paths of the preprocessed MRI scans per patient
#' # and applied preprocessing.
#' paths_preprocess_patients$patient02
#' }
#'
#' @export
preprocess_patients <- function(patients.folder, clinical.covariates){

  if(missing(clinical.covariates)) {
    message("No covariates provided. Intensity normalization step will use the White Stripe algorithm instead of RAVEL.\n")
  } else {
    if(nrow(clinical.covariates) < 1){
      stop('no covariates provided. File is empty.')
    }
  }

  # getting patients scans
  patients_mri <- load_mri_group(patients.folder)

  # empty list to save paths
  paths_mri <- list()

  # Atlas template (MNI152)
  mniTemplate <- MNITemplate::getMNIPath()

  # Atlas brain mask (MNI152)
  brainMask <- MNITemplate::readMNI("Brain_Mask")

  # preprocess each patients T1 sequence scan
  for (patient in patients_mri){

    # selecting patient's folder
    patient_folder <- dirname(patient$T1)

    # getting modalities
    modalities <- get_modalities(patient)

    # getting folder name
    folder_name = unlist(strsplit(patient_folder, '/'))
    folder_name = folder_name[length(folder_name)]

    # Preprocess images
    message(paste0('--------------------------------------------------\n Preprocesing images in ',folder_name ,'\n--------------------------------------------------\n\n'))
    patient_scans <- preprocess_modalities(patient, patient_folder, modalities, mniTemplate, brainMask)

    # Save preprocessed images paths
    paths_mri[[folder_name]] <- patient_scans
  }

  ### RAVEL

  ## Group paths for ravel
  ravel_paths <- lapply(paths_mri, function(x){x$ravel})
  csf_paths <- lapply(paths_mri, function(x){x$csf_mask})
  masked_paths <- lapply(paths_mri, function(x){x$stripped})
  masked_paths_T1 <- lapply(masked_paths, function(x) x[grepl("T1", x)])

  print(ravel_paths)
  print(csf_paths)
  print(masked_paths_T1)


  if(missing(clinical.covariates)) {
    message('*********************************************\n******** White Stripe normalization **********\n*********************************************\n--Running...\n')
    RAVEL::normalizeWS(input.files = masked_paths_T1, output.files = ravel_paths, brain.mask = brainMask, WhiteStripe_Type = "T1", returnMatrix = FALSE , writeToDisk = TRUE)
  } else {
    if (length(ravel_paths) < nrow(clinical.covariates)){
      stop("more covariates information than images. Information per MRI scan should be provided.")
    }

    if (length(ravel_paths) > nrow(clinical.covariates)){
      stop("more images than covariates information. Information per MRI scan should be provided.")
    }

    if (length(ravel_paths) < ncol(clinical.covariates)){
      stop("more covariates than images. Number of covariates can not be greater than number of images.")
    }

    ## ravel algorithm
    image_normalization_ravel(masked_paths_T1, csf_paths, ravel_paths, clinical.covariates, brainMask, patients.folder)
  }

  message(paste0('--------------------------------------------------\n Preprocessing Complete \n--------------------------------------------------\n\n'))
  return(paths_mri)
}


