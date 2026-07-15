use extendr_api::prelude::*;
use pdf_signer::{
    sign_pdf_file, verify_pdf_file, verify_pdf_file_with_roots, Appearance, PadesLevel,
    SignOptions, TrustStore,
};

/// Empty string -> None, otherwise Some.
fn opt(s: &str) -> Option<String> {
    if s.is_empty() {
        None
    } else {
        Some(s.to_string())
    }
}

/// Read a file into bytes when `path` is non-empty; an empty path means "none".
fn read_file_opt(path: &str) -> std::result::Result<Option<Vec<u8>>, Error> {
    if path.is_empty() {
        Ok(None)
    } else {
        std::fs::read(path)
            .map(Some)
            .map_err(|e| Error::Other(format!("reading '{path}': {e}")))
    }
}

/// Sign `pdf_file` with a PKCS#12 keystore, writing `output_file`.
///
/// When `visible` is TRUE, a bordered signature box with `appearance_text`
/// is drawn on `page` at `[x, y, width, height]`. A non-empty `font_path`
/// embeds a TrueType/OpenType font in the box; a non-empty `image_path` draws a
/// PNG/JPEG logo. Errors become R errors.
///
/// Internal: the user-facing wrapper is [`sign_pdf()`].
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
fn rust_sign_pdf(
    pdf_file: &str,
    output_file: &str,
    keystore_path: &str,
    keystore_password: &str,
    reason: &str,
    name: &str,
    location: &str,
    contact_info: &str,
    signing_time: &str,
    visible: bool,
    page: i32,
    x: f64,
    y: f64,
    width: f64,
    height: f64,
    font_size: f64,
    appearance_text: &str,
    border: bool,
    tsa_url: &str,
    pades_level: &str,
    font_path: &str,
    image_path: &str,
) -> std::result::Result<(), Error> {
    let level = match pades_level {
        "bt" => PadesLevel::Bt,
        "blt" => PadesLevel::Blt,
        "blta" => PadesLevel::Blta,
        _ => PadesLevel::Bb,
    };
    let appearance = visible
        .then(|| {
            Ok::<_, Error>(Appearance {
                page: page.max(1) as usize,
                x,
                y,
                width,
                height,
                font_size,
                text: appearance_text.to_string(),
                border,
                font: read_file_opt(font_path)?,
                image: read_file_opt(image_path)?,
                // Default placement: a square logo on the left, sized to the box.
                image_rect: None,
            })
        })
        .transpose()?;

    let opts = SignOptions {
        reason: opt(reason),
        name: opt(name),
        location: opt(location),
        contact_info: opt(contact_info),
        signing_time: opt(signing_time),
        appearance,
        tsa_url: opt(tsa_url),
        pades_level: level,
        ..Default::default()
    };

    sign_pdf_file(
        pdf_file,
        output_file,
        keystore_path,
        keystore_password,
        &opts,
    )
    .map_err(|e| Error::Other(e.to_string()))
}

/// Verify all signatures in `pdf_file`. Returns a list with one named list per
/// signature (`valid`, `signer`, `covers_whole_document`, `signed_len`,
/// `byte_range`, `detail`). An empty list means no signatures were found.
///
/// Internal: the user-facing wrapper is [`verify_pdf_signature()`].
/// @noRd
#[extendr]
fn rust_verify_pdf(
    pdf_file: &str,
    roots_pem_file: &str,
) -> std::result::Result<Robj, Error> {
    let report = if roots_pem_file.is_empty() {
        verify_pdf_file(pdf_file).map_err(|e| Error::Other(e.to_string()))?
    } else {
        let pem = std::fs::read(roots_pem_file).map_err(|e| Error::Other(e.to_string()))?;
        let store = TrustStore::from_pem(&pem).map_err(|e| Error::Other(e.to_string()))?;
        verify_pdf_file_with_roots(pdf_file, &store).map_err(|e| Error::Other(e.to_string()))?
    };

    let items: Vec<Robj> = report
        .signatures
        .iter()
        .map(|s| {
            let byte_range: Vec<f64> = s.byte_range.iter().map(|v| *v as f64).collect();
            list!(
                valid = s.valid,
                signer = s.signer.clone().unwrap_or_default(),
                chain_trusted = s.chain_trusted,
                covers_whole_document = s.covers_whole_document,
                signed_len = s.signed_len as f64,
                byte_range = byte_range,
                detail = s.detail.clone()
            )
            .into_robj()
        })
        .collect();

    Ok(List::from_values(items).into_robj())
}

extendr_module! {
    mod pdfsigner;
    fn rust_sign_pdf;
    fn rust_verify_pdf;
}
