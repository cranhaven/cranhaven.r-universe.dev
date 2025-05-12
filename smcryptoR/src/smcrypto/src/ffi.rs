use libc::{c_char, c_uchar, size_t, c_int};

use crate::{sm3, sm2, sm4};
use std::ffi::{CStr, CString};
use std::panic::catch_unwind;
use std::slice;

#[no_mangle]
pub extern "C" fn sm3_hash(msg: *const c_uchar, len: size_t) -> *mut c_char {
    let msg_c = {
        assert!(!msg.is_null());
        unsafe { slice::from_raw_parts(msg, len) }
    };
    let hash_result = sm3::sm3_hash(msg_c);
    let hash_result_c = CString::new(hash_result).expect("CString::new failed");
    hash_result_c.into_raw()
}

#[no_mangle]
pub extern fn sm3_hash_string(msg_str: *const c_char) -> *mut c_char {
    let msg_str_c = {
        assert!(!msg_str.is_null());
        unsafe { CStr::from_ptr(msg_str) }
    };
    let msg_str_rs = msg_str_c.to_str().expect("not a valid utf-8 string");
    let hash_result = sm3::sm3_hash_string(msg_str_rs);
    let hash_result_c = CString::new(hash_result).expect("null byte in the middle");
    hash_result_c.into_raw()
}

#[no_mangle]
pub extern "C" fn sm3_hash_file(file_path: *const c_char) -> *mut c_char {
    let file_path_c = {
        assert!(!file_path.is_null());
        unsafe { CStr::from_ptr(file_path) }
    };
    let file_path_rs = file_path_c.to_str().expect("not a valid utf-8 string");
    let hash_result = sm3::sm3_hash_file(file_path_rs);
    let hash_result_c = CString::new(hash_result).expect("null byte in the middle");
    hash_result_c.into_raw()
}

#[repr(C)]
pub struct Keypair {
    pub private_key: *mut c_char,
    pub public_key: *mut c_char,
}

#[no_mangle]
pub extern "C" fn gen_keypair() -> *mut Keypair {
    let (private_key, public_key) = sm2::gen_keypair();
    let private_key_c = CString::new(private_key).expect("CString::new failed");
    let public_key_c = CString::new(public_key).expect("CString::new failed");
    Box::into_raw(Box::new(
        Keypair {
            private_key: private_key_c.into_raw(),
            public_key: public_key_c.into_raw(),
        }
    ))
}

#[no_mangle]
pub extern "C" fn pk_from_sk(private_key: *const c_char) -> *mut c_char {
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let public_key = sm2::pk_from_sk(private_key_rs);
    let public_key_c = CString::new(public_key).expect("CString::new failed");
    public_key_c.into_raw()
}

#[no_mangle]
pub extern "C" fn privkey_valid(private_key: *const c_char) -> c_int {
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    match sm2::privkey_valid(private_key_rs) {
        true => 1,
        false => 0,
    }
}

#[no_mangle]
pub extern "C" fn pubkey_valid(public_key: *const c_char) -> c_int {
    let public_key_c = {
        assert!(!public_key.is_null());
        unsafe { CStr::from_ptr(public_key) }
    };
    let public_key_rs = public_key_c.to_str().expect("not a valid utf-8 string");
    match sm2::pubkey_valid(public_key_rs) {
        true => 1,
        false => 0,
    }
}

#[no_mangle]
pub extern "C" fn hex_valid(input: *const c_char) -> c_int {
    let input_c = {
        assert!(!input.is_null());
        unsafe { CStr::from_ptr(input) }
    };
    let input_rs = input_c.to_str().expect("not a valid utf-8 string");
    match sm2::hex_valid(input_rs) {
        true => 1,
        false => 0,
    }
}

#[no_mangle]
pub extern "C" fn base64_valid(input: *const c_char) -> c_int {
    let input_c = {
        assert!(!input.is_null());
        unsafe { CStr::from_ptr(input) }
    };
    let input_rs = input_c.to_str().expect("not a valid utf-8 string");
    match sm2::base64_valid(input_rs) {
        true => 1,
        false => 0,
    }
}

#[no_mangle]
pub extern "C" fn keypair_from_pem_file(pem_file: *const c_char) -> *mut Keypair {
    let pem_file_c = {
        assert!(!pem_file.is_null());
        unsafe { CStr::from_ptr(pem_file) }
    };
    let pem_file_rs = pem_file_c.to_str().expect("not a valid utf-8 string");
    let (private_key, public_key) = sm2::keypair_from_pem_file(pem_file_rs);
    let private_key_c = CString::new(private_key).expect("null byte in the middle");
    let public_key_c = CString::new(public_key).expect("null byte in the middle");
    Box::into_raw(Box::new(
        Keypair {
            private_key: private_key_c.into_raw(),
            public_key: public_key_c.into_raw(),
        }
    ))
}

#[no_mangle]
pub extern "C" fn keypair_to_pem_file(private_key: *const c_char, pem_file: *const c_char) {
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let pem_file_c = {
        assert!(!pem_file.is_null());
        unsafe { CStr::from_ptr(pem_file) }
    };
    let pem_file_rs = pem_file_c.to_str().expect("not a valid utf-8 string");
    sm2::keypair_to_pem_file(private_key_rs, pem_file_rs);
}

#[no_mangle]
pub extern "C" fn pubkey_from_pem_file(pem_file: *const c_char) -> *mut c_char {
    let pem_file_c = {
        assert!(!pem_file.is_null());
        unsafe { CStr::from_ptr(pem_file) }
    };
    let pem_file_rs = pem_file_c.to_str().expect("not a valid utf-8 string");
    let public_key = sm2::pubkey_from_pem_file(pem_file_rs);
    let public_key_c = CString::new(public_key).expect("null byte in the middle");
    public_key_c.into_raw()
}

#[no_mangle]
pub extern "C" fn pubkey_to_pem_file(public_key: *const c_char, pem_file: *const c_char) {
    let public_key_c = {
        assert!(!public_key.is_null());
        unsafe { CStr::from_ptr(public_key) }
    };
    let public_key_rs = public_key_c.to_str().expect("not a valid utf-8 string");
    let pem_file_c = {
        assert!(!pem_file.is_null());
        unsafe { CStr::from_ptr(pem_file) }
    };
    let pem_file_rs = pem_file_c.to_str().expect("not a valid utf-8 string");
    sm2::pubkey_to_pem_file(public_key_rs, pem_file_rs);
}

#[no_mangle]
pub extern "C" fn sign(
    id: *const c_uchar, id_len: size_t,
    data: *const c_uchar, data_len: size_t,
    private_key: *const c_char, sig_len: *mut usize
) -> *mut c_uchar {
    let id_c = {
        assert!(!id.is_null());
        unsafe { slice::from_raw_parts(id, id_len) }
    };
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let sign_ctx = sm2::Sign::new_with_id(id_c, private_key_rs);
    let mut sign_rs = sign_ctx.sign(data_c);
    sign_rs.shrink_to_fit();
    let ptr = sign_rs.as_ptr() as *mut c_uchar;
    unsafe {
        *sig_len = sign_rs.len();
    }
    std::mem::forget(sign_rs);
    ptr
}

#[no_mangle]
pub extern "C" fn verify(
    id: *const c_uchar, id_len: size_t,
    data: *const c_uchar, data_len: size_t,
    sign: *const c_uchar, sign_len: size_t,
    public_key: *const c_char
) -> c_int {
    let id_c = {
        assert!(!id.is_null());
        unsafe { slice::from_raw_parts(id, id_len) }
    };
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let sign_c = {
        assert!(!sign.is_null());
        unsafe { slice::from_raw_parts(sign, sign_len) }
    };
    let public_key_c = {
        assert!(!public_key.is_null());
        unsafe { CStr::from_ptr(public_key) }
    };
    let public_key_rs = public_key_c.to_str().expect("not a valid utf-8 string");
    let verify_ctx = sm2::Verify::new_with_id(id_c, public_key_rs);
    let verify_rs = verify_ctx.verify(data_c, sign_c);
    match verify_rs {
        true => 1,
        false => 0,
    }
}

#[no_mangle]
pub extern "C" fn sign_to_file(
    id: *const c_uchar, id_len: size_t,
    data: *const c_uchar, data_len: size_t,
    sign_file: *const c_char,
    private_key: *const c_char
) {
    let id_c = {
        assert!(!id.is_null());
        unsafe { slice::from_raw_parts(id, id_len) }
    };
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let sign_file_c = {
        assert!(!sign_file.is_null());
        unsafe { CStr::from_ptr(sign_file) }
    };
    let sign_file_rs = sign_file_c.to_str().expect("not a valid utf-8 string");
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let sign_ctx = sm2::Sign::new_with_id(id_c, private_key_rs);
    sign_ctx.sign_to_file(data_c, sign_file_rs);
}

#[no_mangle]
pub extern "C" fn verify_from_file(
    id: *const c_uchar, id_len: size_t,
    data: *const c_uchar, data_len: size_t,
    sign_file: *const c_char,
    public_key: *const c_char
) -> c_int {
    let id_c = {
        assert!(!id.is_null());
        unsafe { slice::from_raw_parts(id, id_len) }
    };
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let sign_file_c = {
        assert!(!sign_file.is_null());
        unsafe { CStr::from_ptr(sign_file) }
    };
    let sign_file_rs = sign_file_c.to_str().expect("not a valid utf-8 string");
    let public_key_c = {
        assert!(!public_key.is_null());
        unsafe { CStr::from_ptr(public_key) }
    };
    let public_key_rs = public_key_c.to_str().expect("not a valid utf-8 string");
    let verify_ctx = sm2::Verify::new_with_id(id_c, public_key_rs);
    match verify_ctx.verify_from_file(data_c, sign_file_rs) {
        true => 1,
        false => 0,
    }
}

#[no_mangle]
pub extern "C" fn encrypt(
    data: *const c_uchar, data_len: size_t,
    public_key: *const c_char, enc_len: *mut size_t
) -> *mut c_uchar {
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let public_key_c = {
        assert!(!public_key.is_null());
        unsafe { CStr::from_ptr(public_key) }
    };
    let public_key_rs = public_key_c.to_str().expect("not a valid utf-8 string");
    let enc_ctx = sm2::Encrypt::new(public_key_rs);
    let mut enc_data = enc_ctx.encrypt(data_c);
    enc_data.shrink_to_fit();
    let ptr = enc_data.as_ptr() as *mut c_uchar;
    unsafe {
        *enc_len = enc_data.len();
    }
    std::mem::forget(enc_data);
    ptr
}

#[no_mangle]
pub extern "C" fn decrypt(
    data: *const c_uchar, data_len: size_t,
    private_key: *const c_char, dec_len: *mut size_t
) -> *mut c_uchar {
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let dec_ctx = sm2::Decrypt::new(private_key_rs);
    let dec_data_try = catch_unwind(|| {
        dec_ctx.decrypt(data_c)
    });
    if let Ok(mut dec_data) = dec_data_try {
        dec_data.shrink_to_fit();
        if dec_data.len() == 0 {
            dec_data = vec![0x00];
        }
        let ptr = dec_data.as_ptr() as *mut c_uchar;
        unsafe {
            *dec_len = dec_data.len();
        }
        std::mem::forget(dec_data);
        ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn encrypt_c1c2c3(
    data: *const c_uchar, data_len: size_t,
    public_key: *const c_char, enc_len: *mut size_t
) -> *mut c_uchar {
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let public_key_c = {
        assert!(!public_key.is_null());
        unsafe { CStr::from_ptr(public_key) }
    };
    let public_key_rs = public_key_c.to_str().expect("not a valid utf-8 string");
    let enc_ctx = sm2::Encrypt::new(public_key_rs);
    let mut enc_data = enc_ctx.encrypt_c1c2c3(data_c);
    enc_data.shrink_to_fit();
    let ptr = enc_data.as_ptr() as *mut c_uchar;
    unsafe {
        *enc_len = enc_data.len();
    }
    std::mem::forget(enc_data);
    ptr
}

#[no_mangle]
pub extern "C" fn decrypt_c1c2c3(
    data: *const c_uchar, data_len: size_t,
    private_key: *const c_char, dec_len: *mut size_t
) -> *mut c_uchar {
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let dec_ctx = sm2::Decrypt::new(private_key_rs);
    let dec_data_try = catch_unwind(|| {
        dec_ctx.decrypt_c1c2c3(data_c)
    });
    if let Ok(mut dec_data) = dec_data_try {
        dec_data.shrink_to_fit();
        if dec_data.len() == 0 {
            dec_data = vec![0x00];
        }
        let ptr = dec_data.as_ptr() as *mut c_uchar;
        unsafe {
            *dec_len = dec_data.len();
        }
        std::mem::forget(dec_data);
        ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn encrypt_asna1(
    data: *const c_uchar, data_len: size_t,
    public_key: *const c_char, enc_len: *mut size_t
) -> *mut c_uchar {
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let public_key_c = {
        assert!(!public_key.is_null());
        unsafe { CStr::from_ptr(public_key) }
    };
    let public_key_rs = public_key_c.to_str().expect("not a valid utf-8 string");
    let enc_ctx = sm2::Encrypt::new(public_key_rs);
    let mut enc_data = enc_ctx.encrypt_asna1(data_c);
    enc_data.shrink_to_fit();
    let ptr = enc_data.as_ptr() as *mut c_uchar;
    unsafe {
        *enc_len = enc_data.len();
    }
    std::mem::forget(enc_data);
    ptr
}

#[no_mangle]
pub extern "C" fn decrypt_asna1(
    data: *const c_uchar, data_len: size_t,
    private_key: *const c_char, dec_len: *mut size_t
) -> *mut c_uchar {
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let dec_ctx = sm2::Decrypt::new(private_key_rs);
    let dec_data_try = catch_unwind(|| {
        dec_ctx.decrypt_asna1(data_c)
    });
    if let Ok(mut dec_data) = dec_data_try {
        dec_data.shrink_to_fit();
        if dec_data.len() == 0 {
            dec_data = vec![0x00];
        }
        let ptr = dec_data.as_ptr() as *mut c_uchar;
        unsafe {
            *dec_len = dec_data.len();
        }
        std::mem::forget(dec_data);
        ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn encrypt_hex(
    data: *const c_uchar, data_len: size_t,
    public_key: *const c_char
) -> *mut c_char {
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let public_key_c = {
        assert!(!public_key.is_null());
        unsafe { CStr::from_ptr(public_key) }
    };
    let public_key_rs = public_key_c.to_str().expect("not a valid utf-8 string");
    let enc_ctx = sm2::Encrypt::new(public_key_rs);
    let enc_data = enc_ctx.encrypt_hex(data_c);
    let enc_data = CString::new(enc_data).expect("CString::new failed");
    enc_data.into_raw()
}

#[no_mangle]
pub extern "C" fn decrypt_hex(
    data: *const c_char, private_key: *const c_char,
    dec_len: *mut size_t
) -> *mut c_uchar {
    let data_c = {
        assert!(!data.is_null());
        unsafe { CStr::from_ptr(data) }
    };
    let data_rs = data_c.to_str().expect("not a valid utf-8 string");
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let dec_ctx = sm2::Decrypt::new(private_key_rs);
    let dec_data_try = catch_unwind(|| {
        dec_ctx.decrypt_hex(data_rs)
    });
    if let Ok(mut dec_data) = dec_data_try {
        dec_data.shrink_to_fit();
        if dec_data.len() == 0 {
            dec_data = vec![0x00];
        }
        let ptr = dec_data.as_ptr() as *mut c_uchar;
        unsafe {
            *dec_len = dec_data.len();
        }
        std::mem::forget(dec_data);
        ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn encrypt_base64(
    data: *const c_uchar, data_len: size_t,
    public_key: *const c_char
) -> *mut c_char {
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let public_key_c = {
        assert!(!public_key.is_null());
        unsafe { CStr::from_ptr(public_key) }
    };
    let public_key_rs = public_key_c.to_str().expect("not a valid utf-8 string");
    let enc_ctx = sm2::Encrypt::new(public_key_rs);
    let enc_data = enc_ctx.encrypt_base64(data_c);
    let enc_data_c = CString::new(enc_data).expect("CString::new failed");
    enc_data_c.into_raw()
}

#[no_mangle]
pub extern "C" fn decrypt_base64(
    data: *const c_char, private_key: *const c_char,
    dec_len: *mut size_t
) -> *mut c_uchar {
    let data_c = {
        assert!(!data.is_null());
        unsafe { CStr::from_ptr(data) }
    };
    let data_rs = data_c.to_str().expect("not a valid utf-8 string");
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let dec_ctx = sm2::Decrypt::new(private_key_rs);
    let dec_data_try = catch_unwind(|| {
        dec_ctx.decrypt_base64(data_rs)
    });
    if let Ok(mut dec_data) = dec_data_try {
        dec_data.shrink_to_fit();
        if dec_data.len() == 0 {
            dec_data = vec![0x00];
        }
        let ptr = dec_data.as_ptr() as *mut c_uchar;
        unsafe {
            *dec_len = dec_data.len();
        }
        std::mem::forget(dec_data);
        ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn encrypt_to_file(
    data: *const c_uchar, data_len: size_t,
    enc_file: *const c_char, public_key: *const c_char
) {
    let data_c = {
        assert!(!data.is_null());
        unsafe { slice::from_raw_parts(data, data_len) }
    };
    let public_key_c = {
        assert!(!public_key.is_null());
        unsafe { CStr::from_ptr(public_key) }
    };
    let public_key_rs = public_key_c.to_str().expect("not a valid utf-8 string");
    let enc_file_c = {
        assert!(!enc_file.is_null());
        unsafe { CStr::from_ptr(enc_file) }
    };
    let enc_file_rs = enc_file_c.to_str().expect("not a valid utf-8 string");
    let enc_ctx = sm2::Encrypt::new(public_key_rs);
    enc_ctx.encrypt_to_file(data_c, enc_file_rs)
}

#[no_mangle]
pub extern "C" fn decrypt_from_file(
    dec_file: *const c_char, private_key: *const c_char,
    dec_len: *mut size_t
) -> *mut c_uchar {
    let dec_file_c = {
        assert!(!dec_file.is_null());
        unsafe { CStr::from_ptr(dec_file) }
    };
    let dec_file_rs = dec_file_c.to_str().expect("not a valid utf-8 string");
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let dec_ctx = sm2::Decrypt::new(private_key_rs);
    let mut dec_data = dec_ctx.decrypt_from_file(dec_file_rs);
    dec_data.shrink_to_fit();
    let ptr = dec_data.as_ptr() as *mut c_uchar;
    unsafe {
        *dec_len = dec_data.len();
    }
    std::mem::forget(dec_data);
    ptr
}

#[repr(C)]
pub struct KeyExchangeResult {
    pub k: *mut c_char,
    pub s12: *mut c_uchar,
}

#[repr(C)]
pub struct KeyExchangeData {
    pub data: *mut c_uchar,
    pub private_key_r: *mut c_char,
}

#[no_mangle]
pub extern "C" fn keyexchange_1ab(
    klen: size_t, id: *const c_uchar, id_len: usize,
    private_key: *const c_char, data_len: *mut size_t
) -> *mut KeyExchangeData {
    let id_c = {
        assert!(!id.is_null());
        unsafe { slice::from_raw_parts(id, id_len) }
    };
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let keyexchange = sm2::KeyExchange::new(id_c, private_key_rs);
    let (mut data, rsk) = keyexchange.keyexchange_1ab(klen);
    data.shrink_to_fit();
    let data_length = data.len();
    let data_c = data.as_ptr() as *mut c_uchar;
    let rsk_c = CString::new(rsk).expect("CString::new failed");
    unsafe {
        *data_len = data_length;
    }
    std::mem::forget(data);
    Box::into_raw(Box::new(
        KeyExchangeData {
            data: data_c,
            private_key_r: rsk_c.into_raw(),
        }
    ))
}

#[no_mangle]
pub extern "C" fn keyexchange_2a(
    id: *const c_uchar, id_len: usize,
    private_key: *const c_char, private_key_r: *const c_char,
    recive_bytes: *const c_uchar, recive_bytes_len: usize,
    s12_len: *mut size_t
) -> *mut KeyExchangeResult {
    let id_c = {
        assert!(!id.is_null());
        unsafe { slice::from_raw_parts(id, id_len) }
    };
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_r_c = {
        assert!(!private_key_r.is_null());
        unsafe { CStr::from_ptr(private_key_r) }
    };
    let recive_bytes_c = {
        assert!(!recive_bytes.is_null());
        unsafe { slice::from_raw_parts(recive_bytes, recive_bytes_len) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let private_key_r_rs = private_key_r_c.to_str().expect("not a valid utf-8 string");
    let key_exchange_a = sm2::KeyExchange::new(id_c, private_key_rs);
    let keyexchange = key_exchange_a.keyexchange_2a(private_key_r_rs, recive_bytes_c);
    let k = keyexchange.k;
    let mut s12 = keyexchange.s12;
    s12.shrink_to_fit();
    let s12_c = s12.as_ptr() as *mut c_uchar;
    let k_c = CString::new(k).expect("CString::new failed");
    let s12_length = s12.len();
    unsafe {
        *s12_len = s12_length;
    }
    std::mem::forget(s12);
    Box::into_raw(Box::new(
        KeyExchangeResult {
            k: k_c.into_raw(),
            s12: s12_c,
        }
    ))
}

#[no_mangle]
pub extern "C" fn keyexchange_2b(
    id: *const c_uchar, id_len: usize,
    private_key: *const c_char, private_key_r: *const c_char,
    recive_bytes: *const c_uchar, recive_bytes_len: usize,
    s12_len: *mut size_t
) -> *mut KeyExchangeResult {
    let id_c = {
        assert!(!id.is_null());
        unsafe { slice::from_raw_parts(id, id_len) }
    };
    let private_key_c = {
        assert!(!private_key.is_null());
        unsafe { CStr::from_ptr(private_key) }
    };
    let private_key_r_c = {
        assert!(!private_key_r.is_null());
        unsafe { CStr::from_ptr(private_key_r) }
    };
    let recive_bytes_c = {
        assert!(!recive_bytes.is_null());
        unsafe { slice::from_raw_parts(recive_bytes, recive_bytes_len) }
    };
    let private_key_rs = private_key_c.to_str().expect("not a valid utf-8 string");
    let private_key_r_rs = private_key_r_c.to_str().expect("not a valid utf-8 string");
    let key_exchange_b = sm2::KeyExchange::new(id_c, private_key_rs);
    let keyexchange = key_exchange_b.keyexchange_2b(private_key_r_rs, recive_bytes_c);
    let k = keyexchange.k;
    let mut s12 = keyexchange.s12;
    s12.shrink_to_fit();
    let s12_c = s12.as_ptr() as *mut c_uchar;
    let k_c = CString::new(k).expect("CString::new failed");
    let s12_length = s12.len();
    unsafe {
        *s12_len = s12_length;
    }
    std::mem::forget(s12);
    Box::into_raw(Box::new(
        KeyExchangeResult {
            k: k_c.into_raw(),
            s12: s12_c,
        }
    ))
}

#[no_mangle]
pub extern "C" fn encrypt_ecb(
    input_data: *const c_uchar, input_date_len: size_t,
    key: *const c_uchar, key_len: size_t,
    output_data_len: *mut size_t
) -> *mut c_uchar {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { slice::from_raw_parts(input_data, input_date_len) }
    };
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let sm4_ctx = sm4::CryptSM4ECB::new(key_c);
    let mut output_data = sm4_ctx.encrypt_ecb(input_data_c);
    output_data.shrink_to_fit();
    let ptr = output_data.as_ptr() as *mut c_uchar;
    unsafe {
        *output_data_len = output_data.len();
    };
    std::mem::forget(output_data);
    ptr
}

#[no_mangle]
pub extern "C" fn encrypt_ecb_base64(
    input_data: *const c_uchar, input_date_len: size_t,
    key: *const c_uchar, key_len: size_t
) -> *mut c_char {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { slice::from_raw_parts(input_data, input_date_len) }
    };
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let sm4_ctx = sm4::CryptSM4ECB::new(key_c);
    let output_data = sm4_ctx.encrypt_ecb_base64(input_data_c);
    let output_data_c = CString::new(output_data).expect("CString::new failed");
    output_data_c.into_raw()
}

#[no_mangle]
pub extern "C" fn encrypt_ecb_hex(
    input_data: *const c_uchar, input_date_len: size_t,
    key: *const c_uchar, key_len: size_t
) -> *mut c_char {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { slice::from_raw_parts(input_data, input_date_len) }
    };
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let sm4_ctx = sm4::CryptSM4ECB::new(key_c);
    let output_data = sm4_ctx.encrypt_ecb_hex(input_data_c);
    let output_data_c = CString::new(output_data).expect("CString::new failed");
    output_data_c.into_raw()
}

#[no_mangle]
pub extern "C" fn encrypt_ecb_to_file(
    input_file: *const c_char, output_file: *const c_char,
    key: *const c_uchar, key_len: size_t
) {
    let input_file_c = {
        assert!(!input_file.is_null());
        unsafe { CStr::from_ptr(input_file) }
    };
    let output_file_c = {
        assert!(!output_file.is_null());
        unsafe { CStr::from_ptr(output_file) }
    };
    let input_file_rs = input_file_c.to_str().expect("not a valid utf-8 string");
    let output_file_rs = output_file_c.to_str().expect("not a valid utf-8 string");
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let sm4_ctx = sm4::CryptSM4ECB::new(key_c);
    sm4_ctx.encrypt_to_file(input_file_rs, output_file_rs);
}

#[no_mangle]
pub extern "C" fn decrypt_ecb(
    input_data: *const c_uchar, input_date_len: size_t,
    key: *const c_uchar, key_len: size_t,
    output_data_len: *mut size_t
) -> *mut c_uchar {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { slice::from_raw_parts(input_data, input_date_len) }
    };
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let sm4_ctx = sm4::CryptSM4ECB::new(key_c);
    let output_data_try = catch_unwind(|| {
        sm4_ctx.decrypt_ecb(input_data_c)
    });
    if let Ok(mut output_data) = output_data_try {
        output_data.shrink_to_fit();
        if output_data.len() == 0 {
            output_data = vec![0x00];
        }
        let ptr = output_data.as_ptr() as *mut c_uchar;
        unsafe {
            *output_data_len = output_data.len();
        };
        std::mem::forget(output_data);
        ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn decrypt_ecb_base64(
    input_data: *const c_char,
    key: *const c_uchar, key_len: size_t,
    output_data_len: *mut size_t
) -> *mut c_uchar {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { CStr::from_ptr(input_data) }
    };
    let input_data_rs = input_data_c.to_str().expect("not a valid utf-8 string");
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let sm4_ctx = sm4::CryptSM4ECB::new(key_c);
    let output_data_try = catch_unwind(|| {
        sm4_ctx.decrypt_ecb_base64(input_data_rs)
    });
    if let Ok(mut output_data) = output_data_try {
        output_data.shrink_to_fit();
        if output_data.len() == 0 {
            output_data = vec![0x00];
        }
        let ptr = output_data.as_ptr() as *mut c_uchar;
        unsafe {
            *output_data_len = output_data.len();
        };
        std::mem::forget(output_data);
        ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn decrypt_ecb_hex(
    input_data: *const c_char,
    key: *const c_uchar, key_len: size_t,
    output_data_len: *mut size_t
) -> *mut c_uchar {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { CStr::from_ptr(input_data) }
    };
    let input_data_rs = input_data_c.to_str().expect("not a valid utf-8 string");
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let sm4_ctx = sm4::CryptSM4ECB::new(key_c);
    let output_data_try = catch_unwind(|| {
        sm4_ctx.decrypt_ecb_hex(input_data_rs)
    });
    if let Ok(mut output_data) = output_data_try {
        output_data.shrink_to_fit();
        if output_data.len() == 0 {
            output_data = vec![0x00];
        }
        let ptr = output_data.as_ptr() as *mut c_uchar;
        unsafe {
            *output_data_len = output_data.len();
        };
        std::mem::forget(output_data);
        ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn decrypt_ecb_from_file(
    input_file: *const c_char, output_file: *const c_char,
    key: *const c_uchar, key_len: size_t
) {
    let input_file_c = {
        assert!(!input_file.is_null());
        unsafe { CStr::from_ptr(input_file) }
    };
    let input_file_rs = input_file_c.to_str().expect("not a valid utf-8 string");
    let output_file_c = {
        assert!(!output_file.is_null());
        unsafe { CStr::from_ptr(output_file) }
    };
    let output_file_rs = output_file_c.to_str().expect("not a valid utf-8 string");
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let sm4_ctx = sm4::CryptSM4ECB::new(key_c);
    sm4_ctx.decrypt_from_file(input_file_rs, output_file_rs);
}

#[no_mangle]
pub extern "C" fn encrypt_cbc(
    input_data: *const c_uchar, input_data_len: size_t,
    key: *const c_uchar, key_len: size_t,
    iv: *const c_uchar, iv_len: size_t,
    output_data_len: *mut size_t
) -> *mut c_uchar {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { slice::from_raw_parts(input_data, input_data_len) }
    };
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let iv_c = {
        assert!(!iv.is_null());
        unsafe { slice::from_raw_parts(iv, iv_len) }
    };
    let sm4_ctx = sm4::CryptSM4CBC::new(key_c, iv_c);
    let output_data_try = catch_unwind(|| {
        sm4_ctx.encrypt_cbc(input_data_c)
    });
    if let Ok(mut output_data) = output_data_try {
        output_data.shrink_to_fit();
        if output_data.len() == 0 {
            output_data = vec![0x00];
        }
        let ptr = output_data.as_ptr() as *mut c_uchar;
        unsafe {
            *output_data_len = output_data.len();
        };
        std::mem::forget(output_data);
        ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn encrypt_cbc_base64(
    input_data: *const c_uchar, input_data_len: size_t,
    key: *const c_uchar, key_len: size_t,
    iv: *const c_uchar, iv_len: size_t
) -> *mut c_char {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { slice::from_raw_parts(input_data, input_data_len) }
    };
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let iv_c = {
        assert!(!iv.is_null());
        unsafe { slice::from_raw_parts(iv, iv_len) }
    };
    let sm4_ctx = sm4::CryptSM4CBC::new(key_c, iv_c);
    let output_data = sm4_ctx.encrypt_cbc_base64(input_data_c);
    let output_data_c = CString::new(output_data).expect("CString::new failed");
    output_data_c.into_raw()
}

#[no_mangle]
pub extern "C" fn encrypt_cbc_hex(
    input_data: *const c_uchar, input_data_len: size_t,
    key: *const c_uchar, key_len: size_t,
    iv: *const c_uchar, iv_len: size_t
) -> *mut c_char {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { slice::from_raw_parts(input_data, input_data_len) }
    };
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let iv_c = {
        assert!(!iv.is_null());
        unsafe { slice::from_raw_parts(iv, iv_len) }
    };
    let sm4_ctx = sm4::CryptSM4CBC::new(key_c, iv_c);
    let output_data = sm4_ctx.encrypt_cbc_hex(input_data_c);
    let output_data_c = CString::new(output_data).expect("CString::new failed");
    output_data_c.into_raw()
}

#[no_mangle]
pub extern "C" fn encrypt_cbc_to_file(
    input_file: *const c_char, output_file: *const c_char,
    key: *const c_uchar, key_len: size_t,
    iv: *const c_uchar, iv_len: size_t
) {
    let input_file_c = {
        assert!(!input_file.is_null());
        unsafe { CStr::from_ptr(input_file) }
    };
    let output_file_c = {
        assert!(!output_file.is_null());
        unsafe { CStr::from_ptr(output_file) }
    };
    let input_file_rs = input_file_c.to_str().expect("not a valid utf-8 string");
    let output_file_rs = output_file_c.to_str().expect("not a valid utf-8 string");
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let iv_c = {
        assert!(!iv.is_null());
        unsafe { slice::from_raw_parts(iv, iv_len) }
    };
    let sm4_ctx = sm4::CryptSM4CBC::new(key_c, iv_c);
    sm4_ctx.encrypt_to_file(input_file_rs, output_file_rs);
}

#[no_mangle]
pub extern "C" fn decrypt_cbc(
    input_data: *const c_uchar, input_data_len: size_t,
    key: *const c_uchar, key_len: size_t,
    iv: *const c_uchar, iv_len: size_t,
    output_data_len: *mut size_t
) -> *mut c_uchar {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { slice::from_raw_parts(input_data, input_data_len) }
    };
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let iv_c = {
        assert!(!iv.is_null());
        unsafe { slice::from_raw_parts(iv, iv_len) }
    };
    let sm4_ctx = sm4::CryptSM4CBC::new(key_c, iv_c);
    let mut output_data = sm4_ctx.decrypt_cbc(input_data_c);
    output_data.shrink_to_fit();
    let ptr = output_data.as_ptr() as *mut c_uchar;
    unsafe {
        *output_data_len = output_data.len();
    };
    std::mem::forget(output_data);
    ptr
}

#[no_mangle]
pub extern "C" fn decrypt_cbc_base64(
    input_data: *const c_char,
    key: *const c_uchar, key_len: size_t,
    iv: *const c_uchar, iv_len: size_t,
    output_data_len: *mut size_t
) -> *mut c_uchar {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { CStr::from_ptr(input_data) }
    };
    let input_data_rs = input_data_c.to_str().expect("not a valid utf-8 string");
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let iv_c = {
        assert!(!iv.is_null());
        unsafe { slice::from_raw_parts(iv, iv_len) }
    };
    let sm4_ctx = sm4::CryptSM4CBC::new(key_c, iv_c);
    let output_data_try = catch_unwind(|| {
        sm4_ctx.decrypt_cbc_base64(input_data_rs)
    });
    if let Ok(mut output_data) = output_data_try {
        output_data.shrink_to_fit();
        if output_data.len() == 0 {
            output_data = vec![0x00];
        }
        let ptr = output_data.as_ptr() as *mut c_uchar;
        unsafe {
            *output_data_len = output_data.len();
        };
        std::mem::forget(output_data);
        ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn decrypt_cbc_hex(
    input_data: *const c_char,
    key: *const c_uchar, key_len: size_t,
    iv: *const c_uchar, iv_len: size_t,
    output_data_len: *mut size_t
) -> *mut c_uchar {
    let input_data_c = {
        assert!(!input_data.is_null());
        unsafe { CStr::from_ptr(input_data) }
    };
    let input_data_rs = input_data_c.to_str().expect("not a valid utf-8 string");
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let iv_c = {
        assert!(!iv.is_null());
        unsafe { slice::from_raw_parts(iv, iv_len) }
    };
    let sm4_ctx = sm4::CryptSM4CBC::new(key_c, iv_c);
    let output_data_try = catch_unwind(|| {
        sm4_ctx.decrypt_cbc_hex(input_data_rs)
    });
    if let Ok(mut output_data) = output_data_try {
        output_data.shrink_to_fit();
        if output_data.len() == 0 {
            output_data = vec![0x00];
        }
        let ptr = output_data.as_ptr() as *mut c_uchar;
        unsafe {
            *output_data_len = output_data.len();
        };
        std::mem::forget(output_data);
        ptr
    } else {
        std::ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn decrypt_cbc_from_file(
    input_file: *const c_char, output_file: *const c_char,
    key: *const c_uchar, key_len: size_t,
    iv: *const c_uchar, iv_len: size_t
) {
    let input_file_c = {
        assert!(!input_file.is_null());
        unsafe { CStr::from_ptr(input_file) }
    };
    let input_file_rs = input_file_c.to_str().expect("not a valid utf-8 string");
    let output_file_c = {
        assert!(!output_file.is_null());
        unsafe { CStr::from_ptr(output_file) }
    };
    let output_file_rs = output_file_c.to_str().expect("not a valid utf-8 string");
    let key_c = {
        assert!(!key.is_null());
        unsafe { slice::from_raw_parts(key, key_len) }
    };
    let iv_c = {
        assert!(!iv.is_null());
        unsafe { slice::from_raw_parts(iv, iv_len) }
    };
    let sm4_ctx = sm4::CryptSM4CBC::new(key_c, iv_c);
    sm4_ctx.decrypt_from_file(input_file_rs, output_file_rs);
}

// free memory
#[no_mangle]
pub extern "C" fn free_byte_array(ptr: *mut c_uchar, len: size_t) {
    unsafe {
        drop(Vec::from_raw_parts(ptr, len, len));
    }
}

#[no_mangle]
pub extern "C" fn free_char_array(ptr: *mut c_char) {
    unsafe {
        drop(CString::from_raw(ptr))
    };
}

#[no_mangle]
pub extern  "C" fn free_struct_keypair(ptr: *mut Keypair) {
    unsafe {
        if !ptr.is_null() {
            let kp = &mut *ptr;
            drop(CString::from_raw(kp.private_key));
            drop(CString::from_raw(kp.public_key));
            drop(Box::from_raw(ptr));
        }
    }
}

#[no_mangle]
pub extern "C" fn free_struct_keyexchangedata(ptr: *mut KeyExchangeData) {
    unsafe {
        if !ptr.is_null() {
            let kp = &mut *ptr;
            libc::free(kp.data as *mut libc::c_void);
            drop(CString::from_raw(kp.private_key_r));
            drop(Box::from_raw(ptr));
        }
    }
}

#[no_mangle]
pub extern "C" fn free_struct_keyexchangeresult(ptr: *mut KeyExchangeResult) {
    unsafe {
        if !ptr.is_null() {
            let kp = &mut *ptr;
            drop(CString::from_raw(kp.k));
            libc::free(kp.s12 as *mut libc::c_void);
            drop(Box::from_raw(ptr));
        }
    }
}
