use extendr_api::prelude::*;
use uuid::{Uuid, Variant};

fn random_node_id() -> [u8; 6] {
    // RFC 9562 allows a randomly generated node ID for privacy-sensitive
    // version 1 and 6 UUIDs. Setting the multicast bit signals that the node
    // value is not a real IEEE 802 MAC address.
    let seed = Uuid::new_v4().into_bytes();
    let mut node = [0_u8; 6];
    node.copy_from_slice(&seed[..6]);
    node[0] |= 0x01;
    node
}

fn uuid_to_raw(uuid: &Uuid) -> Robj {
    Robj::from(Raw::from_bytes(uuid.as_bytes()))
}

fn uuid_strings(uuids: &[Uuid]) -> Robj {
    Strings::from_values(
        uuids
            .iter()
            .map(|uuid| uuid.to_string())
            .collect::<Vec<String>>(),
    )
    .into()
}

fn uuid_raw_list(uuids: &[Uuid]) -> Robj {
    List::from_values(
        uuids
            .iter()
            .map(uuid_to_raw)
            .collect::<Vec<Robj>>(),
    )
    .into()
}

fn uuid_output(uuids: &[Uuid], as_raw: bool) -> Robj {
    if as_raw {
        uuid_raw_list(uuids)
    } else {
        uuid_strings(uuids)
    }
}

fn parse_uuid(value: &str, arg_name: &str) -> Uuid {
    Uuid::try_parse(value).unwrap_or_else(|_| {
        panic!("invalid UUID supplied in `{arg_name}`: {value}");
    })
}

fn variant_label(variant: Variant) -> &'static str {
    match variant {
        Variant::NCS => "ncs",
        Variant::RFC4122 => "rfc4122",
        Variant::Microsoft => "microsoft",
        Variant::Future => "future",
        _ => "unknown",
    }
}

fn build_generated(n: i32, generator: fn() -> Uuid, as_raw: bool) -> Robj {
    let size = if n <= 0 { 0 } else { n as usize };
    let uuids = (0..size).map(|_| generator()).collect::<Vec<Uuid>>();
    uuid_output(&uuids, as_raw)
}

/// Generate version 7 UUIDs using the current time.
/// @noRd
#[extendr]
fn uuid_rust_v7(n: i32, as_raw: bool) -> Robj {
    build_generated(n, Uuid::now_v7, as_raw)
}

/// Generate version 4 UUIDs using the operating system RNG.
/// @noRd
#[extendr]
fn uuid_rust_v4(n: i32, as_raw: bool) -> Robj {
    build_generated(n, Uuid::new_v4, as_raw)
}

/// Generate version 6 UUIDs using a random node identifier.
/// @noRd
#[extendr]
fn uuid_rust_v6(n: i32, as_raw: bool) -> Robj {
    let size = if n <= 0 { 0 } else { n as usize };
    let uuids = (0..size)
        .map(|_| {
            let node_id = random_node_id();
            Uuid::now_v6(&node_id)
        })
        .collect::<Vec<Uuid>>();
    uuid_output(&uuids, as_raw)
}

/// Generate legacy version 1 UUIDs using a random node identifier.
/// @noRd
#[extendr]
fn uuid_rust_v1(n: i32, as_raw: bool) -> Robj {
    let size = if n <= 0 { 0 } else { n as usize };
    let uuids = (0..size)
        .map(|_| {
            let node_id = random_node_id();
            Uuid::now_v1(&node_id)
        })
        .collect::<Vec<Uuid>>();
    uuid_output(&uuids, as_raw)
}

/// Generate deterministic version 5 UUIDs from namespace/name pairs.
/// @noRd
#[extendr]
fn uuid_rust_v5(namespace: Vec<String>, name: Vec<String>, as_raw: bool) -> Robj {
    assert_eq!(
        namespace.len(),
        name.len(),
        "`namespace` and `name` must be the same length after recycling."
    );

    let uuids = namespace
        .iter()
        .zip(name.iter())
        .map(|(ns, nm)| {
            let ns_uuid = parse_uuid(ns, "namespace");
            Uuid::new_v5(&ns_uuid, nm.as_bytes())
        })
        .collect::<Vec<Uuid>>();

    uuid_output(&uuids, as_raw)
}

/// Validate UUID strings.
/// @noRd
#[extendr]
fn uuid_rust_validate(x: Vec<String>) -> Vec<bool> {
    x.iter().map(|value| Uuid::try_parse(value).is_ok()).collect()
}

/// Detect UUID version numbers, returning `NA` for invalid inputs.
/// @noRd
#[extendr]
fn uuid_rust_version(x: Vec<String>) -> Vec<Rint> {
    x.iter()
        .map(|value| match Uuid::try_parse(value) {
            Ok(uuid) => Rint::from(uuid.get_version_num() as i32),
            Err(_) => Rint::na(),
        })
        .collect()
}

/// Parse UUID strings and return canonical strings.
/// @noRd
#[extendr]
fn uuid_rust_parse_string(x: Vec<String>) -> Vec<String> {
    x.iter()
        .map(|value| parse_uuid(value, "x").to_string())
        .collect()
}

/// Parse UUID strings and return a list of raw 16-byte values.
/// @noRd
#[extendr]
fn uuid_rust_parse_raw(x: Vec<String>) -> Robj {
    let uuids = x
        .iter()
        .map(|value| parse_uuid(value, "x"))
        .collect::<Vec<Uuid>>();
    uuid_raw_list(&uuids)
}

/// Parse UUID strings and expose RFC 9562 field slices.
/// @noRd
#[extendr]
fn uuid_rust_parse_fields(x: Vec<String>) -> List {
    let mut canonical = Vec::<String>::with_capacity(x.len());
    let mut version = Vec::<Rint>::with_capacity(x.len());
    let mut variant = Vec::<String>::with_capacity(x.len());
    let mut time_low = Vec::<String>::with_capacity(x.len());
    let mut time_mid = Vec::<String>::with_capacity(x.len());
    let mut time_hi_and_version = Vec::<String>::with_capacity(x.len());
    let mut clock_seq_hi_and_reserved = Vec::<String>::with_capacity(x.len());
    let mut clock_seq_low = Vec::<String>::with_capacity(x.len());
    let mut node = Vec::<String>::with_capacity(x.len());

    for value in x.iter() {
        let uuid = parse_uuid(value, "x");
        let (d1, d2, d3, d4) = uuid.as_fields();

        canonical.push(uuid.to_string());
        version.push(Rint::from(uuid.get_version_num() as i32));
        variant.push(variant_label(uuid.get_variant()).to_string());
        time_low.push(format!("{d1:08x}"));
        time_mid.push(format!("{d2:04x}"));
        time_hi_and_version.push(format!("{d3:04x}"));
        clock_seq_hi_and_reserved.push(format!("{:02x}", d4[0]));
        clock_seq_low.push(format!("{:02x}", d4[1]));
        node.push(
            d4[2..]
                .iter()
                .map(|byte| format!("{byte:02x}"))
                .collect::<String>(),
        );
    }

    list!(
        canonical = canonical,
        version = version,
        variant = variant,
        time_low = time_low,
        time_mid = time_mid,
        time_hi_and_version = time_hi_and_version,
        clock_seq_hi_and_reserved = clock_seq_hi_and_reserved,
        clock_seq_low = clock_seq_low,
        node = node
    )
}

/// Return the nil UUID.
/// @noRd
#[extendr]
fn uuid_rust_nil(as_raw: bool) -> Robj {
    let uuid = Uuid::nil();
    if as_raw {
        uuid_to_raw(&uuid)
    } else {
        Robj::from(uuid.to_string())
    }
}

/// Return the max UUID.
/// @noRd
#[extendr]
fn uuid_rust_max(as_raw: bool) -> Robj {
    let uuid = Uuid::max();
    if as_raw {
        uuid_to_raw(&uuid)
    } else {
        Robj::from(uuid.to_string())
    }
}

extendr_module! {
    mod uuidx;
    fn uuid_rust_v7;
    fn uuid_rust_v4;
    fn uuid_rust_v6;
    fn uuid_rust_v1;
    fn uuid_rust_v5;
    fn uuid_rust_validate;
    fn uuid_rust_version;
    fn uuid_rust_parse_string;
    fn uuid_rust_parse_raw;
    fn uuid_rust_parse_fields;
    fn uuid_rust_nil;
    fn uuid_rust_max;
}
