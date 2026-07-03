// SPDX-License-Identifier: MIT
//! Streaming SHA-256: the authoritative byte-identity check (mirrors the Python
//! `sha256_file`). [`HashingWriter`] lets a download be hashed *as it streams* to
//! disk (incremental SHA-256), so a large file is never buffered whole in memory.

use std::io::{self, Read, Write};

use sha2::{Digest, Sha256};

const CHUNK: usize = 1 << 20; // 1 MiB, matching the Python reader.

/// Hex SHA-256 of everything read from `reader`.
pub fn sha256_hex_reader<R: Read>(mut reader: R) -> io::Result<String> {
    let mut hasher = Sha256::new();
    let mut buf = vec![0u8; CHUNK];
    loop {
        let n = reader.read(&mut buf)?;
        if n == 0 {
            break;
        }
        hasher.update(&buf[..n]);
    }
    Ok(hex::encode(hasher.finalize()))
}

/// Hex SHA-256 of a file's bytes.
pub fn sha256_hex_file(path: &std::path::Path) -> io::Result<String> {
    sha256_hex_reader(std::fs::File::open(path)?)
}

/// Hex SHA-256 of an in-memory blob.
pub fn sha256_hex_bytes(data: &[u8]) -> String {
    hex::encode(Sha256::digest(data))
}

/// A `Write` adapter that forwards bytes to an inner writer while updating a
/// SHA-256 — used to stream a download to a temp file and hash it in one pass.
pub struct HashingWriter<W: Write> {
    inner: W,
    hasher: Sha256,
    written: u64,
}

impl<W: Write> HashingWriter<W> {
    /// Wrap an inner writer.
    pub fn new(inner: W) -> Self {
        HashingWriter {
            inner,
            hasher: Sha256::new(),
            written: 0,
        }
    }

    /// Consume the writer, returning the inner writer, the hex digest, and the
    /// number of bytes written.
    pub fn finish(self) -> (W, String, u64) {
        (
            self.inner,
            hex::encode(self.hasher.finalize()),
            self.written,
        )
    }
}

impl<W: Write> Write for HashingWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let n = self.inner.write(buf)?;
        self.hasher.update(&buf[..n]);
        self.written += n as u64;
        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_vector() {
        // SHA-256("abc")
        assert_eq!(
            sha256_hex_bytes(b"abc"),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );
    }

    #[test]
    fn reader_matches_bytes() {
        let data = b"the quick brown fox";
        assert_eq!(
            sha256_hex_reader(&data[..]).unwrap(),
            sha256_hex_bytes(data)
        );
    }

    #[test]
    fn hashing_writer_streams_and_hashes() {
        let mut sink: Vec<u8> = Vec::new();
        let mut hw = HashingWriter::new(&mut sink);
        hw.write_all(b"the quick ").unwrap();
        hw.write_all(b"brown fox").unwrap();
        let (_, digest, n) = hw.finish();
        assert_eq!(n, 19);
        assert_eq!(digest, sha256_hex_bytes(b"the quick brown fox"));
        assert_eq!(sink, b"the quick brown fox");
    }
}
