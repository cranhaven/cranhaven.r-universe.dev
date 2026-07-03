// SPDX-License-Identifier: MIT
//! The HTTP seam. All network I/O goes through [`HttpClient`] so the resolvers and
//! the fetch orchestration are unit-tested against an injected fake — exactly as the
//! Python suite injects a fake `requests.Session`. The real implementation
//! ([`UreqClient`], behind the default `net` feature) is a thin blocking ureq wrapper
//! using rustls (no system OpenSSL, so it cross-compiles and packages cleanly).

use std::io::Read;

use crate::error::Result;

/// One HTTP response: status, an optional `Location` header (for redirect handling),
/// and the body as a streaming reader (never buffered whole in memory here).
pub struct HttpResponse {
    /// HTTP status code.
    pub status: u16,
    /// The `Location` header, if present (used to follow a redirect manually).
    pub location: Option<String>,
    /// The response body as a reader.
    pub body: Box<dyn Read + Send>,
}

impl HttpResponse {
    /// Whether the status is 2xx.
    pub fn is_success(&self) -> bool {
        (200..300).contains(&self.status)
    }

    /// Whether the status is a 3xx redirect.
    pub fn is_redirect(&self) -> bool {
        (300..400).contains(&self.status)
    }

    /// Read the whole body into a string (for small JSON API responses).
    pub fn text(self) -> Result<String> {
        let mut s = String::new();
        let mut body = self.body;
        body.read_to_string(&mut s)?;
        Ok(s)
    }
}

/// A blocking HTTP GET seam.
pub trait HttpClient {
    /// Perform a GET.
    ///
    /// * `headers` — extra request headers (e.g. `X-Dataverse-key`).
    /// * `follow_redirects` — when `false`, a 3xx is returned as-is (status +
    ///   `Location`) so the caller can decide whether to replay auth on the redirect.
    /// * `range_start` — when set, request `Range: bytes=<start>-` (resume support).
    fn get(
        &self,
        url: &str,
        headers: &[(String, String)],
        follow_redirects: bool,
        range_start: Option<u64>,
    ) -> Result<HttpResponse>;
}

// ---------------------------------------------------------------------------
// Real client (ureq + rustls), behind the default `net` feature.
// ---------------------------------------------------------------------------
#[cfg(feature = "net")]
mod ureq_client {
    use std::time::Duration;

    use super::{HttpClient, HttpResponse};
    use crate::error::{Error, Result};

    /// A blocking [`HttpClient`] backed by ureq (rustls TLS).
    pub struct UreqClient {
        follow: ureq::Agent,
        no_follow: ureq::Agent,
    }

    impl UreqClient {
        /// Build a client with the given per-request read/connect timeout (seconds).
        pub fn new(timeout_secs: u64) -> Self {
            let secs = timeout_secs.max(1);
            let build = |redirects: u32| {
                ureq::AgentBuilder::new()
                    .redirects(redirects)
                    .timeout_connect(Duration::from_secs(30))
                    .timeout_read(Duration::from_secs(secs))
                    .user_agent(concat!(
                        "nirs4all-datasets-core/",
                        env!("CARGO_PKG_VERSION")
                    ))
                    .build()
            };
            UreqClient {
                follow: build(10),
                no_follow: build(0),
            }
        }
    }

    impl Default for UreqClient {
        fn default() -> Self {
            UreqClient::new(300)
        }
    }

    impl HttpClient for UreqClient {
        fn get(
            &self,
            url: &str,
            headers: &[(String, String)],
            follow_redirects: bool,
            range_start: Option<u64>,
        ) -> Result<HttpResponse> {
            let agent = if follow_redirects {
                &self.follow
            } else {
                &self.no_follow
            };
            let mut req = agent.get(url);
            for (k, v) in headers {
                req = req.set(k, v);
            }
            if let Some(start) = range_start {
                req = req.set("Range", &format!("bytes={start}-"));
            }
            // ureq surfaces a non-2xx as Err(Status); fold it back into a response so
            // the caller (not ureq) decides what a 3xx/4xx means.
            let resp = match req.call() {
                Ok(r) => r,
                Err(ureq::Error::Status(_, r)) => r,
                Err(ureq::Error::Transport(t)) => return Err(Error::Http(t.to_string())),
            };
            let status = resp.status();
            let location = resp.header("Location").map(str::to_string);
            Ok(HttpResponse {
                status,
                location,
                body: Box::new(resp.into_reader()),
            })
        }
    }
}

#[cfg(feature = "net")]
pub use ureq_client::UreqClient;

// ---------------------------------------------------------------------------
// Test double (no network), shared by the resolver + fetch tests.
// ---------------------------------------------------------------------------
#[cfg(test)]
pub(crate) mod testing {
    use std::io::Cursor;
    use std::sync::Mutex;

    use super::{HttpClient, HttpResponse};
    use crate::error::Result;

    /// A recorded request.
    #[derive(Clone, Debug)]
    pub struct MockCall {
        pub url: String,
        pub headers: Vec<(String, String)>,
        pub follow_redirects: bool,
    }

    impl MockCall {
        /// The value of a request header, if set.
        pub fn header(&self, name: &str) -> Option<&str> {
            self.headers
                .iter()
                .find(|(k, _)| k == name)
                .map(|(_, v)| v.as_str())
        }
    }

    /// A canned response (status + optional Location + body bytes).
    pub struct MockResponse {
        pub status: u16,
        pub location: Option<String>,
        pub body: Vec<u8>,
    }

    impl MockResponse {
        pub fn ok(body: impl Into<Vec<u8>>) -> MockResponse {
            MockResponse {
                status: 200,
                location: None,
                body: body.into(),
            }
        }
        pub fn redirect(location: &str) -> MockResponse {
            MockResponse {
                status: 303,
                location: Some(location.to_string()),
                body: Vec::new(),
            }
        }
        pub fn status(code: u16) -> MockResponse {
            MockResponse {
                status: code,
                location: None,
                body: Vec::new(),
            }
        }
    }

    /// A fake [`HttpClient`]: a handler closure maps each call to a response, and
    /// every call is recorded for assertions.
    pub struct MockHttp {
        handler: Box<dyn Fn(&MockCall) -> MockResponse + Send + Sync>,
        pub calls: Mutex<Vec<MockCall>>,
    }

    impl MockHttp {
        pub fn new(handler: impl Fn(&MockCall) -> MockResponse + Send + Sync + 'static) -> Self {
            MockHttp {
                handler: Box::new(handler),
                calls: Mutex::new(Vec::new()),
            }
        }

        pub fn call_count(&self) -> usize {
            self.calls.lock().unwrap().len()
        }
    }

    impl HttpClient for MockHttp {
        fn get(
            &self,
            url: &str,
            headers: &[(String, String)],
            follow_redirects: bool,
            range_start: Option<u64>,
        ) -> Result<HttpResponse> {
            let _ = range_start;
            let call = MockCall {
                url: url.to_string(),
                headers: headers.to_vec(),
                follow_redirects,
            };
            self.calls.lock().unwrap().push(call.clone());
            let r = (self.handler)(&call);
            Ok(HttpResponse {
                status: r.status,
                location: r.location,
                body: Box::new(Cursor::new(r.body)),
            })
        }
    }
}
