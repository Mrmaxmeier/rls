// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(concat_idents)]

#[macro_use]
extern crate derive_new;
extern crate env_logger;
extern crate languageserver_types as ls_types;
#[macro_use]
extern crate log;
extern crate rls_vfs as vfs;
extern crate rls_span as span;
extern crate rls_data as data;
extern crate serde;
#[macro_use]
extern crate serde_derive;

#[cfg(test)]
#[macro_use]
extern crate serde_json;
#[cfg(not(test))]
extern crate serde_json;

extern crate toml;
extern crate url;
extern crate url_serde;

extern crate aho_corasick;
extern crate tectonic;
extern crate parking_lot;

use std::sync::Arc;
use std::io::Write;

mod actions;
mod analysis;
// mod build; // see analysis::build_queue
// mod cmd;
// mod config;
mod lsp_data;
mod server;


type Span = span::Span<span::ZeroIndexed>;
pub fn main() {
    eprintln!("out of cheese error");
    env_logger::init().unwrap();

    let vfs = Arc::new(vfs::Vfs::new());

    let (build_queue, analysis_host) = analysis::BuildQueue::connected(vfs.clone());

    server::run_server(vfs, Arc::new(build_queue));
}
