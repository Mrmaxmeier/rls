use std::mem;
use std::fmt::Arguments;
use std::collections::HashMap;

use tectonic::status::*;
use tectonic::errors::Error;

use ls_types::*;

#[derive(Debug)]
pub struct StatusMessage {
    kind: MessageKind,
    message: String,
    location: Option<MessageLocation>
}

impl StatusMessage {
    fn range(&self) -> Option<Range> {
        self.location.map(|loc|
            Range {
                start: Position { line: loc.lines.0, character: loc.chars.0 },
                end: Position { line: loc.lines.1, character: loc.chars.1 },
            }
        )
    }

    fn severity(&self) -> DiagnosticSeverity {
        match self.kind {
            MessageKind::Note => DiagnosticSeverity::Information,
            MessageKind::Warning => DiagnosticSeverity::Warning,
            MessageKind::Error => DiagnosticSeverity::Error,
        }
    }

    pub fn as_diagnostic(&self) -> Diagnostic {
        Diagnostic {
            range: self.range().unwrap_or(Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: 0, character: 0}
            }),
            severity: Some(self.severity()),
            code: None,
            source: None, // TODO: 'TeX', 'BibTeX', 'xdvipfdmx'
            message: self.message.clone(),
        }
    }
}

#[derive(Debug, Default)]
pub struct LanguageServerStatusBackend {
    data: Vec<StatusMessage>,
}

impl LanguageServerStatusBackend {
    pub fn diagnostics(&mut self) -> HashMap<String, Diagnostic> {
        let data = mem::replace(&mut self.data, Vec::new());
        data.iter()
            .map(|msg| {
                let default_file = "main.tex".to_owned();
                let file = msg.location.map(|l| l.file.clone()).unwrap_or(default_file);
                (file, msg.as_diagnostic())
            })
            .collect()
    }
}

impl StatusBackend for LanguageServerStatusBackend {
    fn report(&mut self, kind: MessageKind, args: Arguments, err: Option<&Error>) {
        self.data.push(StatusMessage {
            location: None,
            kind: kind,
            message: format!("{}", args)
        });
    }

    fn report_at(&mut self, location: MessageLocation, kind: MessageKind, args: Arguments) {
        self.data.push(StatusMessage {
            location: Some(location),
            kind: kind,
            message: format!("{}", args)
        });
    }
}