use std::mem;
use std::fmt::Arguments;
use std::collections::HashMap;
use std::path::PathBuf;

use tectonic::status::*;
use tectonic::errors::Error;

use ls_types::*;

#[derive(Debug)]
pub enum StatusMessage {
    WithContext(MessageKind, String, MessageLocation),
    NoContext(MessageKind, String)
}

impl StatusMessage {
    fn range(&self) -> Option<Range> {
        if let StatusMessage::WithContext(_, _, ref loc) = *self {
            Some(Range {
                start: Position { line: loc.lines.0, character: loc.chars.0 },
                end: Position { line: loc.lines.1, character: loc.chars.1 },
            })
        } else {
            None
        }
    }

    fn severity(&self) -> DiagnosticSeverity {
        let kind = match *self {
            StatusMessage::NoContext(kind, _) |
            StatusMessage::WithContext(kind, _, _) => kind,
        };
        match kind {
            MessageKind::Note => DiagnosticSeverity::Information,
            MessageKind::Warning => DiagnosticSeverity::Warning,
            MessageKind::Error => DiagnosticSeverity::Error,
        }
    }

    pub fn as_diagnostic(&self) -> Option<Diagnostic> {
        if let StatusMessage::WithContext(_, ref message, _) = *self {
            Some(Diagnostic {
                range: self.range().unwrap(),
                severity: Some(self.severity()),
                code: None,
                source: None, // TODO: 'TeX', 'BibTeX', 'xdvipfdmx'
                message: message.clone(),
            })
        } else {
            None
        }
    }

    pub fn file(&self) -> Option<String> {
        if let StatusMessage::WithContext(_, _, ref loc) = *self {
            Some(loc.file.clone())
        } else {
            None
        }
    }
}

#[derive(Debug, Default)]
pub struct LanguageServerStatusBackend {
    data: Vec<StatusMessage>,
}

impl LanguageServerStatusBackend {
    pub fn diagnostics(&mut self) -> HashMap<PathBuf, Vec<Diagnostic>> {
        self.data.iter()
            .filter_map(|msg| msg.file())
            .map(|file| {
                let diagnostics = self.data
                    .iter()
                    .filter(|msg| msg.file().as_ref() == Some(&file))
                    .filter_map(|msg| msg.as_diagnostic())
                    .collect::<Vec<_>>();
                (PathBuf::from(file), diagnostics)
            })
            .collect()
    }

    pub fn debug_log(&mut self) -> Vec<String> {
        self.data
            .iter()
            .filter_map(|msg| if let StatusMessage::NoContext(_, ref msg) = *msg {
                Some(msg.clone())
            } else {
                None
            })
            .collect()
    }

    pub fn clear(&mut self) -> Vec<StatusMessage> {
        mem::replace(&mut self.data, Vec::new())
    }
}

impl StatusBackend for LanguageServerStatusBackend {
    fn report(&mut self, kind: MessageKind, args: Arguments, _err: Option<&Error>) {
        eprintln!("{:?} {}", kind, args);
        debug_assert!(_err.is_none());
        self.data.push(StatusMessage::NoContext(kind, format!("{}", args)));
    }

    fn report_at(&mut self, location: MessageLocation, kind: MessageKind, args: Arguments) {
        eprintln!("{:?} [{}] {}", kind, location, args);
        self.data.push(StatusMessage::WithContext(
            kind,
            format!("{}", args),
            location
        ));
    }
}