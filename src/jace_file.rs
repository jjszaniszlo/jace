
use miette::{MietteSpanContents, SourceCode};

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct JaceFile<'a> {
    name: &'a str,
    contents: &'a str,
}

impl<'a> JaceFile<'a> {
    pub fn new(name: &'a str, contents: &'a str) -> Self {
        Self {
            name,
            contents
        }
    }

    pub fn name(&self) -> &'a str {
        self.name
    }

    pub fn contents(&self) -> &'a str {
        self.contents
    } 
}

impl<'b> SourceCode for JaceFile<'b> {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        let inner_contents =
            self.contents()
                .read_span(span, context_lines_before, context_lines_after)?;

        let contents = MietteSpanContents::new_named(
            self.name().to_string(),
            inner_contents.data(), 
            *inner_contents.span(),
            inner_contents.line(),
            inner_contents.column(),
            inner_contents.line_count());

        Ok(Box::new(contents))
    }
}

pub struct JaceFileMap<'a> {
    files: Vec<JaceFile<'a>>,
}

impl<'a> JaceFileMap<'a> {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
        }
    }

    pub fn add(&mut self, jace_file: JaceFile<'a>) -> usize {
        let id = self.files.len();
        self.files.push(jace_file);
        id
    }

    pub fn get(&self, id: usize) -> Option<&JaceFile> {
        self.files.get(id)
    }
}
