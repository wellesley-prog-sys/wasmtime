use cranelift_isle::lexer::Pos;

use crate::program::Program;
use std::{
    fs::File,
    io::{self, Write},
    path::PathBuf,
};

pub struct ExplorerWriter<'a> {
    prog: &'a Program,

    root: std::path::PathBuf,
}

impl<'a> ExplorerWriter<'a> {
    pub fn new(prog: &'a Program, root: std::path::PathBuf) -> Self {
        Self { prog, root }
    }

    pub fn write(&self) -> anyhow::Result<()> {
        self.write_index()?;
        self.write_files()?;
        self.write_terms()?;
        Ok(())
    }

    fn write_index(&self) -> anyhow::Result<()> {
        let mut output = self.create(PathBuf::from("index.html"))?;
        self.header("ISLE Explorer", &mut output)?;
        writeln!(
            output,
            r#"
        <ul>
            <li><a href="/{file_link}">Source Files</a></li>
            <li><a href="/{term_link}">Terms</a></li>
        </ul>
        "#,
            file_link = self.file_dir().display(),
            term_link = self.term_dir().display(),
        )?;
        self.footer(&mut output)?;
        Ok(())
    }

    fn write_files(&self) -> anyhow::Result<()> {
        self.write_files_index()?;
        for id in 0..self.prog.tyenv.filenames.len() {
            self.write_file(id)?;
        }
        Ok(())
    }

    fn write_files_index(&self) -> anyhow::Result<()> {
        let mut output = self.create(self.file_dir().join("index.html"))?;
        self.header("Source Files", &mut output)?;

        // Files.
        writeln!(output, "<ul>")?;
        for (id, filename) in self.prog.tyenv.filenames.iter().enumerate() {
            writeln!(
                output,
                r#"<li><a href="/{link}">{filename}</a></li>"#,
                link = self.file_path(id).display()
            )?;
        }
        writeln!(output, "</ul>")?;

        self.footer(&mut output)?;
        Ok(())
    }

    fn write_file(&self, id: usize) -> anyhow::Result<()> {
        let mut output = self.create(self.file_path(id))?;

        // Header.
        let filename = &self.prog.tyenv.filenames[id];
        let title = format!("File: {filename}");
        self.header(&title, &mut output)?;

        // Source code.
        let file_text = &self.prog.tyenv.file_texts[id];

        writeln!(&mut output, "<pre>")?;
        for (i, line) in file_text.lines().enumerate() {
            let n = i + 1;
            writeln!(
                &mut output,
                "{n:4}:\t<span id=\"{fragment}\">{line}</span>",
                fragment = self.line_url_fragment(n)
            )?;
        }
        writeln!(&mut output, "</pre>")?;

        // Footer.
        self.footer(&mut output)?;

        Ok(())
    }

    fn write_terms(&self) -> anyhow::Result<()> {
        let mut output = self.create(self.term_dir().join("index.html"))?;
        self.header("Terms", &mut output)?;

        // Files.
        writeln!(output, "<ul>")?;
        for term in &self.prog.termenv.terms {
            writeln!(
                output,
                r#"<li>{name} {pos}</li>"#,
                name = self.prog.term_name(term.id),
                pos = self.pos(term.decl_pos)
            )?;
        }
        writeln!(output, "</ul>")?;

        self.footer(&mut output)?;
        Ok(())
    }

    fn header(&self, title: &str, dst: &mut dyn Write) -> io::Result<()> {
        write!(
            dst,
            r#"
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <title>{title}</title>
    <link rel="stylesheet" href="https://unpkg.com/terminal.css@0.7.4/dist/terminal.min.css" />
  </head>
  <body>
    <main>
      <h1>{title}</h1>
        "#
        )
    }

    fn footer(&self, dst: &mut dyn Write) -> io::Result<()> {
        write!(
            dst,
            r#"
    </main>
  </body>
</html>
        "#
        )
    }

    fn pos(&self, pos: Pos) -> String {
        format!(
            r#"<a href="{href}">{loc}</a>"#,
            href = self.pos_href(pos),
            loc = self.loc(pos)
        )
    }

    fn loc(&self, pos: Pos) -> String {
        format!("{}:{}", self.prog.tyenv.filenames[pos.file], pos.line)
    }

    fn pos_href(&self, pos: Pos) -> String {
        format!(
            "/{}#{}",
            self.file_path(pos.file).display(),
            self.line_url_fragment(pos.line)
        )
    }

    fn line_url_fragment(&self, n: usize) -> String {
        format!("L{n}")
    }

    fn term_dir(&self) -> PathBuf {
        PathBuf::from("term")
    }

    fn file_dir(&self) -> PathBuf {
        PathBuf::from("file")
    }

    fn file_path(&self, id: usize) -> PathBuf {
        self.file_dir().join(format!("{id}.html"))
    }

    fn create(&self, path: std::path::PathBuf) -> io::Result<File> {
        assert!(path.is_relative());
        log::info!("create: {}", path.display());
        let path = self.root.join(path);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        File::create(path)
    }
}
