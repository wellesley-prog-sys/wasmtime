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
    dev: bool,
}

impl<'a> ExplorerWriter<'a> {
    pub fn new(prog: &'a Program, root: std::path::PathBuf) -> Self {
        Self {
            prog,
            root,
            // TODO(mbm): configurable dev mode
            dev: true,
        }
    }

    pub fn write(&self) -> anyhow::Result<()> {
        self.init()?;
        self.write_assets()?;
        self.write_index()?;
        self.write_files()?;
        self.write_terms()?;
        self.write_rules()?;
        Ok(())
    }

    fn init(&self) -> anyhow::Result<()> {
        std::fs::create_dir_all(&self.root)?;
        Ok(())
    }

    fn write_assets(&self) -> anyhow::Result<()> {
        // In development mode, setup a symlink.
        if self.dev {
            let crate_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            // TODO(mbm): platform-independent symlink
            let original = crate_root.join("src/assets");
            let link = self.abs(self.assets_dir());
            std::os::unix::fs::symlink(original, link)?;
            return Ok(());
        }

        // CSS.
        let style_css = include_bytes!("./assets/style.css");
        let mut output = self.create(self.style_path())?;
        output.write(style_css)?;

        Ok(())
    }

    fn write_index(&self) -> anyhow::Result<()> {
        let mut output = self.create(PathBuf::from("index.html"))?;
        self.header("ISLE Explorer", &mut output)?;
        writeln!(
            output,
            r#"
        <ul>
            <li><a href="/{files_href}">Files</a></li>
            <li><a href="/{terms_href}">Terms</a></li>
            <li><a href="/{rules_href}">Rules</a></li>
        </ul>
        "#,
            files_href = self.file_dir().display(),
            terms_href = self.term_dir().display(),
            rules_href = self.rule_dir().display(),
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
        self.header("Files", &mut output)?;

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
                r#"<code id=\"{fragment}\">{line}</code>"#,
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

        // Terms.
        writeln!(
            output,
            r#"
        <table>
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Location</th>
                </tr>
            </thead>
            <tbody>
        "#
        )?;
        for term in &self.prog.termenv.terms {
            writeln!(
                output,
                r#"<tr><td>{name}</td><td>{pos}</td></tr>"#,
                name = self.prog.term_name(term.id),
                pos = self.pos(term.decl_pos)
            )?;
        }
        writeln!(
            output,
            r#"
            </tbody>
        </table>
        "#
        )?;

        self.footer(&mut output)?;
        Ok(())
    }

    fn write_rules(&self) -> anyhow::Result<()> {
        let mut output = self.create(self.rule_dir().join("index.html"))?;
        self.header("Rules", &mut output)?;

        // Rules.
        writeln!(output, "<ul>")?;
        for rule in &self.prog.termenv.rules {
            writeln!(
                output,
                r#"<li><a href="{href}">{identifier}</a></li>"#,
                href = self.pos_href(rule.pos),
                identifier = rule.identifier(&self.prog.tyenv)
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
    <link rel="stylesheet" href="/{style_path}" />
  </head>
  <body>
    <main>
      <h1>{title}</h1>
        "#,
            style_path = self.style_path().display()
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
        let path = PathBuf::from(self.prog.tyenv.filenames[pos.file].as_ref());
        format!(
            "{}:{}",
            path.file_name().unwrap().to_string_lossy(),
            pos.line
        )
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

    fn rule_dir(&self) -> PathBuf {
        PathBuf::from("rule")
    }

    fn file_dir(&self) -> PathBuf {
        PathBuf::from("file")
    }

    fn file_path(&self, id: usize) -> PathBuf {
        self.file_dir().join(format!("{id}.html"))
    }

    fn assets_dir(&self) -> PathBuf {
        PathBuf::from("assets")
    }

    fn asset_path(&self, name: &str) -> PathBuf {
        self.assets_dir().join(name)
    }

    fn style_path(&self) -> PathBuf {
        self.asset_path("style.css")
    }

    fn abs(&self, path: PathBuf) -> PathBuf {
        self.root.join(path)
    }

    fn create(&self, path: PathBuf) -> io::Result<File> {
        assert!(path.is_relative());
        log::info!("create: {}", path.display());
        let path = self.abs(path);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        File::create(path)
    }
}
