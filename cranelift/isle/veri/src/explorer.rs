use cranelift_isle::{
    lexer::Pos,
    sema::{RuleId, TermId, TypeId},
    trie_again::BindingId,
};

use crate::{
    debug::{binding_string, constraint_string},
    expand::Expansion,
    program::Program,
    trie_again::{binding_type, BindingType},
};
use std::{
    fs::File,
    io::{self, Write},
    path::PathBuf,
};

pub struct ExplorerWriter<'a> {
    prog: &'a Program,
    expansions: &'a Vec<Expansion>,

    root: std::path::PathBuf,
    dev: bool,
}

impl<'a> ExplorerWriter<'a> {
    pub fn new(
        root: std::path::PathBuf,
        prog: &'a Program,
        expansions: &'a Vec<Expansion>,
    ) -> Self {
        Self {
            prog,
            expansions,
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
        self.write_types()?;
        self.write_terms()?;
        self.write_rules()?;
        self.write_expansions()?;
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
        output.write_all(style_css)?;

        Ok(())
    }

    fn write_index(&self) -> anyhow::Result<()> {
        let mut output = self.create(PathBuf::from("index.html"))?;
        self.header("ISLE Explorer", &mut output)?;
        writeln!(
            output,
            r#"
        <menu>
            <li><a href="/{files_href}">Files</a></li>
            <li><a href="/{types_href}">Types</a></li>
            <li><a href="/{terms_href}">Terms</a></li>
            <li><a href="/{rules_href}">Rules</a></li>
            <li><a href="/{expansions_href}">Expansions</a></li>
        </menu>
        "#,
            files_href = self.file_dir().display(),
            types_href = self.type_dir().display(),
            terms_href = self.term_dir().display(),
            rules_href = self.rule_dir().display(),
            expansions_href = self.expansion_dir().display(),
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
                r#"<code id="{fragment}">{line}</code>"#,
                fragment = self.line_url_fragment(n)
            )?;
        }
        writeln!(&mut output, "</pre>")?;

        // Footer.
        self.footer(&mut output)?;

        Ok(())
    }

    fn write_types(&self) -> anyhow::Result<()> {
        let mut output = self.create(self.type_dir().join("index.html"))?;
        self.header("Types", &mut output)?;

        // Types.
        writeln!(
            output,
            r#"
        <table>
            <thead>
                <tr>
                    <th class="id">&num;</th>
                    <th>Name</th>
                    <th>Location</th>
                    <th>Model</th>
                </tr>
            </thead>
            <tbody>
        "#
        )?;
        for ty in &self.prog.tyenv.types {
            writeln!(output, "<tr>")?;
            writeln!(output, r#"<td class="id">{id}</td>"#, id = ty.id().index())?;

            // Name.
            writeln!(output, r"<td>{name}</td>", name = ty.name(&self.prog.tyenv))?;

            // Location.
            writeln!(output, "<td>{pos}</td>", pos = self.pos(ty.pos()))?;

            // Model.
            if let Some(model) = self.prog.specenv.type_model.get(&ty.id()) {
                // TODO(mbm): link type model to source position
                writeln!(output, "<td>{model}</td>")?;
            } else {
                writeln!(output, "<td></td>")?;
            }

            writeln!(output, "</tr>")?;
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
                    <th class="id">&num;</th>
                    <th>Name</th>
                    <th>Location</th>
                    <th>Spec</th>
                </tr>
            </thead>
            <tbody>
        "#
        )?;
        for term in &self.prog.termenv.terms {
            writeln!(output, "<tr>")?;
            writeln!(output, r#"<td class="id">{id}</td>"#, id = term.id.index())?;

            // Name.
            writeln!(
                output,
                r"<td>{name}</td>",
                name = self.prog.term_name(term.id)
            )?;

            // Location.
            writeln!(output, "<td>{pos}</td>", pos = self.pos(term.decl_pos))?;

            // Spec.
            if let Some(spec) = self.prog.specenv.term_spec.get(&term.id) {
                writeln!(output, "<td>{pos}</td>", pos = self.pos(spec.pos))?;
            } else {
                writeln!(output, "<td></td>")?;
            }

            writeln!(output, "</tr>")?;
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
        let rule_ids = (0..self.prog.termenv.rules.len()).map(RuleId);
        self.write_rules_list(&mut output, rule_ids)?;

        self.footer(&mut output)?;
        Ok(())
    }

    fn write_rules_list(
        &self,
        dst: &mut dyn Write,
        rule_ids: impl Iterator<Item = RuleId>,
    ) -> anyhow::Result<()> {
        writeln!(
            dst,
            r#"
        <table>
            <thead>
                <tr>
                    <th class="id">&num;</th>
                    <th>Identifier</th>
                </tr>
            </thead>
            <tbody>
        "#
        )?;

        for rule_id in rule_ids {
            writeln!(dst, "<tr>")?;
            writeln!(dst, r#"<td class="id">{id}</td>"#, id = rule_id.index())?;
            writeln!(
                dst,
                "<td>{rule_ref}</td>",
                rule_ref = self.rule_ref(rule_id)
            )?;
            writeln!(dst, "</tr>")?;
        }

        writeln!(
            dst,
            r#"
            </tbody>
        </table>
        "#
        )?;
        Ok(())
    }

    fn write_expansions(&self) -> anyhow::Result<()> {
        self.write_expansions_index()?;
        for (id, expansion) in self.expansions.iter().enumerate() {
            self.write_expansion(id, expansion)?;
        }
        Ok(())
    }

    fn write_expansions_index(&self) -> anyhow::Result<()> {
        let mut output = self.create(self.expansion_dir().join("index.html"))?;
        self.header("Expansions", &mut output)?;

        // Expansions.
        writeln!(
            output,
            r#"
        <table>
            <thead>
                <tr>
                    <th>&num;</th>
                    <th>First Rule</th>
                </tr>
            </thead>
            <tbody>
        "#
        )?;
        for (id, expansion) in self.expansions.iter().enumerate() {
            writeln!(output, "<tr>")?;

            // ID
            writeln!(
                output,
                r#"<td><a href="/{link}">&num;{id}</a></td>"#,
                link = self.expansion_path(id).display()
            )?;

            // First Rule
            let rule_id = expansion
                .rules
                .first()
                .expect("expansion must have at least one rule");
            writeln!(
                output,
                "<td>{rule_ref}</td>",
                rule_ref = self.rule_ref(*rule_id)
            )?;

            writeln!(output, "</tr>")?;
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

    fn write_expansion(&self, id: usize, expansion: &Expansion) -> anyhow::Result<()> {
        let mut output = self.create(self.expansion_path(id))?;

        // Header.
        let title = format!("Expansion: &num;{id}");
        self.header(&title, &mut output)?;

        // Term.
        writeln!(
            output,
            "<p>Term: {term_ref}</p>",
            term_ref = self.term_ref(expansion.term)
        )?;

        // Rules
        writeln!(output, "<h2>Rules</h2>")?;
        self.write_rules_list(&mut output, expansion.rules.iter().copied())?;

        // Bindings
        writeln!(output, "<h2>Bindings</h2>")?;
        writeln!(
            output,
            r#"
        <table>
            <thead>
                <tr>
                    <th>&num;</th>
        "#
        )?;
        if !expansion.equals.is_empty() {
            writeln!(output, "<th>&equals;</th>")?;
        }
        writeln!(
            output,
            r#"
                    <th>Type</th>
                    <th>Binding</th>
                    <th>Constraints</th>
                </tr>
            </thead>
            <tbody>
        "#
        )?;
        let lookup_binding =
            |binding_id: BindingId| expansion.bindings[binding_id.index()].clone().unwrap();
        for (i, binding) in expansion.bindings.iter().enumerate() {
            let id: BindingId = i.try_into().unwrap();
            if let Some(binding) = binding {
                writeln!(output, "<tr>")?;
                let ty = binding_type(binding, expansion.term, &self.prog, lookup_binding);

                // ID
                writeln!(output, "<td>{id}</td>", id = id.index())?;

                // Equals
                if let Some(eq) = expansion.equals.find(id) {
                    if id != eq {
                        write!(output, "<td>&equals; {}</td>", eq.index())?;
                    }
                }

                // Type
                writeln!(output, "<td>{ty}</td>", ty = self.binding_type(&ty))?;

                // Binding
                writeln!(
                    output,
                    "<td>{binding}</td>",
                    binding = binding_string(binding, expansion.term, &self.prog, lookup_binding)
                )?;

                // Constraints
                if let Some(constraints) = expansion.constraints.get(&id) {
                    writeln!(
                        output,
                        "<td>{}</td>",
                        constraints
                            .iter()
                            .map(|c| constraint_string(c, &self.prog.tyenv))
                            .collect::<Vec<_>>()
                            .join(" ")
                    )?;
                } else {
                    writeln!(output, "<td></td>")?;
                }

                writeln!(output, "</tr>")?;
            }
        }

        // TODO(mbm): Terms
        // TODO(mbm): Parameters
        // TODO(mbm): Result

        // Footer.
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

    fn binding_type(&self, ty: &BindingType) -> String {
        match ty {
            BindingType::Base(type_id) => self.type_ref(*type_id),
            BindingType::Option(inner) => format!("Option({})", self.binding_type(inner)),
            BindingType::Tuple(inners) => format!(
                "({inners})",
                inners = inners
                    .iter()
                    .map(|inner| self.binding_type(inner))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }

    fn type_ref(&self, type_id: TypeId) -> String {
        let ty = self.prog.ty(type_id);
        format!(
            r#"<a href="{href}">{name}</a>"#,
            href = self.pos_href(ty.pos()),
            name = self.prog.type_name(ty.id())
        )
    }

    fn term_ref(&self, term_id: TermId) -> String {
        let term = self.prog.term(term_id);
        format!(
            r#"<a href="{href}">{name}</a>"#,
            href = self.pos_href(term.decl_pos),
            name = self.prog.term_name(term_id)
        )
    }

    fn rule_ref(&self, rule_id: RuleId) -> String {
        let rule = self.prog.rule(rule_id);
        format!(
            r#"<a href="{href}">{identifier}</a>"#,
            href = self.pos_href(rule.pos),
            identifier = rule.identifier(&self.prog.tyenv)
        )
    }

    fn pos(&self, pos: Pos) -> String {
        if pos.is_unknown() {
            "&lt;unknown&gt;".to_string()
        } else {
            format!(
                r#"<a href="{href}">{loc}</a>"#,
                href = self.pos_href(pos),
                loc = self.loc(pos)
            )
        }
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

    fn type_dir(&self) -> PathBuf {
        PathBuf::from("type")
    }

    fn term_dir(&self) -> PathBuf {
        PathBuf::from("term")
    }

    fn rule_dir(&self) -> PathBuf {
        PathBuf::from("rule")
    }

    fn expansion_dir(&self) -> PathBuf {
        PathBuf::from("expansion")
    }

    fn expansion_path(&self, id: usize) -> PathBuf {
        self.expansion_dir().join(format!("{id}.html"))
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
