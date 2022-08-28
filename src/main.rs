use std::{collections::VecDeque, env, fs, process::Command, sync::{Arc, Mutex}, path::Path};

use anyhow::{Result, bail};
use regex::{RegexBuilder, Replacer};
use rnix::{
    types::{Apply, AttrSet, EntryHolder, Ident, TokenWrapper, TypedNode, Select, KeyValue, Paren},
    SyntaxKind, TextRange, SyntaxNode,
};
use tempfile::tempdir;
use threadpool::ThreadPool;

struct StatusReportData {
    files: usize,
    items: usize,
    total_files: usize,
    total_items: usize,
    changed_items: usize,
    last_file: String,
    last_item: String,
}

impl StatusReportData {
    fn print(&self, clear: bool) {
        if clear {
            print!("\x1b[1F\x1b[2K\x1b[1F\x1b[2K");
        }
        println!("{}/{} files ({})", self.files, self.total_files, self.last_file);
        println!("{}/{} ({}) items ({})", self.items, self.total_items,
                 self.changed_items, self.last_item);
    }
}

struct StatusReport(Mutex<StatusReportData>);

impl StatusReport {
    fn new(total_files: usize, total_items: usize) -> Self {
        Self(Mutex::new(StatusReportData {
            files: 0,
            items: 0,
            total_files,
            total_items,
            changed_items: 0,
            last_file: "".to_string(),
            last_item: "".to_string(),
        }))
    }

    fn enter_file(&self, f: &str) {
        let mut m = self.0.lock().unwrap();
        m.files += 1;
        m.last_file = f.to_string();
        m.print(m.files > 1 || m.items >= 1);
    }

    fn enter_item(&self, i: String) {
        let mut m = self.0.lock().unwrap();
        m.items += 1;
        m.last_item = i;
        m.print(m.files >= 1 || m.items > 1);
    }

    fn update_item(&self, i: String) {
        let mut m = self.0.lock().unwrap();
        m.last_item = i;
        m.print(true);
    }

    fn changed_item(&self) {
        let mut m = self.0.lock().unwrap();
        m.changed_items += 1;
        m.print(true);
    }

    fn skip_items(&self, i: usize) {
        let mut m = self.0.lock().unwrap();
        m.items += i;
        m.print(m.files >= 1 || m.items >= 1);
    }
}

struct StatusPart<'a>(&'a StatusReport, usize);

impl<'a> StatusPart<'a> {
    fn enter_item(&mut self, i: String) {
        self.0.enter_item(i);
        self.1 -= 1;
    }

    fn update_item(&mut self, i: String) {
        self.0.update_item(i);
    }

    fn changed_item(&mut self) {
        self.0.changed_item();
    }
}

impl<'a> Drop for StatusPart<'a> {
    fn drop(&mut self) {
        self.0.skip_items(self.1);
    }
}

fn is_call_to(n: SyntaxNode, f: &str) -> bool {
    let tgt = match Apply::cast(n) {
        Some(tgt) => tgt,
        _ => return false,
    };
    if let Some(id) = tgt.lambda().and_then(Ident::cast) {
        return id.as_str() == f;
    }
    if let Some(sel) = tgt.lambda().and_then(Select::cast) {
        return match (sel.set().and_then(Ident::cast), sel.index().and_then(Ident::cast)) {
            (Some(s), Some(i)) => s.as_str() == "lib" && i.as_str() == f,
            _ => false,
        };
    }
    false
}

// doesn't need to escape . because we're only interested in single-entry
// paths anyway
fn key_string(kv: &KeyValue) -> String {
    kv.key().map_or_else(
        || String::new(),
        |kv| kv.path().map(|p| p.to_string()).collect::<Vec<_>>().join("."))
}

fn is_visible(attrs: &AttrSet) -> bool {
    // there's no reason to set these keys if not to hide an item. we'll want
    // to ignore hidden items because they don't show up in the docs, processing
    // them only takes time for no changes.
    attrs.entries().map(|e| key_string(&e)).all(|k| {
        k != "internal" && k != "visible"
    })
}

fn find_candidates(s: &str) -> Vec<(TextRange, bool)> {
    let ast = rnix::parse(s).as_result().unwrap();
    let mut nodes: VecDeque<_> = [(ast.node(), false)].into();
    let mut result = vec![];

    while let Some((node, parent_is_option)) = nodes.pop_front() {
        match node.kind() {
            SyntaxKind::NODE_APPLY => {
                let call = Apply::cast(node.clone()).unwrap();
                if let Some(arg) = call.value() {
                    nodes.push_back((arg.clone(), is_call_to(node.clone(), "mkOption")));
                    if is_call_to(node.clone(), "mkEnableOption")
                        && Paren::cast(call.value().unwrap()).map_or(true, |p| {
                            !is_call_to(p.node().first_child().unwrap(), "mdDoc")
                        })
                    {
                        result.push((arg.text_range(), true));
                    }
                    continue;
                }
            }
            SyntaxKind::NODE_ATTR_SET => {
                let attrs = AttrSet::cast(node.clone()).unwrap();
                for e in attrs.entries() {
                    if key_string(&e) == "description"
                        && parent_is_option
                        && !e.value().map(|v| is_call_to(v, "mdDoc")).unwrap_or(false)
                        && is_visible(&attrs)
                    {
                        result.push((e.value().unwrap().text_range(), false));
                    }
                }
            }
            _ => (),
        };

        for c in node.children() {
            nodes.push_back((c, false));
        }
    }

    result.sort_by(|(a, _), (b, _)| b.start().cmp(&a.start()));
    result
}

fn markdown_escape(s: &str) -> String {
    s.replace("`", "\\`")
     .replace("*", "\\*")
     .replace("&lt;", "<")
     .replace("&gt;", ">")
}

struct SurroundPat(&'static str, &'static str, &'static str);

impl Replacer for SurroundPat {
    fn replace_append(&mut self, caps: &regex::Captures<'_>, dst: &mut String) {
        dst.push_str(self.0);
        let mut tmp = String::new();
        self.1.replace_append(caps, &mut tmp);
        dst.push_str(&markdown_escape(&tmp));
        dst.push_str(self.2);
    }
}

struct CodePat(&'static str);

impl Replacer for CodePat {
    fn replace_append(&mut self, caps: &regex::Captures<'_>, dst: &mut String) {
        dst.push_str(self.0);
        dst.push_str("`");
        dst.push_str(&caps[1].replace("^gt;", ">").replace("&lt;", "<"));
        dst.push_str("`");
    }
}

fn convert_one(s: &str, pos: TextRange, add_parens: bool) -> (String, String) {
    let prefix = &s[.. pos.start().into()];
    let chunk = &s[pos.start().into() .. pos.end().into()];
    let suffix = &s[usize::from(pos.end()) ..];

    let new_chunk = RegexBuilder::new(r#"<literal>([^`]*?)</literal>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .build().unwrap()
        .replace_all(&chunk, CodePat(""));
    // let new_chunk = RegexBuilder::new(r#"<replaceable>([^»]*?)</replaceable>"#)
    //     .multi_line(true)
    //     .dot_matches_new_line(true)
    //     .build().unwrap()
    //     .replace_all(&new_chunk, SurroundPat("«", "$1", "»"));
    let new_chunk = RegexBuilder::new(r#"<filename>([^`]*?)</filename>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .build().unwrap()
        .replace_all(&new_chunk, CodePat("{file}"));
    let new_chunk = RegexBuilder::new(r#"<option>([^`]*?)</option>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .build().unwrap()
        .replace_all(&new_chunk, CodePat("{option}"));
    // let new_chunk = RegexBuilder::new(r#"<code>([^`]*?)</code>"#)
    //     .multi_line(true)
    //     .dot_matches_new_line(true)
    //     .build().unwrap()
    //     .replace_all(&new_chunk, SurroundPat("`", "$1", "`"));
    let new_chunk = RegexBuilder::new(r#"<command>([^`]*?)</command>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .build().unwrap()
        .replace_all(&new_chunk, CodePat("{command}"));
    let new_chunk = RegexBuilder::new(r#"<link xlink:href="(.+?)" ?/>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .build().unwrap()
        .replace_all(&new_chunk, SurroundPat("<", "$1", ">"));
    let new_chunk = RegexBuilder::new(r#"<link xlink:href="(.+?)">(.*?)</link>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .build().unwrap()
        .replace_all(&new_chunk, SurroundPat("", "[$2]($1)", ""));
    let new_chunk = RegexBuilder::new(r#"<xref linkend="(.+?)" ?/>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .build().unwrap()
        .replace_all(&new_chunk, SurroundPat("[](#", "$1", ")"));
    let new_chunk = RegexBuilder::new(r#"<link linkend="(.+?)">(.*?)</link>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .build().unwrap()
        .replace_all(&new_chunk, SurroundPat("", "[$2](#$1)", ""));
    // let new_chunk = RegexBuilder::new(r#"<package>([^`]*?)</package>"#)
    //     .multi_line(true)
    //     .dot_matches_new_line(true)
    //     .build().unwrap()
    //     .replace_all(&new_chunk, SurroundPat("`", "$1", "`"));
    let new_chunk = RegexBuilder::new(r#"<emphasis>([^*]*?)</emphasis>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .build().unwrap()
        .replace_all(&new_chunk, SurroundPat("*", "$1", "*"));
    let new_chunk = RegexBuilder::new(r#"<emphasis role="string">([^*]*?)</emphasis>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .build().unwrap()
        .replace_all(&new_chunk, SurroundPat("**", "$1", "**"));
    let new_chunk = RegexBuilder::new(r#"
            <citerefentry>\s*
                <refentrytitle>\s*(.*?)\s*</refentrytitle>\s*
                <manvolnum>\s*(.*?)\s*</manvolnum>\s*
            </citerefentry>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .ignore_whitespace(true)
        .build().unwrap()
        .replace_all(&new_chunk, "{manpage}`$1($2)`");
    let new_chunk = RegexBuilder::new(r#"</?programlisting>"#)
        .multi_line(true)
        .dot_matches_new_line(true)
        .build().unwrap()
        .replace_all(&new_chunk, "```");

    let (lpar, rpar) = if add_parens {
        ("(", ")")
    } else {
        ("", "")
    };

    (
        prefix.to_owned() + lpar + "\"a\" + (" + chunk + ")" + rpar + suffix,
        prefix.to_owned()
            + lpar
            + "lib.mdDoc "
            + &new_chunk
            + rpar
            + suffix,
    )
}

fn build_manual(dir: impl AsRef<Path>, import: Option<&str>) -> Result<String> {
    let tmp = tempdir()?;
    let f = format!("{}/out", tmp.path().to_str().unwrap());
    let replace = match import {
        Some(new) => format!(r#"imports = [ {new} ];"#),
        _ => "".to_string(),
    };
    let result = Command::new("nix-build")
        .current_dir(dir)
        .args(["-o", &f, "-E"])
        .arg(format!(r#"let sys = import ./nixos/default.nix {{
                            configuration = {{
                                # include the overridden module!
                                documentation.nixos.includeAllModules = true;
                                documentation.nixos.options.warningsAreErrors = false;
                                {replace}
                            }};
                        }};
                        in sys.config.system.build.manual.optionsDocBook"#))
        .output()?;
    if !result.status.success() {
        bail!("build failed: {}", String::from_utf8_lossy(&result.stderr));
    }
    // Ok(fs::read_to_string(format!("{f}/share/doc/nixos/options.json"))?)
    Ok(fs::read_to_string(f)?)
}

fn convert_file(file: &str, import: bool, p: &StatusReport) -> Result<String> {
    let mut content = fs::read_to_string(file)?;
    let initial_content = content.clone();
    let candidates = find_candidates(&content);
    let mut p = StatusPart(p, candidates.len());
    if candidates.is_empty() {
        return Ok(content);
    }

    let tmp = tempdir()?;
    let result = Command::new("cp")
        .args(["-at", tmp.path().to_str().unwrap(), "--reflink=always", "."])
        .output()?;
    if !result.status.success() {
        bail!("copy failed: {}", String::from_utf8_lossy(&result.stderr));
    }

    let f = format!("{}/{file}", tmp.path().to_str().unwrap());
    let import = match import {
        true => Some(f.as_str()),
        false => None,
    };

    p.update_item(format!("old in {file}"));
    fs::write(&f, initial_content.as_bytes())?;
    let old = build_manual(&tmp, import)?;

    for (i, &(range, add_parens)) in candidates.iter().enumerate() {
        let (test, change) = convert_one(&content, range, add_parens);
        p.enter_item(format!("check {}/{} in {file}", i + 1, candidates.len()));
        fs::write(&f, change.as_bytes())?;
        if let Ok(changed) = build_manual(&tmp, import) {
            if old == changed {
                p.update_item(format!("test {}/{} in {file}", i + 1, candidates.len()));
                fs::write(&f, test.as_bytes())?;
                if let Ok(tested) = build_manual(&tmp, import) {
                    if old != tested {
                        p.changed_item();
                        content = change;
                    }
                }
            }
        }
    }

    fs::write(&f, initial_content.as_bytes())?;
    Ok(content)
}

fn main() -> Result<()> {
    let (skip, import) = match env::args().skip(1).next() {
        Some(s) if s == "--import" => (2, true),
        _ => (1, false),
    };

    let pool = ThreadPool::new(16);
    let changes = Arc::new(Mutex::new(vec![]));

    let total_items = env::args().skip(skip).map(|file| {
        let content = fs::read_to_string(file)?;
        let candidates = find_candidates(&content);
        Ok(candidates.len())
    }).sum::<Result<usize>>()?;

    let printer = Arc::new(StatusReport::new(env::args().count() - skip, total_items));

    for file in env::args().skip(skip) {
        pool.execute({
            let (changes, printer) = (Arc::clone(&changes), Arc::clone(&printer));
            move || {
                printer.enter_file(&file);
                let new = convert_file(&file, import, &printer).unwrap();
                changes.lock().unwrap().push((file, new));
            }
        });
    }
    pool.join();

    for (file, content) in changes.lock().unwrap().iter() {
        fs::write(&file, content.as_bytes())?;
    }

    Ok(())
}
