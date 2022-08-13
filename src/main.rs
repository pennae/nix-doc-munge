use std::{collections::VecDeque, env, fs, process::Command, sync::{Arc, Mutex}};

use anyhow::{Result, bail};
use regex::{RegexBuilder, Replacer};
use rnix::{
    types::{Apply, AttrSet, EntryHolder, Ident, TokenWrapper, TypedNode, Select, KeyValue},
    SyntaxKind, TextRange, SyntaxNode,
};
use tempfile::tempdir;
use threadpool::ThreadPool;

const THREADS: usize = 24;

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

fn find_candidates(s: &str) -> Vec<TextRange> {
    let ast = rnix::parse(s).as_result().unwrap();
    let mut nodes: VecDeque<_> = [(ast.node(), false)].into();
    let mut result = vec![];

    while let Some((node, parent_is_option)) = nodes.pop_front() {
        match node.kind() {
            SyntaxKind::NODE_APPLY => {
                let call = Apply::cast(node.clone()).unwrap();
                if let Some(arg) = call.value() {
                    nodes.push_back((arg, is_call_to(node.clone(), "mkOption")));
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
                        result.push(e.value().unwrap().text_range());
                    }
                }
            }
            _ => (),
        };

        for c in node.children() {
            nodes.push_back((c, false));
        }
    }

    result.sort_by(|a, b| b.start().cmp(&a.start()));
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

fn convert_one(s: &str, pos: TextRange) -> (String, String) {
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

    (
        prefix.to_owned() + "\"a\" + (" + chunk + ")" + suffix,
        prefix.to_owned()
            + "lib.mdDoc "
            + &new_chunk
            + suffix,
    )
}

fn build_manual(replace: Option<(&str, &str)>) -> Result<String> {
    let tmp = tempdir()?;
    let f = format!("{}/out", tmp.path().to_str().unwrap());
    let replace = match replace {
        Some((old, new)) if old != new => {
            format!(r#"disabledModules = [ ./{old} ]; imports = [ {new} ];"#)
        },
        _ => "".to_string(),
    };
    let result = Command::new("nix-build")
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
    Ok(fs::read_to_string(f)?)
}

fn convert_file(file: &str, live: bool) -> Result<String> {
    println!("  find change locations in {file}");
    let mut content = fs::read_to_string(file)?;
    let initial_content = content.clone();
    let candidates = find_candidates(&content);
    if candidates.is_empty() {
        return Ok(content);
    }

    let tmp = tempdir()?;
    let f = if live {
        file.to_string()
    } else {
        format!("{}/mod", tmp.path().to_str().unwrap())
    };

    println!("    build old {file}");
    fs::write(&f, initial_content.as_bytes())?;
    let old = build_manual(Some((file, &f)))?;

    for (i, range) in candidates.iter().enumerate() {
        let (test, change) = convert_one(&content, *range);
        println!("    build change for {i}/{} in {file}", candidates.len());
        fs::write(&f, change.as_bytes())?;
        if let Ok(changed) = build_manual(Some((file, &f))) {
            if old == changed {
                println!("    build test for {i}/{} in {file}", candidates.len());
                fs::write(&f, test.as_bytes())?;
                if let Ok(tested) = build_manual(Some((file, &f))) {
                    if old != tested {
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
    let (skip, live) = match env::args().skip(1).next() {
        Some(s) if s == "--live" => (2, true),
        _ => (1, false),
    };

    let pool = ThreadPool::new(if live { 1 } else { THREADS });
    let changes = Arc::new(Mutex::new(vec![]));

    for (i, file) in env::args().skip(skip).enumerate() {
        pool.execute({
            let changes = Arc::clone(&changes);
            move || {
                println!("check {file} ({i} of {})", env::args().count() - 1);
                let new = convert_file(&file, live).unwrap();
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
