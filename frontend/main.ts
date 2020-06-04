"use strict";

const utf8_encoder = new TextEncoder();
const utf8_decoder = new TextDecoder("utf-8");

// ripgrep output record
type RgOut =
  | {
      type: "begin",
      data: { path: RgOutData }
    }
  | {
      type: "context",
      data: {
        path: RgOutData,
        lines: RgOutData,
        line_number: number,
        absolute_offset: number,
        submatches: RgOutMatch[]
      }
    }
  | {
      type: "match",
      data: {
        path: RgOutData,
        lines: RgOutData,
        line_number: number,
        absolute_offset: number,
        submatches: RgOutMatch[]
      }
    }
  | {
      type: "end",
      data: {
        path: RgOutData,
        stats: RgOutStats
      }
    }
  | {
      type: "summary",
      data: {
        elapsed_total: RgOutDuration,
        stats: RgOutStats
      }
    }

type RgOutMatch =
  {
    match: RgOutData,
    start: number,
    end: number
  }

type RgOutDuration =
  {
    human: string,
    nanos: number,
    secs: number
  }

type RgOutData =
  | { text: string }
  // | { bytes: string }

type RgOutStats =
  {
    elapsed: RgOutDuration,
    searches: number,
    searches_with_match: number,
    bytes_searched: number,
    bytes_printed: number,
    matched_lines: number,
    matches: number
  }

interface PathResultMap { [index: string]: Result; }

type PkgResult =
  {
    element: Element
    files: PathResultMap
  }

interface PkgResultMap { [index: string]: PkgResult; }

type Result =
  {
    element: Element
    last_line_number: number | null;
  }

window.onload = function () {
  const search_field = <HTMLInputElement>document.getElementById("search-field");
  search_field.onkeydown =
  event => {
    if(event.key === "Enter") {
      const result_map: PkgResultMap = {}
      const old_results = document.getElementById("results");
      const results = document.createElement("div");
      results.id = "results";
      old_results.parentNode.replaceChild(results, old_results);
      const resource = "/rg/" + encodeURIComponent(search_field.value);
      fetch_and_process(resource, result_map, results);
    }
  }
}

async function fetch_and_process(resource: string, result_map: PkgResultMap, results: Element) {
  const controller = new AbortController();
  const response = await fetch(resource, { signal: controller.signal });
  for await (const line of read_lines(response.body)) {
    if (!document.body.contains(results)) {
      console.log("Canceling query:", resource);
      controller.abort();
      break;
    }
    const j = <RgOut>JSON.parse(line);
    process_rg_out(j, result_map, results);
  }
}

async function* read_lines(stream: ReadableStream<Uint8Array>): AsyncIterable<string> {
  const reader = stream.getReader();
  let buf = new Uint8Array();
  while (true) {
    let chunk = await reader.read();
    if (chunk.done) break;
    buf = append_Uint8Array(buf, chunk.value);
    while (true) {
      const pivot = buf.indexOf(10);
      if (pivot === -1) break;
      const line = buf.slice(0, pivot);
      yield utf8_decoder.decode(line);
      buf = buf.slice(pivot+1);
    }
  }
  if (buf.length != 0) yield utf8_decoder.decode(buf);
}

function append_Uint8Array(a: Uint8Array, b: Uint8Array): Uint8Array {
  if (a.length === 0) return b;
  if (b.length === 0) return a;
  const iterable: Iterable<number> =
    function* () {
      yield* a;
      yield* b;
    }();
  return new Uint8Array(iterable);
}

function init_pkg(result_map: PkgResultMap, pkg_name: string, results: Element) {
  if (!result_map.hasOwnProperty(pkg_name)) {
    const pkg = instantiate_pkg_template(pkg_name);
    result_map[pkg_name] = { element: pkg, files: {} };
    results.append(pkg);
  }
}

function process_rg_out(j: RgOut, result_map: PkgResultMap, results: Element) {
  if (j.type === "begin") {
    const { pkg_name, path } = split_pkg_name(j.data.path.text);
    const result = instantiate_result_template(path);
    init_pkg(result_map, pkg_name, results);
    const pkg = result_map[pkg_name];
    pkg.files[path] = { element: result, last_line_number: null };
    pkg.element.append(result);
  }
  if (j.type === "context") {
    const { pkg_name, path } = split_pkg_name(j.data.path.text);
    const line = document.createElement("code");
    line.textContent = j.data.lines.text;
    const line_of_code = instantiate_line_of_code_template(j.data.line_number, line);
    append_line_of_code(result_map[pkg_name].files, path, j.data.line_number, line_of_code);
  }
  if (j.type === "match") {
    const { pkg_name, path } = split_pkg_name(j.data.path.text);
    const line = document.createElement("code");
    highlight_match(j.data.lines.text, line, j.data.submatches);
    const line_of_code = instantiate_line_of_code_template(j.data.line_number, line);
    append_line_of_code(result_map[pkg_name].files, path, j.data.line_number, line_of_code);
  }
}

type SplitPkgOut =
  {
    pkg_name: string,
    path: string
  }

function split_pkg_name(full_path: string): SplitPkgOut {
  const i = full_path.indexOf('/');
  return {
    pkg_name: full_path.slice(0, i),
    path: full_path.slice(i+1)
  }
}

function append_line_of_code(result_map: PathResultMap, path: string, line_number: number, line_of_code: Node) {
  const result = result_map[path];
  const result_code = result.element.querySelector(".result-code");
  const continuous = result.last_line_number === null || result.last_line_number === line_number - 1;
  if (!continuous) {
    const omission = clone_template("omission-template");
    result_map[path].element.querySelector(".result-code").append(omission);
  }
  result_code.append(line_of_code);
  result.last_line_number = line_number;
}

function instantiate_pkg_template(pkg_name: string): Element {
  const pkg = clone_template("package-template");
  pkg.querySelector("summary").textContent = pkg_name;
  return pkg;
}

function instantiate_result_template(header: string): Element {
  const result = clone_template("result-template");
  result.querySelector("h3").textContent = header;
  return result;
}

function instantiate_line_of_code_template(line_number: number, line: Node): Element {
  const line_of_code = clone_template("line-of-code-template");
  line_of_code.querySelector(".line-number").textContent = line_number.toString();
  line_of_code.querySelector(".line").append(line);
  return line_of_code;
}

function clone_template(id: string): Element {
  const template = <HTMLTemplateElement>document.getElementById(id);
  const instance = <DocumentFragment>template.content.cloneNode(true);
  return instance.firstElementChild;
}

function highlight_match(str: string, fmtstr: Element, submatches: RgOutMatch[]) {
  const rawstr = utf8_encoder.encode(str);
  let lastpos = 0;
  for (const sub of submatches) {
    fmtstr.append(utf8_decoder.decode(rawstr.slice(lastpos, sub.start)));
    let fmtspan = document.createElement("span");
    fmtspan.classList.add("match");
    fmtspan.textContent = utf8_decoder.decode(rawstr.slice(sub.start, sub.end));
    fmtstr.append(fmtspan);
    lastpos = sub.end;
  }
  fmtstr.append(utf8_decoder.decode(rawstr.slice(lastpos)));
}
