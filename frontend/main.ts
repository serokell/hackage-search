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

interface PathResultMap {
  [index: string]: Result;
}

type Result =
  {
    element: Element
    last_line_number: number | null;
  }

window.onload = function () {
  const result_template = <HTMLTemplateElement>document.getElementById("result-template");
  const results = document.getElementById("results");
  const search_field = <HTMLInputElement>document.getElementById("search-field");
  let result_map: PathResultMap = {}
  search_field.onkeydown =
  event => {
    if(event.key === "Enter") {
      results.innerHTML = "";
      const resource = "/rg/" + encodeURIComponent(search_field.value);
      fetch_and_process(resource, result_map, results);
    }
  }
}

async function fetch_and_process(resource: string, result_map: PathResultMap, results: Element) {
  const response = await fetch(resource);
  for await (const line of read_lines(response.body)) {
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

function process_rg_out(j: RgOut, result_map: PathResultMap, results: Element) {
  if (j.type === "begin") {
    const path = j.data.path.text;
    const result = instantiate_result_template(j.data.path.text);
    result_map[path] = { element: result, last_line_number: null };
    results.append(result);
  }
  if (j.type === "context") {
    const path = j.data.path.text;
    const line = document.createElement("code");
    line.textContent = j.data.lines.text;
    const line_of_code = instantiate_line_of_code_template(j.data.line_number, line);
    append_line_of_code(result_map, path, j.data.line_number, line_of_code);
  }
  if (j.type === "match") {
    const path = j.data.path.text;
    const line = document.createElement("code");
    highlight_match(j.data.lines.text, line, j.data.submatches);
    const line_of_code = instantiate_line_of_code_template(j.data.line_number, line);
    append_line_of_code(result_map, path, j.data.line_number, line_of_code);
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
