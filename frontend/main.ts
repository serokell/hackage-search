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

window.onload = function () {
  const result_template = <HTMLTemplateElement>document.getElementById("result-template");
  const results = document.getElementById("results");
  const search_field = <HTMLInputElement>document.getElementById("search-field");
  search_field.onkeydown =
  event => {
    if(event.key === "Enter") {
      results.innerHTML = "";
      const resource = "/rg/" + encodeURIComponent(search_field.value);
      fetch_and_process(resource, results);
    }
  }
}

async function fetch_and_process(resource: string, results: Element) {
  const response = await fetch(resource);
  for await (const line of read_lines(response.body)) {
    const j = <RgOut>JSON.parse(line);
    process_rg_out(j, results);
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
  const iterable: Iterable<number> =
    function* () {
      yield* a;
      yield* b;
    }();
  return new Uint8Array(iterable);
}

function process_rg_out(j: RgOut, results: Element) {
  const result_template = <HTMLTemplateElement>document.getElementById("result-template");
  const result = <Element>result_template.content.cloneNode(true);
  if (j.type === "match") {
    result.querySelector("h3").textContent = j.data.path.text;
    let fmtstr = result.querySelector("code");
    highlight_match(j.data.lines.text, fmtstr, j.data.submatches);
    results.appendChild(result);
  }
}

function highlight_match(str: string, fmtstr: Element, submatches: RgOutMatch[]) {
  const rawstr = utf8_encoder.encode(str);
  let lastpos = 0;
  for (const sub of submatches) {
    fmtstr.appendChild(
      document.createTextNode(utf8_decoder.decode(rawstr.slice(lastpos, sub.start)))
    );
    let fmtspan = document.createElement("span");
    fmtspan.classList.add("match");
    fmtspan.textContent = utf8_decoder.decode(rawstr.slice(sub.start, sub.end));
    fmtstr.appendChild(fmtspan);
    lastpos = sub.end;
  }
  fmtstr.appendChild(
    document.createTextNode(utf8_decoder.decode(rawstr.slice(lastpos)))
  );
}
