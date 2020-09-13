"use strict";

const utf8_decoder = new TextDecoder("utf-8");

type SearchOut =
  | {
      type: "file"
      package: string
      path: string
      lines: SearchOutLine[]
    }
  | {
      type: "summary"
      matches: number
    }
  | {
      type: "error"
      message: string
    }

type SearchOutLine =
  {
    number: number
    parts: SearchOutLinePart[]
  }

type SearchOutLinePart =
  {
    context?: string
    match?: string
  }

interface PkgResultMap { [index: string]: Element }

type Results =
  {
    element: Element
    items: Element
    status: Element
  }

type PkgCount =
  {
    n: number
  }

window.onload = function () {
  const search_field = <HTMLInputElement>document.getElementById("search-field");
  search_field.onkeydown =
    event => {
      if(event.key === "Enter") {
        set_search_q(search_field.value);
        run_search(search_field.value);
      }
    };
  const search_q = get_search_q();
  if (search_q != null) {
    search_field.value = search_q;
    run_search(search_q);
  }
}

function run_search(q: string) {
  const result_map: PkgResultMap = {}
  const old_results = document.getElementById("results")!;
  const results = instantiate_results_template();
  old_results.parentNode!.replaceChild(results.element, old_results);
  const resource = rg_endpoint(q);
  fetch_and_process(resource, result_map, results).catch(
    error => {
      const err = instantiate_error_template(error.toString());
      results.items.append(err);
      results.status.textContent = "Search failed";
    });
}

function rg_endpoint(q: string): string {
  const current_path = window.location.pathname;
  const rg_suffix = current_path.substr(-1) == '/' ? 'rg/' : '/rg/';
  return current_path + rg_suffix + encodeURIComponent(q);
}

function get_search_q(): (string | null) {
  return new URL(window.location.href).searchParams.get("q");
}

function set_search_q(q: string) {
  const new_url = new URL(window.location.href);
  new_url.searchParams.set("q", q);
  history.replaceState(null, "", new_url.toString());
}

async function fetch_and_process(resource: string, result_map: PkgResultMap, results: Results) {
  const controller = new AbortController();
  const response = await fetch(resource, { signal: controller.signal });
  check_response_status(response);
  const pkg_count = { n: 0 }
  for await (const line of read_lines(response.body!)) {
    if (!document.body.contains(results.element)) {
      console.log("Canceling query:", resource);
      controller.abort();
      break;
    }
    const j = <SearchOut>JSON.parse(line);
    await process_search_out(j, pkg_count, result_map, results);
  }
}

function check_response_status(response: Response): void {
  if (!response.ok) {
    throw response.status.toString() + ': ' + response.statusText;
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

function init_pkg(pkg_count: PkgCount, result_map: PkgResultMap, pkg_name: string, results: Results) {
  if (!result_map.hasOwnProperty(pkg_name)) {
    const pkg = instantiate_pkg_template(pkg_name);
    result_map[pkg_name] = pkg;
    results.items.append(pkg);
    if (pkg_count.n === 0) {
      pkg.open = true;
      pkg.querySelector("summary")!.focus();
    };
    pkg_count.n++;
  }
}

function process_search_out(j: SearchOut, pkg_count: PkgCount, result_map: PkgResultMap, results: Results): Promise<void> {
  if (j.type === "file") {
    init_pkg(pkg_count, result_map, j.package, results);
    const result = instantiate_result_template(j.path);
    const result_code = result.querySelector(".result-code")!;
    let last_line_number = null;
    for (const result_line of j.lines) {
      append_line_of_code(result_line, result_code, last_line_number);
      last_line_number = result_line.number;
    }
    result_map[j.package].append(result);
  }
  if (j.type === "error") {
    const err = instantiate_error_template(j.message);
    results.items.append(err);
    results.status.textContent = "Search failed";
  }
  if (j.type === "summary") {
    results.status.textContent =
      "Search completed ("
        + summary_message(j.matches, pkg_count.n)
        + ")";
  }
  return new Promise(r => setTimeout(r, 10)); // See Note [setTimeout in process_search_out]
}

function process_line_parts(line_parts: SearchOutLinePart[]): Element {
  const fmt_line = document.createElement("code");
  for (const line_part of line_parts) {
    const context = line_part.context ?? null;
    if (context != null) {
      fmt_line.append(context);
    }
    const match = line_part.match ?? null;
    if (match != null) {
      let fmt_span = document.createElement("span");
      fmt_span.classList.add("match");
      fmt_span.textContent = match;
      fmt_line.append(fmt_span);
    }
  }
  return fmt_line;
}

function summary_message(matches: number, pkg_count: number): string {
  // e.g. "15 matches across 10 packages", "2 matches in 1 package"
  return (
    pluralise(matches, "match", "matches")
      + (pkg_count === 1 ? " in " : " across ")
      + pluralise(pkg_count, "package", "packages")
    );
}

function pluralise(n: number, singular: string, plural: string): string {
  return n.toString() + ' ' + (n === 1 ? singular : plural);
}

/* Note [setTimeout in process_search_out]
------------------------------------------

In JavaScript, data processing and UI rendering happens in the same thread
(without WebWorkers). So if the server responds with lots of data, we must
take breaks from processing to let the browser handle UI events and render the
page.

A timeout for 10ms after every search result seems to yield best UX.

*/

type Result =
  {
    element: Element
    last_line_number: number | null;
  }

function append_line_of_code(result_line: SearchOutLine, result_code: Element, last_line_number: number | null) {
  const line = process_line_parts(result_line.parts);
  const line_of_code = instantiate_line_of_code_template(result_line.number, line);
  const continuous = last_line_number === null || last_line_number === result_line.number - 1;
  if (!continuous) {
    const omission = clone_template("omission-template");
    result_code.append(omission);
  }
  result_code.append(line_of_code);
}

function instantiate_error_template(message: string): Element {
  const err = clone_template("error-template");
  err.textContent = message;
  return err;
}

function instantiate_results_template(): Results {
  const results_element = clone_template("results-template");
  const results =
    {
      element: results_element,
      items: results_element.querySelector(".results-items")!,
      status: results_element.querySelector(".results-status")!
    };
  results.element.id = "results";
  return results;
}

function instantiate_pkg_template(pkg_name: string): HTMLDetailsElement {
  const button_expand_all = document.getElementById("button-expand-all")!;
  const button_collapse_all = document.getElementById("button-collapse-all")!;
  const pkg = <HTMLDetailsElement>clone_template("package-template");
  pkg.querySelector("summary")!.textContent = pkg_name;
  pkg.addEventListener("toggle", event => {
    if (pkg.open) {
      pkg.scrollIntoView({behavior: "smooth", block: "start"});
    }
    else {
      pkg.scrollIntoView({behavior: "auto", block: "center"});
    }
  })
  button_expand_all.addEventListener("click", event => {
    pkg.open = true;
  })
  button_collapse_all.addEventListener("click", event => {
    pkg.open = false;
  })
  return pkg;
}

function instantiate_result_template(header: string): Element {
  const result = clone_template("result-template");
  result.querySelector("h3")!.textContent = header;
  return result;
}

function instantiate_line_of_code_template(line_number: number, line: Node): Element {
  const line_of_code = clone_template("line-of-code-template");
  line_of_code.querySelector(".line-number")!.textContent = line_number.toString();
  line_of_code.querySelector(".line")!.append(line);
  return line_of_code;
}

function clone_template(id: string): Element {
  const template = <HTMLTemplateElement>document.getElementById(id);
  const instance = <DocumentFragment>template.content.cloneNode(true);
  return instance.firstElementChild!;
}
